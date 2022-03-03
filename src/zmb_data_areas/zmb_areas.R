#' ## Zambia (ZMB)
#' Source: Estimates team (?) edited by UNAIDS
#' Levels:
#'   * 1: Province (10)
#'   * 2: District (116)
#' Spectrum: National
#' EPP: Province (level 1)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: District (level 2)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

urls <- list(zmb = "ZMB/2020-02-19/Zambia116.zip",
             dist = "ZMB/2020-10-30/2019%20Zambia%20Locations.xlsx",
             zmb_pepfar = "ZMB/2019-07-10/Zambia_PROD_5_District_DistrictLsib_2019_Jan.zip"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)


zmb <- read_sf_zip(files$zmb)
dist <- readxl::read_excel(files$dist) %>%
  select(country = 1, province = 2, district = 3)

zmb_pepfar <- read_sf_zip(files$zmb_pepfar)

#' Mwandi district is duplicated

p1 <- zmb_pepfar %>%
  filter(PROVINCE == "Western") %>%
  ggplot(aes(label = NAME)) +
  geom_sf() +
  geom_sf_label() +
  ggtitle("PEPFAR Jan 2019 (105 districts)")

p2 <- zmb %>%
  filter(PROVINCE == "Western") %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = NAME)) +
  ggtitle("116 district file: original")

zmb <- zmb %>%
  mutate(idx = row_number())

zmb %>%
  filter(PROVINCE == "Western") %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = idx))

zmb <- zmb %>%
  mutate(NAME = if_else(idx == 95, "Sesheke", as.character(NAME)))

p3 <- zmb %>%
  filter(PROVINCE == "Western") %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = NAME)) +
  ggtitle("116 district file: renamed district")

p <- gridExtra::arrangeGrob(p1, p2, p3, nrow = 1)

ggsave("check/check-zmb-rename-duplicated-mwandi-district.png", p, h = 5, w = 15)


#' Simplify and clean boundaries

zmb_simple <- zmb %>%
  rmapshaper::ms_simplify(0.05)

zmb_simple %>% pryr::object_size()


zmb_simple %>% summarise() %>% ggplot() + geom_sf()

zmb_clean <- zmb_simple %>%
  st_transform(3857) %>%
  sf::st_snap(., ., tolerance = 50) %>%
  rmapshaper::ms_simplify(1.0) %>%
  st_transform(4326)

zmb_clean %>%
  summarise() %>%
  ggplot() +
  geom_sf(fill = NA)

zmb_clean %>%
  ggplot() +
  geom_sf(fill = NA)

#' Make valid geometry

filter(zmb_clean) %>%
  filter(!st_is_valid(.))

zmb_clean <- sf::st_make_valid(zmb_clean)

#' Get rid of fragments created by st_make_valid()
zmb_clean <- st_collection_extract(zmb_clean, "POLYGON")



#' # Spectrum region codes

spectrum_region_code  <- c("Central" = 10,
                           "Copperbelt" = 11,
                           "Eastern" = 12,
                           "Luapula" = 13,
                           "Lusaka" = 14,
                           "Muchinga" = 15,
                           "Northern" = 16,
                           "North-Western" = 19,
                           "Southern" = 17,
                           "Western" = 18)


#' Recode area hierarchy

zmb <- zmb_clean %>%
  mutate(district = recode(NAME, "Shiwamg'andu" = "Shiwang'andu",
                           "Shang'ombo" = "Shangombo",
                           "Lunte District" = "Lunte"),
         province = recode(PROVINCE, "Muchiga" = "Muchinga"),
         spectrum_region_code = recode(province, !!!spectrum_region_code)) %>%
  arrange(spectrum_region_code, district)


zmb_wide <- zmb %>%
  transmute(name0 = "Zambia",
            id0 = "ZMB",
            name1 = province,
            id1 = fct_inorder(paste0("ZMB_1_", spectrum_region_code)),
            name2 = district,
            id2 = fct_inorder(paste0("ZMB_2_", as.integer(fct_inorder(district)))),
            spectrum_region_code)

#' Move Shibuyunji from Lusaka to Central province
zmb_wide <- zmb_wide %>%
  mutate(name1 = as.factor(name1),
         id1 = as.factor(id1),
         name1 = if_else(name2 == "Shibuyunji", factor("Central", levels(name1)), name1),
         id1 = if_else(name2 == "Shibuyunji", factor("ZMB_1_10", levels(id1)), id1),
         spectrum_region_code = if_else(name2 == "Shibuyunji", 10, spectrum_region_code)) %>%
  arrange(id1)

zmb_long <- gather_areas(zmb_wide)

zmb_areas <- zmb_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country", `1` = "Province", `2` = "District"),
         display = TRUE,
         spectrum_level = area_level == 1,
         epp_level = area_level == 1,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 2)


#' # Create ADR files
zmb_area_hierarchy <- zmb_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

zmb_area_boundaries <- zmb_areas %>%
  select(area_id, geometry)

zmb_area_levels <- zmb_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(zmb_areas, "zmb_areas.geojson", delete_dsn = TRUE)
sf::st_write(zmb_area_boundaries, "zmb_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(zmb_area_hierarchy, "zmb_area_hierarchy.csv")
write_csv(zmb_area_levels, "zmb_area_levels.csv")

sf::st_write(zmb_wide,"zmb_areas_wide.shp", delete_layer = TRUE)

#' Areas file for national spectrum region code
zmb_areas_spectrum_national <- zmb_areas %>%
  mutate(spectrum_region_code = 0L)

sf::st_write(zmb_areas_spectrum_national, "zmb_areas_spectrum_national.geojson", delete_dsn = TRUE)


#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(zmb_areas)

ggsave("check/zmb-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

dev.off()
