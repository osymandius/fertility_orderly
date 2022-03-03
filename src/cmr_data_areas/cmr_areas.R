dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

#' Read in files from SharePoint
urls <- list( humdata = "CMR/2020-01-06/cmr_admbnda_inc_20180104_shp.zip",
              pepfar = "CMR/2019-25-06/Cameroon_HealthDistricts12012016.zip",
              dist = "CMR/2020-01-06/CameroonHierarchie.xlsx",
              raw = "CMR/2020-01-18/cmr_areas.geojson.zip"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

humdata <- read_sf_zip_list(files$humdata) %>% bind_rows()
pepfar <- read_sf_zip(files$pepfar)

#' Health districts
dist <- readxl::read_excel(files$dist, 1) %>%
  rename(region = 1, health_district = 2)

#' Statistical divisions:
stat <- readxl::read_excel(files$dist, 2)

pepfar %>%
  st_drop_geometry()  %>%
  distinct(level4na_1, level5na_1, uid) %>%
  mutate(health_district = level5na_1 %>%
           recode("Saa" = "Sa'a",
                  "Garoua 1" = "Garoua I",
                  "GAROUA 2" = "Garoua II",
                  "Malentouen" = "Malantouen",
                  "Mbangue" = "Bangue")) %>%
  full_join(dist %>% mutate(hsd = 1),
            by = c("level4na_1" = "region", "health_district")) %>%
  filter(is.na(uid) | is.na(hsd))

p_nregion_fotokol <- ggplot(filter(pepfar, level4na_1 == "Extreme Nord")) +
  geom_sf() +
  geom_sf_label(aes(label = if_else(name_1 == "Makary", "Makary", NA_character_))) +
  geom_sf(aes(fill = ADM3_FR), data = filter(humdata, ADM3_FR == "Fotokol"), alpha = 0.5) +
  naomi::th_map() +
  labs(x = NULL, y = NULL) +
  ggtitle("CMR: Extreme Nord region with Fotokol commune overlaid")


ggsave("check/check-overlay-north-region-fotokol.png", p_nregion_fotokol, h = 7, w = 7)


#' ## Edits to shape file
raw <- read_sf_zip(files$raw, pattern = "geojson$")

#' Recode types and accents
raw <- raw %>%
  mutate(area_level = as.integer(area_level),
         display = as.logical(display),
         area_name = stringr::str_replace(area_name, "\xe9", "é") %>%
           recode("Centre" = "Centre (sans Yaoundé)",
                  "Littoral" = "Littoral (sans Douala)",
                  "Yaounde" = "Yaoundé",
                  "North West" = "Nord-Ouest",
                  "Extreme Nord" = "Extreme-Nord",
                  "South West" = "Sud-Ouest"))


#' Simplify boundaries to reduce file size
raw_wide <- spread_areas(raw)

cmr_wide1 <- raw_wide %>%
  rmapshaper::ms_simplify(0.05)

cmr_wide2 <- raw_wide %>%
  rmapshaper::ms_simplify(0.02)

pryr::object_size(raw_wide)
pryr::object_size(cmr_wide1)
pryr::object_size(cmr_wide2)

raw_wide %>%
  split(1:nrow(.)) %>%
  sapply(pryr::object_size) %>%
  `/`(1e6) %>%
  summary()

cmr_wide1 %>%
  split(1:nrow(.)) %>%
  sapply(pryr::object_size) %>%
  `/`(1e6) %>%
  summary()

cmr_wide2 %>%
  split(1:nrow(.)) %>%
  sapply(pryr::object_size) %>%
  `/`(1e6) %>%
  summary()

ggplot() +
  geom_sf(data = cmr_wide2, color = "red", fill = NA) +
  geom_sf(data = cmr_wide1, color = "black", fill = NA)


cmr <- cmr_wide2 %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

pryr::object_size(cmr)

cmr_areas <- cmr %>%
  left_join(select(st_drop_geometry(raw), area_id, area_sort_order)) %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = recode(area_level,
                                   `0` = "Pays",
                                   `1` = "Région",
                                   `2` = "Département",
                                   `3` = "District Sanitaire"),
         display = TRUE) %>%
  select(names(raw)) %>%
  arrange(area_level, area_sort_order)

cmr_area_hierarchy <- cmr_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)


#' Save boundaries
sf::st_write(cmr_areas, "cmr_areas.geojson", delete_dsn = TRUE)
write_csv(cmr_area_hierarchy, "cmr_area_hierarchy.csv")

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(cmr_areas)

ggsave("check/cmr-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
dev.off()
