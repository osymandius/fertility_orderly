#' ## Tanzania (TZA)
#' Source: PEPFAR
#' Levels:
#'   * 1: State (2)
#'   * 2: Zone (7)
#'   * 3: Region (31)
#'   * 4: District (195)
#' Spectrum: National (level 0)
#' EPP: 7 Zones (level 2)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: District (level 4)
#' Notes:
#'   * From 2017 to 2021, EPP is fit to 26 mainland regions + Zanzibar aggregated as a single region.
#'   * For 2022, updated to 7 zones for EPP

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw/"

urls <- list(tza_pepfar = "TZA/2019-07-10/Tanzania_PROD_5_District_DistrictLsib_2019_Mar.zip",
             shp3 = "TZA/2019-11-02/tza_admbnda_adm3_20181019.zip",
             id_map = "TZA/2021-12-11 zones/tza_area_id_map2022.csv"
             ) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

tza_pepfar <- read_sf_zip(files$tza_pepfar)

#' Mafinga TC is contained in Mufindi DC

tza_pepfar %>% st_contains %>% print(n = Inf)

tza_pepfar %>%
  filter(level5name %in% c("Mufindi DC", "Mafinga TC")) %>%
  ggplot() +
  geom_sf(aes(fill = level5name), alpha = 0.3)

mufindi_updated <- st_difference(tza_pepfar %>% filter(level5name == "Mufindi DC") %>% .$geometry,
                                 tza_pepfar %>% filter(level5name == "Mafinga TC") %>% .$geometry)
plot(mufindi_updated)

st_geometry(tza_pepfar)[tza_pepfar$level5name == "Mufindi DC"] <- mufindi_updated


#' Replace Zanzibar with additonal district version

#' https://www.unicef.org/tanzania/sites/unicef.org.tanzania/files/2018-10/Tanzania-2017-Magharibi-A-District-Profile.pdf
#' https://www.unicef.org/tanzania/sites/unicef.org.tanzania/files/2018-10/Tanzania-2017-Magharibi-B-District-Profile.pdf

shp3 <- read_sf_zip(files$shp3)

magharibi_a <- c("Mwakaje", "Mfenesini", "Kama", "Bumbwisudi", "Chuini", "Mbuzini",
                 "Kihinani", "Kizimbani", "Bububu", "Dole", "Mwanyanya", "Kibweni",
                 "Sharifumsa", "Mtoni", "Kianga", "Mtoni Kidatu", "Mtopepo",
                 "Welezo", "Mwera", "Mtufaani", "Magogoni")

magharibi_b <- c("Kinuni", "Mwanakwerekwe", "Pangawe", "Melinne", "Tomondo",
                 "Mombasa", "Fuoni Kijito Upele", "Fuoni Kibondeni", "Kiembesamaki",
                 "Kisauni", "Maungani", "Chukwani", "Shakani", "Kombeni",
                 "Nyamanzi", "Dimani", "Bweleo", "Fumba")


zanzibar <- shp3 %>%
  filter(ADM1_EN %in% c("Mjini Magharibi", "Kusini Unguja", "Kaskazini Unguja")) %>%
  mutate(district = case_when(ADM3_EN %in% magharibi_a ~ "Magharibi A",
                             ADM3_EN %in% magharibi_b ~ "Magharibi B",
                             TRUE ~ as.character(ADM2_EN))) %>%
  count(region = ADM1_EN, district)

p1 <- tza_pepfar %>%
  filter(Region_Nam %in% c("Mjini Magharibi", "Kusini Unguja", "Kaskazini Unguja")) %>%
  ggplot(aes(fill = District_N)) + geom_sf() +
  ggtitle("PEPFAR March 2019 shapefile")

p2 <- zanzibar %>% ggplot(aes(fill = district)) + geom_sf() +
  ggtitle("New boundaries based on\nhumdata.org level 3")

p3 <- gridExtra::grid.arrange(p1, p2, nrow = 1)

ggsave("check/tza-comparsion-pepfar-humdata-boundaries.png", p3, h = 7, w = 14)

#' Harmonize shapefile features
tza <- tza_pepfar %>%
  transmute(region_code = Region_Cod,
            region = Region_Nam,
            district = sub("City Council", "CC", name),
            psnu_uid = uid)

#' Add region_code and psnu_uid to zanzibar
zanzibar <- zanzibar %>%
  left_join(tza %>% st_set_geometry(NULL) %>% distinct(region_code, region)) %>%
  left_join(tza %>% st_set_geometry(NULL) %>% distinct(district, psnu_uid))

tza <- tza %>%
  anti_join(
    zanzibar %>%
    st_set_geometry(NULL) %>%
    select(region_code)
  ) %>%
  rbind(zanzibar %>% select(-n))

nrow(tza) # Check 195 districts

#' Shinyanga and Geita assignments are discrepant

tza %>%
  filter(region == "Shinyanga" | region_code %in% c(17, 25)) %>%
  print(n = Inf)

tza %>%
  filter(region == "Shinyanga" | region_code %in% c(17, 25)) %>%
  ggplot() + geom_sf(aes(fill = region))


tza %>%
  filter(region == "Shinyanga" | region_code %in% c(17, 25)) %>%
  ggplot() +
  geom_sf(aes(fill = region_code)) +
  geom_sf_label(aes(label = district))

tza <- tza %>%
  mutate(region_code = if_else(district %in% c("Ushetu DC", "Msalala DC"),
                               "17", as.character(region_code)))

#' Chunya DC should be in Mbeya region

tza <- tza %>%
  mutate(region_code = if_else(district == "Chunya DC", "12", region_code),
         region = if_else(district == "Chunya DC", "Mbeya", as.character(region)))

#' Tidy shape file
tza_simple <- tza %>% rmapshaper::ms_simplify(0.05)

tza_clean <- tza_simple %>%
  st_transform(3857) %>%
  sf::st_snap(., ., tolerance = 1) %>%
  st_transform(4326) %>%
  st_make_valid()

st_is_valid(tza_clean)

tza_clean <- st_collection_extract(tza_clean, "POLYGON")


check_boundaries(tza_simple, tza_clean)

tza_pepfar %>% select %>% pryr::object_size()
tza_clean %>% select %>% pryr::object_size()


#' Shapefile needs a bit of touching up
tza_pepfar %>%
filter(level5name == "Kaliua DC") %>%
.$geometry %>%
plot


#' Recode area hierarchy

tza_zones <- list("Central" = c("Dodoma", "Singida", "Tabora"),
                  "Coastal" = c("Dar-es-salaam", "Lindi", "Morogoro", "Mtwara", "Pwani"),
                  "Lake" = c("Geita", "Kagera", "Mara", "Mwanza", "Shinyanga", "Simiyu"),
                  "Northern" = c("Arusha", "Kilimanjaro", "Manyara", "Tanga"),
                  "Southern Highland" = c("Iringa", "Mbeya", "Njombe", "Rukwa", "Ruvuma", "Songwe"),
                  "Western" = c("Katavi", "Kigoma"),
                  "Zanzibar" = c("Mjini Magharibi", "Kaskazini Pemba", "Kusini Pemba",
                                 "Kaskazini Unguja", "Kusini Unguja"))

id_map <- read_csv(files$id_map)

tza_wide <- tza_clean %>%
  mutate(
    state = if_else(region %in% c("Mjini Magharibi", "Kusini Unguja", "Kaskazini Unguja", "Kusini Pemba", "Kaskazini Pemba"), "Zanzibar", "Mainland"),
    zone = fct_collapse(region, !!!tza_zones) %>%
      fct_relevel(names(tza_zones))
  ) %>%
  arrange(zone, region_code, district) %>%
  transmute(name0 = "United Republic of Tanzania",
            id0 = "TZA",
            name1 = state,
            name2 = zone,
            name3 = region,
            name4 = district %>%
              recode("Mbongwe DC" = "Mbogwe DC"),
            spectrum_region_code = 0) %>%
  left_join(
    filter(id_map, area_level == 1) %>%
    select(name1 = area_name, id1 = area_id),
    by = "name1"
  ) %>% 
  left_join(
    filter(id_map, area_level == 2) %>%
    select(name2 = area_name, id2 = area_id),
    by = "name2"
  ) %>% 
  left_join(
    filter(id_map, area_level == 3) %>%
    select(name3 = area_name, id3 = area_id),
    by = "name3"
  ) %>% 
  left_join(
    filter(id_map, area_level == 4) %>%
    select(name4 = area_name, id4 = area_id),
    by = "name4"
  ) %>%
  mutate(
    id1 = fct_inorder(id1),
    id2 = fct_inorder(id2),
    id3 = fct_inorder(id3),
    id4 = fct_inorder(id4)
  )

stopifnot(!is.na(tza_wide$id1))
stopifnot(!is.na(tza_wide$id2))
stopifnot(!is.na(tza_wide$id3))
stopifnot(!is.na(tza_wide$id4))

tza_long <- gather_areas(tza_wide)

tza_areas <- tza_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "State",
                  `2` = "Zone",
                  `3` = "Region",
                  `4` = "District"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = 2,
         naomi_level = area_level == 4,
         pepfar_psnu_level = area_level == 4)


#' # Create ADR files

tza_area_hierarchy <- tza_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

tza_area_boundaries <- tza_areas %>%
  select(area_id, geometry)

tza_area_levels <- tza_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(tza_areas, "tza_areas.geojson", delete_dsn = TRUE)
sf::st_write(tza_area_boundaries, "tza_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(tza_area_hierarchy, "tza_area_hierarchy.csv")
write_csv(tza_area_levels, "tza_area_levels.csv")


#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(tza_areas)

ggsave("check/tza-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

dev.off()

