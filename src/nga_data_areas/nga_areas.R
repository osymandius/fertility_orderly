#' ## Nigeria (NGA)
#' Source: PEPFAR
#' Levels:
#'   * 1: Zone (6)
#'   * 2: State (37)
#'   * 3: Local Government Authority (LGA; 774)
#' Spectrum: State (level 2)
#' EPP: State (level 2)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: LGA (level 3)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

urls <- list(
  dhis2_id_mapping = "NGA/2020-11-10/datim/dhis2_id_mapping.csv",
  datim_hierarchy = "NGA/2020-11-10/datim/location_hierarchy.csv",
  datim_areas = "NGA/2020-11-10/datim/areas.json.zip",
  datim_fac = "NGA/2020-11-10/datim/facility_list.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

## read in local copy of datim areas file
datim_areas <- read_sf_zip(files$datim_areas, pattern = "json$")

#' Datim

datim <- read_csv(files$datim_hierarchy) %>%
  left_join(read_csv(files$dhis2_id_mapping) %>% rename(area_id = id)) %>%
  left_join(datim_areas) %>%
  left_join({.} %>% select(parent_area_id = area_id, parent_map_id = dhis2_id)) %>%
  mutate(map_source = "Datim") %>%
  select(map_level = area_level,
         map_name = area_name,
         map_id = dhis2_id,
         parent_map_id = parent_map_id,
         map_source = map_source,
         map_geometry = geometry) %>%
  st_as_sf()

datim_fac <- read_csv(files$datim_fac) %>%
  left_join(read_csv(files$dhis2_id_mapping) %>%
            rename(facility_id = id, map_id = dhis2_id)) %>%
  left_join(read_csv(files$dhis2_id_mapping) %>%
            rename(parent_area_id = id, parent_map_id = dhis2_id)) %>%
  select(map_id, parent_map_id, facility_name:type)

datim <- datim %>%
  filter(!map_name %in%
         c("Medical and Dental Council of Nigeria, Abuja",
           "AIDS Prevention Initiative in Nigeria, LTD",
           "African Society for Laboratory Medicine (ASLM)",
           "Department of Health Planning, Research & Statistics",
           "Medical Laboratory Council of Nigeria (MLCN)",
           "_Military Nigeria",
           "HIV/AIDS Division, Federal Ministry of Health")) %>%
  mutate(parent_map_id = if_else(map_level == 0, NA_character_, parent_map_id)) %>%
  left_join(datim_fac %>% count(map_id = parent_map_id, name = "nfac"))

#' NGA LGA duplicates:
#' * im Ezinihitte (IxeWi5YG9lE, dzjXm8e1cNs): dzjXm8e1cNs is kept; IxeWi5YG9lE is deleted
#' * im Ngor Okpala (vpCKW3gWNhV, D47MUIzTapM): vpCKW3gWNhV is kept; D47MUIzTapM is deleted
#' * yo Damagum (WEoQdoRLFok): This is a town which is the seat of yo Fune (p7SAVi1RX6S). It is not an LGA

state_code <- c("Abia" = "AB", "Adamawa" = "AD", "Akwa Ibom" = "AK", "Anambra" = "AN", "Bauchi" = "BA", "Benue" = "BE",
                "Borno" = "BR", "Bayelsa" = "BY", "Cross River" = "CR", "Delta" = "DE", "Ebonyi" = "EB", "Edo" = "ED",
                "Ekiti" = "EK", "Enugu" = "EN", "FCT" = "FC", "Gombe" = "GO", "Imo" = "IM", "Jigawa" = "JI",
                "Kebbi" = "KB", "Kaduna" = "KD", "Kano" = "KN", "Kogi" = "KO", "Katsina" = "KT", "Kwara" = "KW",
                "Lagos" = "LA", "Nasarawa" = "NA", "Niger" = "NI", "Ogun" = "OG", "Ondo" = "ON", "Osun" = "OS",
                "Oyo" = "OY", "Plateau" = "PL", "Rivers" = "RI", "Sokoto" = "SO", "Taraba" = "TA", "Yobe" = "YO", "Zamfara" = "ZA")

zone_code <- c("North Central" = "NC",
               "North East" = "NE",
               "North West" = "NW",
               "South East" = "SE",
               "South South" = "SS",
               "South West" = "SW")

spectrum_region_code <- c("Abia"       = 10,
                          "Adamawa"    = 11,
                          "Akwa Ibom"   = 12,
                          "Anambra"    = 13,
                          "Bauchi"     = 14,
                          "Bayelsa"    = 15,
                          "Benue"      = 16,
                          "Borno"      = 17,
                          "Cross River" = 18,
                          "Delta"      = 19,
                          "Ebonyi"     = 20,
                          "Edo"        = 21,
                          "Ekiti"      = 22,
                          "Enugu"      = 23,
                          "FCT"        = 46,
                          "Gombe"      = 24,
                          "Imo"        = 25,
                          "Jigawa"     = 26,
                          "Kaduna"     = 27,
                          "Kano"       = 28,
                          "Katsina"    = 29,
                          "Kebbi"      = 30,
                          "Kogi"       = 31,
                          "Kwara"      = 32,
                          "Lagos"      = 33,
                          "Nasarawa"   = 34,
                          "Niger"      = 35,
                          "Ogun"       = 36,
                          "Ondo"       = 37,
                          "Osun"       = 38,
                          "Oyo"        = 39,
                          "Plateau"    = 40,
                          "Rivers"     = 41,
                          "Sokoto"     = 42,
                          "Taraba"     = 43,
                          "Yobe"       = 44,
                          "Zamfara"    = 45)

nga_wide <- datim %>%
  filter(!map_id %in% c("IxeWi5YG9lE", "WEoQdoRLFok", "D47MUIzTapM")) %>%
  st_drop_geometry() %>%
  rename_all(function(x) sub("map", "area", x)) %>%
  spread_areas() %>%
  rename(map_id = area_id) %>%
  left_join(select(as.data.frame(datim), map_id, geometry = map_geometry)) %>%
  rename(area_name3 = area_name2,
         area_name2 = area_name1,
         area_id3 = area_id2,
         area_id2 = area_id1) %>%
  mutate(
    area_name1 = fct_collapse(area_name2,
                              "North Central" = c("Benue", "Kogi", "Kwara", "Nasarawa", "Niger", "Plateau", "FCT"),
                              "North East" = c("Adamawa", "Bauchi", "Borno", "Gombe", "Taraba", "Yobe"),
                              "North West" = c("Jigawa", "Kaduna", "Kano", "Katsina", "Kebbi", "Sokoto", "Zamfara"),
                              "South East" = c("Abia", "Anambra", "Ebonyi", "Enugu", "Imo"),
                              "South South" = c("Akwa Ibom", "Cross River", "Bayelsa", "Rivers", "Delta", "Edo"),
                              "South West" = c("Ekiti", "Lagos", "Ogun", "Ondo", "Osun", "Oyo")),
    area_id1 = paste0("NGA_1_", recode(area_name1, !!!zone_code)),
    area_name3 = sub("^...", "", area_name3) %>%
      sub("[^A-Za-z]$", "", .)
  ) %>%
  st_as_sf()

nga_wide <- nga_wide %>%
  st_make_valid()

#' Get rid of fragments created by st_make_valid()
nga_wide <- st_collection_extract(nga_wide, "POLYGON")

st_is_valid(nga_wide) %>% table


object_size(nga_wide %>% select)
check_boundaries(nga_wide[1:10,])
check_boundaries(nga_wide)

#' Create area files

nga_long <- nga_wide %>%
  rename_all(function(x) sub("area\\_", "", x)) %>%
  arrange(name0, name1, name2, name3) %>%
  mutate(spectrum_region_code = recode(name2, !!!spectrum_region_code),
         id0 = fct_inorder(id0),
         id1 = fct_inorder(id1),
         id2 = fct_inorder(id2),
         id3 = fct_inorder(id3)) %>%
  gather_areas()

nga_long <- nga_long %>%
  group_by(area_level) %>%
  mutate(
    area_id = as.character(area_id),
    map_id = area_id,
    parent_map_id = parent_area_id,
    parent_area_id = NULL,
    map_level = case_when(area_level == 0 ~ 0,
                          area_level == 1 ~ NA_real_,
                          area_level %in% 2:3 ~ area_level - 1),
    area_id = case_when(area_level == 0 ~ "NGA",
                        area_level == 1 ~ area_id,
                        area_level == 2 ~ paste0("NGA_2_", recode(area_name, !!!state_code)),
                        area_level == 3 ~ sprintf("NGA_3_%03d", row_number()))
  ) %>%
  ungroup() %>%
  left_join(st_drop_geometry(.) %>%
            select(parent_area_id = area_id, parent_map_id = map_id))

nga_areas <- nga_long %>%
  select(area_id, area_name, area_level, parent_area_id, spectrum_region_code) %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Zone",
                  `2` = "State",
                  `3` = "LGA"),
         display = TRUE,
         spectrum_level = area_level == 2,
         epp_level = 2,
         naomi_level = area_level == 3,
         pepfar_psnu_level = area_level == 2)


#' # Create ADR files

nga_area_hierarchy <- nga_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

nga_area_boundaries <- nga_areas %>%
  select(area_id, geometry)

nga_area_levels <- nga_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(nga_areas, "nga_areas.geojson", delete_dsn = TRUE)
sf::st_write(nga_area_boundaries, "nga_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(nga_area_hierarchy, "nga_area_hierarchy.csv", na = "")
write_csv(nga_area_levels, "nga_area_levels.csv", na = "")

#' Plot hierarchy

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(nga_areas)

ggsave("check/nga-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

#' # Map Datim

nga_area_map_datim <- nga_long %>%
  filter(!is.na(map_level)) %>%
  select(area_id, map_id, map_level) %>%
  full_join(datim %>%
              st_drop_geometry() %>%
              select(map_level, map_name, map_id)) %>%
  mutate(map_source = "Datim") %>%
  select(area_id, map_level, map_name, map_id, map_source)


write_csv(nga_area_map_datim, "nga_area_map_datim.csv")


