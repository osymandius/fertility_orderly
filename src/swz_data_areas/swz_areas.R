#' ## Eswatini (SWZ)
#' Source: Datim
#' Levels:
#'   * 1: Region (4)
#'   * 2: Inkundla (55)
#' Spectrum: National (level 0)
#' EPP: Region (level 1)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: Region (level 1)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

urls <- list(
  dhis2_id_mapping = "SWZ/2019-11-10/datim/dhis2_id_mapping.csv",
  datim_hierarchy = "SWZ/2019-11-10/datim/location_hierarchy.csv",
  datim_areas = "SWZ/2019-11-10/datim/areas.json.zip"
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

datim <- datim %>%
  filter(!grepl("\\_Military", map_name)) %>%
  mutate(parent_map_id = if_else(map_level == 0, NA_character_, parent_map_id))

datim$map_geometry[datim$map_level == 0] <- datim %>% filter(map_level == 1) %>% summarise() %>% .$map_geometry


swz <- datim %>%
  rename(geometry = map_geometry) %>%
  left_join(st_drop_geometry(.) %>% select(parent_map_id = map_id, parent_name = map_name)) %>%
  arrange(map_level, parent_name, map_name) %>%
  group_by(map_level) %>%
  mutate(area_id = case_when(map_level == 0 ~ "SWZ",
                             map_level == 1 ~ sprintf("SWZ_%d_%d", map_level, row_number()),
                             map_level == 2 ~ sprintf("SWZ_%d_%02d", map_level, row_number())),
         area_level = map_level,
         area_name = map_name) %>%
  ungroup() %>%
  left_join(st_drop_geometry(.) %>% select(parent_map_id = map_id, parent_area_id = area_id)) %>%
  mutate(spectrum_region_code = 0,
         area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "Tinkundla"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = 1,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 1)


swz_areas <- swz %>% select(area_id, area_name, area_level, parent_area_id,
                            spectrum_region_code, area_sort_order,
                            center_x, center_y, area_level_label,
                            display, spectrum_level, epp_level,
                            naomi_level, pepfar_psnu_level, geometry)


swz_wide <- spread_areas(st_drop_geometry(swz_areas)) %>%
  left_join(select(swz_areas, area_id)) %>%
  select(-area_id)

#' # Create ADR files

swz_area_hierarchy <- swz_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

swz_area_boundaries <- swz_areas %>%
  select(area_id, geometry)

swz_area_levels <- swz_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(swz_areas, "swz_areas.geojson", delete_dsn = TRUE)
sf::st_write(swz_area_boundaries, "swz_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(swz_area_hierarchy, "swz_area_hierarchy.csv")
write_csv(swz_area_levels, "swz_area_levels.csv")


#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(swz_areas)
ggsave("check/swz-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)


