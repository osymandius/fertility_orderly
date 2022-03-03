#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/BFA/2020-01-19/bfa_areas.geojson"
raw_path <- URLencode(raw_path)
raw_file <- sharepoint$download(raw_path)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' Recode types and accents
raw <- read_sf(raw_file) %>%
  mutate(area_level = as.integer(area_level),
         display = as.logical(display),
         area_name = stringr::str_to_title(area_name))


#' Simplify boundaries to reduce file size
raw_wide <- spread_areas(raw)

bfa_wide <- raw_wide %>%
  ms_simplify(0.2)

pryr::object_size(raw_wide)
pryr::object_size(bfa_wide)

p_boundary_check <- ggplot() +
  geom_sf(data = bfa_wide, color = "red", fill = NA) +
  geom_sf(data = raw_wide, color = "black", fill = NA)

dir.create("check")
ggsave("check/check-simplified-boundaries.png", p_boundary_check, h = 7, w = 7)

bfa <- bfa_wide %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()


bfa_area_hierarchy <- bfa %>%
  mutate(center = st_point_on_surface(bfa$geometry),
         center_x = st_coordinates(center)[,1],
         center_y = st_coordinates(center)[,2],
         center = NULL) %>%
  as.data.frame %>%
  select(-geometry)

bfa_areas <- bfa %>%
  left_join(select(st_drop_geometry(raw), area_id, area_sort_order)) %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = recode(area_level,
                                   `0` = "Pays",
                                   `1` = "Région",
                                   `2` = "District Sanitaire"),
         display = TRUE) %>%
  select(names(raw)) %>%
  arrange(area_level, area_sort_order)


#' Save boundaries
sf::write_sf(bfa_areas, "bfa_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
p_bfa_areas <- plot_area_hierarchy_summary(bfa_areas)

ggsave("bfa_area_hierarchy.png", p_bfa_areas, h = 6, w = 12)
