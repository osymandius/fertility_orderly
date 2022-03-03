#' ## Guinea(GIN)
#' Source:
#'   * 1: Region (8)
#'   * 2: District (38)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "GIN/2020-11-17/gin_shapefiles.zip",
             id_map = "GIN/2020-12-14/gin_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

raw <- read_sf_zip(files$raw,"gin_adm2.shp$")

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(raw)

#' Replace old area IDs with 2021 area IDs
# # Recode special characters
id_map <- read_csv(files$id_map)

raw_2021 <- raw %>%
  mutate_at(vars(area_name1, area_name2), funs(str_replace_all(., "Ã©","é" ))) %>%
  mutate(across(c(area_id1, area_id2), ~id_map$area_id_2021[match(., id_map$area_id)]))


#' Format to wide and long
raw_long <- raw_2021 %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()


#' Create boundaries file
gin_areas <- raw_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)

#' Save boundaries
sf::st_write(gin_areas, "gin_areas.geojson", delete_dsn = TRUE)
write_csv(raw_2021, "gin_area_hierarchy.csv", na = "")


#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(gin_areas)
ggsave("check/gin-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
