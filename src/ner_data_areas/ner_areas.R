#' ## Niger (NER)
#' Source:
#'   * 1: Region (8)
#'   * 2: Department (67)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "NER/2020-11-17/ner_shapefiles.zip",
             id_map = "NER/2021-01-07/ner_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw <- read_sf_zip(files$raw) %>%
  arrange(area_id1, area_id2)

raw_long <- raw %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(raw_long)

#' Fix invalid geometry
clean_long <- raw_long %>%
  st_make_valid()

stopifnot(st_is_valid(clean_long))
stopifnot(st_geometry_type(clean_long) %in% c("POLYGON", "MULTIPOLYGON"))

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

raw_2021 <- clean_long %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

raw_2021 %>% group_by(area_level) %>%
  summarise(n = n())

#' Create boundaries file
ner_areas <- raw_2021 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "Department"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(ner_areas, "ner_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(ner_areas)
ggsave("check/ner-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
