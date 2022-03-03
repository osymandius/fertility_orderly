#' ## Gambia (GMB)
#' Source:
#'   * 1: Survey region aggregate (5)
#'   * 2: Health regions (7)
#'   * 3: District (42)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(areas = "GMB/2021-01-12/gmb_area-id_health-region_2021_corrected.csv",             
             shape = "GMB/2021-01-12/gmb_dhis2_areas.geojson"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

## Read in files from Sharepoint
raw <- read_sf(files$shape) %>%
  filter(area_level == 2) %>%
  transmute(district = trimws(area_name))

areas <- read_csv(files$areas)

areas_wide <- areas %>%
  full_join(raw, by = c("area_name3" = "district")) %>%
  st_as_sf()

stopifnot(nrow(areas) == nrow(raw))

areas_wide <- st_make_valid(areas_wide)
areas_wide <- st_collection_extract(areas_wide, "POLYGON")


#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(areas_wide)

areas <- areas_wide %>%
  rename_all(~sub("area_", "", .)) %>%
  mutate(spectrum_region_code = 0,
         name1 = fct_inorder(name1),
         name2 = fct_inorder(name2)) %>%
  gather_areas()

areas <- st_collection_extract(areas, "POLYGON")

#' Create boundaries file
gmb_areas <- areas %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Aggregated LGA",
                  `2` = "Health region",
                  `3` = "District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)

stopifnot(st_is_valid(gmb_areas))

#' Save boundaries
sf::st_write(gmb_areas, "gmb_areas.geojson", delete_dsn = TRUE)

#' Health region level dataset
gmb_areas_region <- filter(gmb_areas, area_level %in% 0:2)
sf::st_write(gmb_areas_region, "gmb_areas_health-region.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(gmb_areas)
ggsave("gmb_area_hierarchy.png", hierarchy_plot, h = 4, w = 12)
