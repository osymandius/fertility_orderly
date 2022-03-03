#' ## Liberia (LBR)
#' Source:
#'   * 1: County 15
#'   * 2: Health District (136)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "LBR/2020-09-21/lbr_areas.geojson",
             id_map = "LBR/2020-12-14/lbr_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw <- read_sf(files$raw)

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(raw)

lbr_simple <- rmapshaper::ms_simplify(raw, keep = 0.5)
pryr::object_size(lbr_simple)

p_compare_boundaries <- compare_boundaries(raw, lbr_simple) +
  ggtitle("Liberia district boundaries")

ggsave("check/lbr-district-boundaries-reduced.png", p_compare_boundaries, h = 6, w = 4.5)


#' Replace old area IDs with 2021 area IDs

id_map <- read_csv(files$id_map)

simple_2021 <- lbr_simple %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

simple_2021 %>% group_by(area_level, area_level_label) %>%
  summarise(n = n())

#' Create boundaries file
lbr_areas <- simple_2021 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "County",
                  `2` = "Health District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(lbr_areas, "lbr_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(lbr_areas)
ggsave("check/lbr-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
