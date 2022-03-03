#' ## Ghana (GHA)
#' Source: National Malaria Control Programme
#'   * 1: Region (16)
#'   * 2: District (260)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural: Yes
#' PEPFAR PSNU: District (level 2)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "GHA/2020-11-17/gha_adm2.zip",
             id_map = "GHA/2021-01-06/gha_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

raw <- read_sf_zip(files$raw)

gha_simple <- ms_simplify(raw, keep = 0.03)

pryr::object_size(raw)
pryr::object_size(gha_simple)

p_compare_ghana <- compare_boundaries(raw, gha_simple) +
  ggtitle("Ghana district boundaries")

p_compare_accra <- compare_boundaries(
  filter(raw, area_name1 == "Greater Accra"),
  filter(gha_simple, area_name1 == "Greater Accra")
) +
  ggtitle("Greater Accra district boundaries")

ggsave("check/ghana-district-boundaries-reduced.png", p_compare_ghana, h = 6, w = 4.5)
ggsave("check/greater-accra-district-boundaries-reduced.png", p_compare_accra, h = 4.5, w = 6)


#' Replace old area IDs with 2021 area IDs

gha_long <- gha_simple %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

id_map <- read_csv(files$id_map)

raw_2021 <- gha_long %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

raw_2021 %>% group_by(area_level) %>%
  summarise(n = n())

gha_areas <- raw_2021 %>%
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

sf::write_sf(gha_areas, "gha_areas.geojson", delete_dsn = TRUE)


#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(gha_areas)

ggsave("gha_area_hierarchy.png", hierarchy_plot, h = 4, w = 8)
