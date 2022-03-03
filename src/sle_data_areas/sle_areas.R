#' ## Sierra Leone (SLE)
#' Source:
#'   * 1: Province (4)
#'   * 2: District (16)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "SLE/2020-12-16/sle_areasdhs.geojson",
             id_map = "SLE/2021-01-05/sle_area_id_map_2021.csv"
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
plot_area_hierarchy_summary(raw)

#' Clean boundaries
sle_clean <- raw %>%
  ms_simplify(1.0) %>%
  st_union(by_feature = TRUE)
plot_area_hierarchy_summary(sle_clean)

#' Northern province border is still fragmented
#' Solution:
#'  1. Combine nested districts within Northern province
#'  to get a valid provincial border
#'  2. Subtract Koinadugu from Fabala to repair discrepant district boundaries

sle_northern <- sle_clean %>%
  filter(parent_area_id == "sle_1_3") %>%
  st_union()

sle_falaba <- st_difference(sle_clean %>% filter(area_name == "Falaba") %>% .$geometry,
                            sle_clean %>% filter(area_name == "Koinadugu") %>% .$geometry)
plot(sle_northern)
plot(sle_falaba)

#' Check that corrected boundaries nest within fragemented provincial boundaries
ggplot(sle_clean %>% filter(area_level == 1)) +
  geom_sf(color = "black") +
  geom_sf(data = sle_northern, color = "red") +
  geom_sf(data = sle_falaba, color = "green")

#' Add summed coordinates into clean shapefile
st_geometry(sle_clean[sle_clean$area_name == "Northern",]) <- sle_northern
st_geometry(sle_clean[sle_clean$area_name == "Falaba",]) <- sle_falaba
plot_area_hierarchy_summary(sle_clean)

p_compare_boundaries <- compare_boundaries(raw, sle_clean) +
  ggtitle("Sierra Leone district boundaries")

ggsave("check/sle-district-boundaries-reduced.png", p_compare_boundaries, h = 6, w = 4.5)

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

clean_2021 <- sle_clean %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

clean_2021 %>% group_by(area_level, area_level_label) %>%
  summarise(n = n())

#' Create boundaries file
sle_areas <- clean_2021 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Province",
                  `2` = "District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry) %>%
  mutate_at(vars(area_id, parent_area_id), ~ str_replace_all(., "sle","SLE" ))



#' Save boundaries
sf::st_write(sle_areas, "sle_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(sle_areas)
ggsave("check/sle-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

while (!is.null(dev.list())) dev.off()
