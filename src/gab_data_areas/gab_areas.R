#' ## Gabon (GAB)
#' Source:
#'   * 1: Province (9)
#'   * 2: Department (51)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")
sf::sf_use_s2(FALSE)

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "GAB/2020-11-17/gab_shapefiles.zip",
             id_map = "GAB/2021-08-20/gab_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw <- read_sf_zip(files$raw)

raw_long <- raw %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

# Merge Estuaire and Libreville as per country team recommendation
#' Create combined area level to use for survey
est_lib_geom <- st_union(raw_long %>% filter(area_name == "Estuaire") %>% .$geometry,
                    raw_long %>% filter(area_name == "Libreville") %>% .$geometry)


# Add combined geometery back into file
raw_long$area_name[raw_long$area_name == "Estuaire"] <- "Estuaire-Libreville"
st_geometry(raw_long[raw_long$area_name == "Estuaire-Libreville",]) <- est_lib_geom
# Fix up area_id
gab_long <- raw_long %>%
  filter( area_name != "Libreville") %>%
  mutate(parent_area_id = str_replace(parent_area_id, "GAB_1_3", "GAB_1_1"))



#' Check that regions merged correctly
ggplot() +
  geom_sf(data = gab_long %>% filter(area_level == 1), aes(fill = area_name), colour = NA) +
  geom_sf(data = gab_long %>% filter(area_level == 2), colour = "black", fill = NA)


#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(raw_long)

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map, show_col_types = FALSE)

gab_2021 <- gab_long %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

gab_2021 %>% group_by(area_level) %>%
  summarise(n = n())

#' Create boundaries file
gab_areas <- gab_2021 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Province",
                  `2` = "Department"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(gab_areas, "gab_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(gab_areas)
ggsave("check/gab-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
grDevices::dev.off()
