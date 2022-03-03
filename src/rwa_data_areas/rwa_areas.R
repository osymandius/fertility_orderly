#' ## Rwanda (RWA)
#' Source: Rwanda NISR
#' Levels:
#'   * 1: Province (5)
#'   * 2: District (30)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural: Yes
#' PEPFAR PSNU: Province (level 1)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw"
file <- "RWA/2019-10-31/District_Boundary_2012.zip"


#' Download files from SharePoint
raw_path <- URLencode(file.path(naomi_raw_path, file))
sh <- sharepoint$download(raw_path)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

sh <- read_sf_zip(sh)

rwa_simple <- sh %>% rmapshaper::ms_simplify(0.05)

rwa_clean <- rwa_simple %>%
  st_transform(3857) %>%
  sf::st_snap(., ., tolerance = 0.1) %>%
  st_transform(4326)


#' Recode area hierarchy

rwa_wide <- rwa_clean %>%
  arrange(Prov_ID, Dist_ID) %>%
  transmute(name0 = "Rwanda",
            id0 = "RWA",
            name1 = Prov_Name,
            id1 = fct_inorder(paste0("RWA_1_", Prov_ID)),
            name2 = District,
            id2 = fct_inorder(paste0("RWA_2_", Dist_ID)),
            spectrum_region_code = 0)

rwa_long <- gather_areas(rwa_wide)

rwa_areas <- rwa_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Province",
                  `2` = "District"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = 0,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 1)

rwa_areas_wide <- spread_areas(st_drop_geometry(rwa_areas)) %>%
  left_join(select(rwa_areas, area_id)) %>%
  select(-area_id)

#' # Create ADR files

rwa_area_hierarchy <- rwa_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

rwa_area_boundaries <- rwa_areas %>%
  select(area_id, geometry)

rwa_area_levels <- rwa_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(rwa_areas, "rwa_areas.geojson", delete_dsn = TRUE)
sf::st_write(rwa_area_boundaries, "rwa_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(rwa_area_hierarchy, "rwa_area_hierarchy.csv")
write_csv(rwa_area_levels, "rwa_area_levels.csv")

#' Plot hierarchy

#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(rwa_areas)

ggsave("check/rwa-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
