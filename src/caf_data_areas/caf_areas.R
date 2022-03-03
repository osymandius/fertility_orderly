#' ## Central African Republic (CAF)
#' Source: MOH (received by mail from UNAIDS)
#'   * 1: Region sanitaires (7)
#'   * 2: Prefecture (17)
#' Spectrum: 0
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "CAF/2020-11-17/CAF.geojson",
             raw2 = "CAF/2021-02-10/car_shapefiles.zip",
             id_map = "CAF/2021-02-11/caf_area_id_map_2021.csv"

) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw_wide <- read_sf(files$raw) %>%
  mutate(area_name = case_when(
    area_id == "CAF_1_2" ~ "Sangha-Mbarere",
    area_id == "CAF_1_3" ~ "Mambere-Kadei",
    area_id == "CAF_1_4" ~ "Nana-Mambere",
    area_id == "CAF_1_6" ~ "Kemo",
    area_id == "CAF_1_8" ~ "Ouham Pende",
    TRUE ~ area_name),
    spectrum_region_code = 0L) %>%
    rename(area_name2 = area_name,
           area_id2 = area_id) %>%
  select(area_id0, area_name0, area_name2, spectrum_region_code)

# Add in Regional heirarchy from district level file
hierarchy <- read_sf_zip(files$raw2) %>%
  st_drop_geometry()%>%
  select(area_name1 = Région_sa, area_name2 = Prefecture) %>%
  mutate(area_name2 = recode(area_name2,
                             "Bamingui Bangoran" = "Bamingui-Bangoran",
                             "Basse Kotto" = "Basse-Kotto",
                             "Haut Mbomou" = "Haut-Mbomou",
                             "Haute Kotto" = "Haute-Kotto"))%>%
  group_by(area_name1, area_name2) %>%
  summarise() %>%
  ungroup() %>%
  mutate(region_rank = dense_rank(as_factor(area_name1)),
         area_id1 = paste0("CAF_1_", str_pad(region_rank, 2, pad = "0"))) %>%
  arrange(area_id1, area_name2) %>%
  mutate(area_id2 = paste0("CAF_2_", str_pad(row_number(), 2, pad = "0")))


caf_wide <- raw_wide %>%
  left_join(hierarchy) %>%
  select(area_id0, area_name0, area_id1, area_name1, area_id2, area_name2,
         spectrum_region_code)

caf_long <- caf_wide %>%
  rename_all(~sub("area\\_", "", .)) %>%
  gather_areas()

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(caf_long)

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

caf_2021 <- caf_long %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

caf_2021 %>% group_by(area_level) %>%
  summarise(n = n())

#' Create boundaries file
caf_areas <- caf_2021 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region sanitaires",
                  `2` = "Prefecture"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry) %>%
  st_make_valid()

caf_areas <- st_collection_extract(caf_areas, "POLYGON")
stopifnot(st_is_valid(caf_areas))

#' Save boundaries
sf::st_write(caf_areas, "caf_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(caf_areas)
ggsave("caf_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)
