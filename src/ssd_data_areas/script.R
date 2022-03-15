## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

path <- "Shared Documents/Data/naomi-raw/SSD/2022-03-13 humdata-shapefile/ssd_adm_imwg_nbs_20220121.zip"

path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), path)

files <- sharepoint$download(path)

tempdir <- tempdir()
unzip(files, exdir = tempdir)
admin0 <- read_sf(file.path(tempdir, "ssd_admbnda_adm0_imwg_nbs_20210924.shp")) %>%
  rename(area_name = ADM0_EN) %>%
  mutate(area_id = "SSD",
         area_level = 0,
         area_level_label = "Country",
         spectrum_level = TRUE,
         epp_level = TRUE,
         naomi_level = FALSE,
         pepfar_psnu_level = FALSE,
         display = TRUE)

admin1 <- read_sf(file.path(tempdir, "ssd_admbnda_adm1_imwg_nbs_20210924.shp")) %>%
  rename(area_name = ADM1_EN) %>%
  mutate(area_id = paste0("SSD_1_", row_number()))

admin1 <- admin1 %>%
  select(area_id, area_name, geometry) %>%
  mutate(parent_area_id = "SSD",
         area_level = 1,
         area_level_label = "State",
         spectrum_level = FALSE,
         epp_level = FALSE,
         naomi_level = FALSE,
         pepfar_psnu_level = FALSE,
         display = TRUE)

admin2 <- read_sf(file.path(tempdir, "ssd_admbnda_adm2_imwg_nbs_20180817.shp")) %>%
  rename(area_name = ADM2_EN) %>%
  mutate(area_id = paste0("SSD_2_", row_number()))

admin2 <- admin2 %>%
  select(area_id, area_name, geometry, parent_area_name = ADM1_EN) %>%
  left_join(admin1 %>% select(parent_area_name = area_name, parent_area_id = area_id) %>% st_drop_geometry()) %>%
  select(area_id, area_name, parent_area_id) %>%
  mutate(area_level = 2,
         area_level_label = "County",
         spectrum_level = FALSE,
         epp_level = FALSE,
         naomi_level = TRUE,
         pepfar_psnu_level = TRUE,
         display = TRUE)

areas <- admin0 %>%
  bind_rows(admin1, admin2) %>%
  mutate(area_sort_order = row_number(),
         spectrum_region_code = NA,
         center = st_centroid(geometry),
         center_x = do.call(rbind, center)[,1],
         center_y = do.call(rbind, center)[,2]) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code, area_level_label, display, spectrum_level, epp_level, naomi_level, pepfar_psnu_level) %>%
  st_make_valid()

st_write(areas,"ssd_areas.geojson", delete_dsn = TRUE)