#' ## Lesotho (LSO)
#' Source: https://data.humdata.org/dataset/lesotho-administrative-level-0-2-boundaries
#' Levels:
#'   * 1: District (10)
#'   * 2: Community Council (76)
#' Spectrum: National
#' EPP: National
#' EPP Urban/Rural: Yes
#' PEPFAR PSNU: District (level 1)


#' Sharepoint
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
sharepoint_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/"

#' humdata.org community council shapefile
#'

lso_humdata2_path <- "shape files/humdata/Lesotho/lso_adm_fao_mlgca_2019_shp.zip"
lso_humdata2_file <- file.path(sharepoint_path, lso_humdata2_path) %>%
  URLencode() %>%
  sharepoint$download()

lso_humdata2 <- read_sf_zip(lso_humdata2_file, "adm2.*shp$")


#' Lesotho Bureau of statistics
#' Shared via Lesotho HIV estimates team

bos_path <- "naomi-raw/LSO/2019-11-06/DISTRICTS AND COUNCILS BOUNDARIES.zip"

bos_file <- file.path(sharepoint_path, bos_path) %>%
  URLencode() %>%
  sharepoint$download()

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

bos_dir <- tempfile()
unzip(bos_file, exdir = bos_dir)

bos_cc <- read_sf(file.path(bos_dir, "DISTRICTS AND COUNCILS BOUNDARIES/CommunityCouncils_NEW_final.shp"))
bos_dist <- read_sf(file.path(bos_dir, "DISTRICTS AND COUNCILS BOUNDARIES/DISTRICT_BOUNDARIES.shp"))
bos_peri <- read_sf(file.path(bos_dir, "DISTRICTS AND COUNCILS BOUNDARIES/Peri_Urban_Final.shp"))
bos_urban <- read_sf(file.path(bos_dir, "DISTRICTS AND COUNCILS BOUNDARIES/Urban_Councils.shp"))

bos_cc <- st_transform(bos_cc, 4326)
bos_dist <- st_transform(bos_dist, 4326)
bos_peri <- st_transform(bos_peri, 4326)
bos_urban <- st_transform(bos_urban, 4326)


dist_path <- "naomi-raw/LSO/2019-11-06/2019 Lesotho Areas (002)_Community Councils.csv"

dist <- file.path(sharepoint_path, dist_path) %>%
  URLencode() %>%
  sharepoint$download() %>%
  read_csv() %>%
  rename(district = 2, cc = 3)

coord <- coord_sf(xlim = c(27.0, 29.5), ylim = c(-30.7, -28.5))

p1 <- ggplot(bos_dist) +
  geom_sf(fill = "green4") +
  coord +
  ggtitle("DISTRICT_BOUNDARIES.shp", "10 districts (all good)")

p2 <- ggplot(bos_cc) + geom_sf(fill = "lightblue3") + coord + ggtitle("CommunityCouncils_NEW_final.shp", "64 community councils, excludes urban")

p3 <- ggplot(bos_cc) +
  geom_sf(data = bos_cc, fill = "lightblue3") +
  geom_sf(data = bos_urban, fill = "red3", alpha = 0.6) +
  ggtitle("Urban_Councils.shp", "12 urban councils, some overlaps with CommunityCouncils_NEW_final.shp") +
  coord

p4 <- ggplot() +
  geom_sf(data=bos_cc, fill = "lightblue3") +
  geom_sf(data=bos_peri, fill = "red3", alpha = 0.6) +
  coord +
  ggtitle("Peri_Urban_Final.shp", "32 areas, entirely overlaps with CommunityCouncils_NEW_final.shp")

p <- arrangeGrob(p1, p2, p3, p4)

dir.create("check")
ggsave("check/lesotho-BOS-shapefile-summary.pdf", p, h=8, w=10)

bos_cc <- bos_cc %>%
  st_join(bos_dist, largest = TRUE)

bos_cc %>%
  st_drop_geometry() %>%
  count(NAME)

dist <- dist %>%
  mutate(Longitude = -abs(Longitude)) %>%
  st_as_sf(coords = c("Latitude", "Longitude"), remove = FALSE) %>%
  `st_crs<-`(4326)

#' Compare the Lat/Long in the district and community council list
#' against the shapefiles. Unclear correspondence
#'
## ggplot(bos_cc) +
##   geom_sf(data = bos_cc, fill = "lightblue3") +
##   geom_sf(data = bos_peri, fill = "red3", alpha = 0.6) +
##   geom_sf(data = bos_urban, fill = "darkgreen", alpha = 0.6) +
##   geom_sf(data = dist)


#' Decision: Use humdata.org shapefile
#'
#' Simplify boundaries

lso_simple <- lso_humdata2 %>%
  rmapshaper::ms_simplify()

object_size(select(lso_humdata2))
object_size(select(lso_simple))

ggsave("check/check-simplified-boundaries.png",
       check_boundaries(lso_humdata2, lso_simple),
       h = 5, w = 10)

#' Create area hierarchy

lso_simple <- lso_simple %>%
  mutate(ADM2_EN = recode(ADM2_EN, "Maseru (Berea Distict)" = "Maseru (Berea District)"))

lso_wide <- lso_simple %>%
  arrange(ADM2_PCODE) %>%
  transmute(
    name0 = "Lesotho",
    id0 = "LSO",
    name1 = ADM1_EN,
    id1 = fct_inorder(paste0("LSO_1_", fct_inorder(ADM1_PCODE) %>% as.integer)),
    name2 = ADM2_EN,
    id2 = fct_inorder(paste0("LSO_2_", fct_inorder(ADM2_PCODE) %>% as.integer)),
    spectrum_region_code = 0L
  )

lso_long <- gather_areas(lso_wide)

lso_areas <- lso_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "District",
                  `2` = "Community Council"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = area_level == 0,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 1)


#' # Create ADR files

lso_area_hierarchy <- lso_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)


#' # Save boundaries

write_sf(lso_areas, "lso_areas.geojson", delete_dsn = TRUE)
write_csv(lso_area_hierarchy, "lso_area_hierarchy.csv")
write_sf_shp_zip(lso_wide, "lso_areas_wide.zip")


#' Plot hierarchy

p_lso_areas <- plot_area_hierarchy_summary(lso_areas)

ggsave("lso_area_hierarchy.png", h = 6, w = 12)


dev.off() # gridExtra::arrangeGrob() opened a device...
