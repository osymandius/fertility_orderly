#' ## Malawi (MWI)
#' Source: https://data.humdata.org/dataset/2018_malawi_ta_dataset-updated-admin3 (2018 Census Admin 3 shapefile from NSO)
#' Levels:
#'   * 1: Region (3)
#'   * 2: Zone (5)
#'   * 3: District (28)
#'   * 4: Health district (29)
#'   * 5: Health district + Cities (33)
#'   * 6: Traditional Authority
#' Spectrum: National
#' EPP: Region (level 1)
#' EPP Urban/Rural: No
#' PEPFPAR PSNU: District (level 3)


sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

mwi_humdata_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/shape files/humdata/Malawi/mwi_ta_adm3_nso_2018_20190327.zip"

mwi_humdata_file <- sharepoint$download(mwi_humdata_path)

tmpd <- tempfile()
unzip(mwi_humdata_file, exdir = tmpd)
mwi_humdata <- read_sf(list.files(tmpd, "shp$", recursive = TRUE, full.names = TRUE))


#' Split Mzimba district into Mzimba North and Mzimba South health districts based on TAs

mzimba_north <- c("TA Jaravikuba Munthali", "TA Mpherembe", "TA Mtwalo", "Vwaza Marsh Reserve")
mzimba_south <- c("Mzimba Boma", "TA Kampingo Sibande", "TA Khosolo Gwaza Jere", "TA Chindi",
                  "TA M'Mbelwa", "TA Mabulabo", "TA Mzikubola", "TA Mzukuzuku", "STA Levi Jere")

mwi_simple <- mwi_humdata %>%
  rmapshaper::ms_simplify(0.05)

pryr::object_size(mwi_humdata)
pryr::object_size(mwi_simple)


p_boundary_check <- check_boundaries(mwi_simple)

dir.create("check")
ggsave("check/check-simplified-boundaries.png", p_boundary_check, h = 7, w = 7)


mwi_clean <- mwi_simple %>%
  sf::st_snap(., ., tolerance = 0.0001) %>%
  sf::st_make_valid() %>%
  sf::st_transform(4326) 


ggsave("check/check-cleaned-boundaries.png", check_boundaries(mwi_clean), h = 7, w = 7)


mwi <- mwi_clean %>%
  mutate(health_district_city = case_when(TA_NAME %in% mzimba_north ~ "Mzimba North",
                                          TA_NAME %in% mzimba_south ~ "Mzimba South",
                                          TRUE ~ as.character(DIST_NAME)),
         health_district = health_district_city  %>%
           fct_recode("Zomba" = "Zomba City",
                      "Blantyre" = "Blantyre City",
                      "Lilongwe" = "Lilongwe City",
                      "Mzimba North" = "Mzuzu City"),
         district = health_district %>% fct_collapse("Mzimba" = c("Mzimba North", "Mzimba South")),
         zone = district %>%
           fct_collapse("Northern" = c("Chitipa", "Karonga", "Nkhatabay", "Rumphi", "Mzimba", "Likoma"),
                        "Central Eastern" = c("Kasungu", "Nkhotakota", "Ntchisi", "Dowa", "Salima"),
                        "Central Western" = c("Lilongwe", "Mchinji", "Dedza", "Ntcheu"),
                        "South Eastern" = c("Mangochi", "Machinga", "Balaka", "Zomba", "Mulanje", "Phalombe"),
                        "South Western" = c("Chiradzulu", "Blantyre", "Mwanza", "Thyolo", "Chikwawa", "Nsanje", "Neno")) %>%
           fct_relevel("Northern", "Central Eastern", "Central Western", "South Eastern", "South Western")
         ) %>%
  arrange(REG_CODE, zone, TA_CODE) 
  

#' Recode area hierarchy

mwi_wide <- mwi %>%
  transmute(name0 = "Malawi",
            id0 = "MWI",
            name1 = REG_NAME,
            id1 = fct_inorder(paste0("MWI_1_", REG_CODE)),
            name2 = zone,
            id2 = fct_inorder(paste0("MWI_2_", as.integer(fct_inorder(zone)))),
            name3 = district,
            id3 = fct_inorder(sprintf("MWI_3_%02d", as.integer(fct_inorder(district)))),
            name4 = health_district,
            id4 = fct_inorder(sprintf("MWI_4_%02d", as.integer(fct_inorder(health_district)))),
            name5 = health_district_city,
            id5 = fct_inorder(sprintf("MWI_5_%02d", as.integer(fct_inorder(health_district_city)))),
            name6 = TA_NAME,
            id6 = fct_inorder(sprintf("MWI_6_%03d", as.integer(fct_inorder(TA_CODE)))),
            spectrum_region_code = 0L)
         
mwi_long <- gather_areas(mwi_wide)

mwi_areas_ta <- mwi_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "Zone",
                  `3` = "District",
                  `4` = "Health District",
                  `5` = "Health District + Cities",
                  `6` = "Traditional Authority"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = area_level == 1,
         naomi_level = area_level == 5,
         pepfar_psnu_level = area_level == 3)


mwi_areas <- mwi_areas_ta %>%
  filter(area_level != 6)

#' # Create ADR files

mwi_area_hierarchy <- mwi_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

mwi_area_boundaries <- mwi_areas %>%
  select(area_id, geometry)

mwi_area_levels <- mwi_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(mwi_areas_ta, "mwi_areas_ta.geojson", delete_dsn = TRUE)

sf::st_write(mwi_areas, "mwi_areas.geojson", delete_dsn = TRUE)
sf::st_write(mwi_area_boundaries, "mwi_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(mwi_area_hierarchy, "mwi_area_hierarchy.csv")
write_csv(mwi_area_levels, "mwi_area_levels.csv")


path <- file.path(getwd(), "mwi_areas_wide.zip")
tmp <- file.path(tempdir(), "mwi_areas_wide")
dir.create(tmp)
sf::st_write(mwi_wide, file.path(tmp, "mwi_areas_wide.shp"), delete_layer = TRUE)
withr::with_dir(tmp, zip(path, list.files()))


#' Plot hierarchy

p_mwi_areas <- mwi_areas %>%
  mutate(area_level_label = area_level_label %>% fct_reorder(area_level)) %>%
  ggplot() +
  geom_sf() +
  facet_wrap(~area_level_label, nrow = 1) +
  naomi:::th_map()

ggsave("mwi_area_hierarchy.png", p_mwi_areas, h = 6, w = 12)

# dev.off()

while (!is.null(dev.list())) dev.off()
