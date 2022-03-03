#' ## Namibia (NAM)
#' Source: MoH via CDC
#' Levels:
#'   * 1: Region (14)
#'   * 2: Health District (38)
#' Spectrum: National (level 0)
#' EPP: Region (level 1)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: Health district (level 2)

#' Issues:
#'
#' * MoH shapefile has 38 districts; DHIS and Datim have 34. DHIS and Datim are consistent.
#' * Level 2 boundaries in Datim and DHIS are different.
#' * Level 1 and Level 2 boundaries in Datim are not consistent
#'
#'
#' * Datim has former Kavango region
#'
#' * Recodes:
#'   * "Grootfontein" --> divided into "Grootfontein" and "Tsumkwe"
#'   * "Oshakati" --> divided into "Oshakati and "Ondangwa"
#'   * "Luderitz" --> divided into "Rosh Pinah" and "!Nami#nus"
#'   * "Rundu" and "Nankudu" --> "Ncamangoro", "Nkurenkuru", "Rundu"
#'   * "Onandjokwe" --> divided into "Onandjokwe" and "Omuthiya"
#'   * "Mariental" and "Aranos" --> "Mariental"

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

urls <- list(sh = "NAM/2020-01-22/NamibiaHealth38Districts.zip",
             dist = "NAM/2019-10-30/2019,%20Namiba,%20areas.xlsx",
             nam_pepfar = "NAM/2019-06-25/NamibiaHealthDistrictsLsib2016Dec.zip",
             humdata = "NAM/2019-12-08/nam_admbnda_nsa_ocha_shp.zip"
             # dhis_areas = "NAM/2019-12-08/nam_admbnda_nsa_ocha_shp.zip",
             # dhis_id_map = "NAM/2019-11-09/dhis/dhis2_id_mapping.csv",
             # dhis_heirarchy = "NAM/2019-11-09/dhis/location_hierarchy.csv",
             # datim

) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

sh <- read_sf_zip(files$sh)
dist <- readxl::read_excel(files$dist) %>%
  select(1:3) %>%
  rename(country = 1, region = 2, health_district = 3)

#' # Comparison shapefiles
nam_pepfar <- read_sf_zip(files$nam_pepfar)
#' (PEPFAR shapefile is identical)

#' Constituency shapefile
tmp <- tempfile()
unzip(files$humdata, exdir = tmp)
humdata <- read_sf(file.path(tmp, "nam_admbnda_NSA_OCHA_SHP/nam_admbnda_adm2_NSA_OCHA.shp"))

# dhis <- read_csv(here("NAM/data-raw/dhis/location_hierarchy.csv")) %>%
#   left_join(
#     read_csv(here("NAM/data-raw/dhis/dhis2_id_mapping.csv")) %>%
#     rename(area_id = id)
#   ) %>%
#   left_join(read_sf(here("NAM/data-raw/dhis/areas.json"))) %>%
#   left_join({.} %>% select(parent_area_id = area_id, parent_map_id = dhis2_id)) %>%
#   mutate(map_source = "MOHSS DHIS2") %>%
#   select(map_level = area_level,
#          map_name = area_name,
#          map_id = dhis2_id,
#          parent_map_id = parent_map_id,
#          map_source = map_source,
#          map_geometry = geometry) %>%
#   st_as_sf()
#
#
# datim <- read_csv(here("NAM/data-raw/datim/location_hierarchy.csv")) %>%
#   left_join(
#     read_csv(here("NAM/data-raw/datim/dhis2_id_mapping.csv")) %>%
#     rename(area_id = id)
#   ) %>%
#   left_join(read_sf(here("NAM/data-raw/datim/areas.json"))) %>%
#   left_join({.} %>% select(parent_area_id = area_id, parent_map_id = dhis2_id)) %>%
#   mutate(map_source = "Datim") %>%
#   select(map_level = area_level,
#          map_name = area_name,
#          map_id = dhis2_id,
#          parent_map_id = parent_map_id,
#          map_source = map_source,
#          map_geometry = geometry) %>%
#   st_as_sf()
#
#
# dhis %>% filter(map_level == 2) %>% arrange(map_name)
# datim_boundaries %>% filter(map_level == 2) %>% arrange(map_name)

#' # Clean boundaries

pryr::object_size(sh)
check_boundaries(sh)

nam_clean <- nam_pepfar %>%
  st_transform(3857) %>%
  sf::st_snap(., ., tolerance = 0.0001) %>%
  st_transform(4326)

check_boundaries(nam_clean)

#' Couple of slivers remaining
check_boundaries(nam_clean)
check_boundaries(sh, nam_clean)


#' # Code areas
#'
#' Note: In shapefile Ncamangoro is west of Nkurenkuru. But on Google maps driving
#' directions, Nkurenkuru is to the west of Ncamagoro:
#' * https://goo.gl/maps/cw4kL3JoETpCvxoZA
#'
#' This is also more consistent with health facility locations.  Swap these lables.
#'
nam_wide <- nam_clean %>%
  arrange(RegionNum, District_1) %>%
  transmute(name0 = "Namibia",
            id0 = "NAM",
            name1 = Region_12,
            id1 = fct_inorder(sprintf("NAM_1_%02d", RegionNum)),
            name2 = recode(District_1,
                           "Kongo" = "Okongo",
                           "Oshikati" = "Oshakati",
                           "Nkurenkuru" = "Ncamagoro",
                           "Ncamangoro" = "Nkurenkuru"),
            id2 = fct_inorder(sprintf("NAM_2_%02d", as.integer(fct_inorder(paste(Region_12, District_1))))),
            spectrum_region_code = 0, uid)

nam_long <- gather_areas(nam_wide)

nam_areas <- nam_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country", `1` = "Region", `2` = "District"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = area_level == 1,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 2)


#' # Create ADR files

nam_area_hierarchy <- nam_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

nam_area_boundaries <- nam_areas %>%
  select(area_id, geometry)

nam_area_levels <- nam_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(nam_areas, "nam_areas.geojson", delete_dsn = TRUE)
sf::st_write(nam_area_boundaries, "nam_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(nam_area_hierarchy, "nam_area_hierarchy.csv")
write_csv(nam_area_levels, "nam_area_levels.csv")


#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(nam_areas)

ggsave("check/hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
