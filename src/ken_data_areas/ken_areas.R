#' ## Kenya (KEN)
#' Source:
#' Levels:
#'   * 1: Former Province (8)
#'   * 2: County (47)
#' Spectrum:
#' EPP: National
#' EPP Urban/Rural:
#' PEPFAR PSNU: County (level 1)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

urls <- list(ken_pepfar = "KEN/2019-06-25/KenyaCountyLsibSept2016.zip",
             iebc1 = "KEN/2019-12-09/ken_adm_iebc_20191031_shp.zip",
             dist = "KEN/2019-12-10/AREA_ID_ESTIMATES%20for%20use%20in%20Naomi%20Model%2009_12_2019.xlsx"

) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

tmpf <- tempfile()
unzip(files$iebc1, exdir = tmpf)
iebc1 <- read_sf(file.path(tmpf, "ken_adm_iebc_20191031_SHP/ken_admbnda_adm1_iebc_20191031.shp"))

dist <- files$dist %>%
  readxl::read_excel(skip= 2) %>%
  select(1:5)

#' PEPFAR shapefile for coastline
ken_pepfar <- read_sf_zip(files$ken_pepfar)

ken_pepfar0 <- ken_pepfar %>%
  sf::st_make_valid() %>%
  st_union() %>%
  st_as_sf() %>%
  ms_simplify(0.1) %>%
  sf::st_make_valid()


#' Simplify
ken_simple <- iebc1 %>%
  ms_simplify(0.1)

pryr::object_size(iebc1 %>% select())
pryr::object_size(ken_simple %>% select())

check_boundaries(iebc1, ken_simple)
check_boundaries(ken_pepfar0, ken_simple)

#' # Spectrum region codes

spectrum_region_code <- c("Central" = 10,
                          "Coast" = 11,
                          "Eastern" = 12,
                          "Nairobi" = 13,
                          "North Eastern" = 14,
                          "Nyanza" = 15,
                          "Rift Valley" = 16,
                          "Western" = 17)


#' Recode area hierarchy

ken_wide <- dist %>%
  transmute(name0 = "Kenya",
            id0 = "KEN",
            name1 = Province,
            id1 = sprintf("KEN_1_%d", Province_Code),
            name2 = County__Name %>%
              recode("Taita–Taveta" = "Taita-Taveta"),
            id2 = sprintf("KEN_2_%02d", County_Code),
            spectrum_region_code = recode(Province, !!!spectrum_region_code)) %>%
  full_join(select(ken_simple, name2 = ADM1_EN) %>%
            mutate(name2 = recode(name2,
                                  "Nairobi" = "Nairobi (County)",
                                  "Taita Taveta" = "Taita-Taveta",
                                  "Trans Nzoia" = "Trans-Nzoia"))) %>%
  st_as_sf()

ken_long <- gather_areas(ken_wide)

ken_areas <- ken_long %>%
  arrange(area_level, area_name) %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Former Province",
                  `2` = "County"),
         display = TRUE,
         spectrum_level = area_level == 1,
         epp_level = area_level == 1,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 2)


## ken_areas_clipped <- st_intersection(ken_areas, ken_pepfar0)

ken_area_hierarchy <- ken_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

ken_area_boundaries <- ken_areas %>%
  select(area_id, geometry)

ken_area_levels <- ken_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' Save boundaries
sf::st_write(ken_areas, "ken_areas.geojson", delete_dsn = TRUE)
sf::st_write(ken_area_boundaries, "ken_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(ken_area_hierarchy, "ken_area_hierarchy.csv")
write_csv(ken_area_levels, "ken_area_levels.csv")


path <- file.path(getwd(), "ken_areas_wide.zip")
tmp <- file.path(tempdir(), "ken_areas_wide")
dir.create(tmp)
sf::st_write(ken_wide, file.path(tmp, "ken_areas_wide.shp"), delete_layer = TRUE)
with_dir(tmp, zip(path, list.files()))



#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(ken_areas)
ggsave("check/ken-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
