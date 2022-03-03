#' ## Zimbabwe (ZWE)
#' Source: Boundary file and district list via email correspondence from Isaac Taramusi, 24 October 2019
#' Levels:
#'   * 1: Province (10)
#'   * 2: District (63)
#' Spectrum: Province (level 1)
#' EPP: Province (level 1)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: Former district (level 2)

#' Boundaries received from Isaac Taramusi; email 24 October 2019
#' Compared to previous districts (admin 2):
#' * Kadoma split into Mhondoro and Sanyati
#' * Guruve split into Guruve and Mbire (Mashonaland Central)


#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

urls <- list( master = "ZWE/2019-10-30/List%20of%20districts.xlsx",
              sh = "ZWE/2019-10-24/ZIM%20Updated%20Districts%20Shp.zip",
              dhis2 = "ZWE/2019-11-05/Districts%20and%20geo%20codes.xlsx",
              zwe_gadm2 = "ZWE/2019-06-25/gadm36_ZWE_2_sf.rds",
              zwe_pepfar = "ZWE/2019-06-25/ZimbabweDistrictLsib2016Sept.zip",
              zwe_uscb = "ZWE/2019-10-30/zimbabwe-adm1-adm2-uscb-oct18.zip"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

sh <- read_sf_zip(files$sh)

master <- readxl::read_excel(files$master) %>%
  fill(Province) %>%
  select(-3) %>%
  bind_rows(c(Province = "Harare", District = "Chitungwiza"))

#' Boundaries received from Zimbabwe DHIS; email from Isaac Taramusi 5 November 2019

dhis2 <- readxl::read_excel(files$dhis2) %>%
  mutate(coordinates = paste0('{ "type" : "MultiPolygon", "coordinates" : ', coordinates, ' }'))
coord <- lapply(dhis2$coordinates, read_sf)
dhis2$geometry <- do.call(rbind, coord)$geometry
dhis2$coordinates <- NULL

dhis2 <- st_as_sf(dhis2)

p1 <- dhis2 %>%
  sf::st_make_valid() %>%
  filter(Province == "Mashonaland West") %>%
  ggplot() +
  geom_sf(aes(fill = `District Name`)) +
  ggtitle("Mashonaland West", "ZWE DHIS2 boundaries (Isaac 5 November)")

dir.create("check")
ggsave("check/kadoma-urban-hole_5-november-dhis2-boundaries.png", p1, h = 7, w = 7)


p2 <- dhis2[2:3,] %>%
  sf::st_make_valid() %>%
  ggplot() +
  geom_sf() +
  facet_wrap(~`District Name`) +
  ggtitle("Harare and Chitungwiza", "ZWE DHIS2 boundaries (Isaac 5 November)")

 ggsave("check/harare_5-november-dhis2-boundaries.png",p2, h=4, w=8)

#' Comparison sources
zwe_gadm2 <- readRDS(files$zwe_gadm2)
zwe_pepfar <- read_sf_zip(files$zwe_pepfar)


#' US Census Bureau shapefile
tmpf <- tempfile()
unzip(files$zwe_uscb, exdir = tmpf)
zwe_uscb <- sf::read_sf(file.path(tmpf, "Zimbabwe_adm2_uscb_2018.shp"))

#' The shape file from Isaac has many fragements, but the USCB shapefile looks clean

p3 <- gridExtra::grid.arrange(
  sh %>% ggplot() + geom_sf() + ggtitle("Isaac shapefile"),
  zwe_uscb %>% ggplot() + geom_sf() + ggtitle("USCB shapefile"),
  sh %>% summarise() %>% ggplot() + geom_sf() + ggtitle("Isaac shapefile aggregated"),
  zwe_uscb %>% summarise() %>%  ggplot() + geom_sf() + ggtitle("USCB shapefile aggregated"),
  nrow = 2
  )

ggsave("check/isaac_uscb_shapefile_compare.png", p3, h = 8, w= 8)

#' # Spectrum region codes

spectrum_region_code <- c("Bulawayo" = 10,
                          "Harare" = 11,
                          "Manicaland" = 12,
                          "Mashonaland Central" = 13,
                          "Mashonaland East" = 14,
                          "Mashonaland West" = 15,
                          "Masvingo" = 16,
                          "Matabeleland North" = 17,
                          "Matabeleland South" = 18,
                          "Midlands" = 19)


#' # Clean up the shape file

#' There's two rows for the new Sanyati district, filling both halves of former Kadoma region.  Mhondoro is the
#' southern half, so by process of elimination, OBJECTID 74 is Sanyati and 90 should be dropped.
#'
#' It's ambiguous where Kadoma Urban gets assigned: Mhondoro or Sanyati. Will need to go back to team for this

 p4 <- sh %>%
  filter(NAME_2 %in% c("Sanyati", "KadomaUrban", "Mhondoro-Ngezi")) %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = OBJECTID)) +
  facet_wrap(~NAME_2) +
  ggtitle("Isaac shapefile")


sh <- sh %>% filter(OBJECTID != 90)

 p5 <- zwe_uscb %>%
  filter(NSO_NAME %in% c("Mhondoro-Ngezi", "Kadoma", "Sanyati")) %>%
  ggplot() +
  geom_sf() +
  facet_wrap(~NSO_NAME) +
  ggtitle("USCB shapefile")

 p6 <- gridExtra::grid.arrange(p4, p5, nrow = 2)

 ggsave("check/isaac_uscb_shapefile_fragmented_districts_compare.png", p4, h = 8, w= 8)

#' Recode district names to match master list
#'

zwe <- zwe_uscb %>%
  mutate(
    District = sub(" Urban$", "", NSO_NAME) %>%
      sub(" Rural$", "", .) %>%
      if_else(ADM2_NAME == "CHIVHU LOCAL BOARD", "Chikomba", .) %>%
      recode("Epworth" = "Seke",
             "Rusape" = "Makoni",
             "Muzarabani" = "Centenary",
             "Mt Darwin" = "Mt. Darwin",
             "Mvurwi" = "Mazowe",
             "Ruwa" = "Goromonzi",
             "Uzumba Maramba Pfungwe" = "UMP",
             "Chinhoyi" = "Makonde",
             "Kadoma" = "Sanyati",   # Confirmed by Isaac Taramusi (email 12 November)
             "Karoi" = "Hurungwe",
             "Mhondoro-Ngezi" = "Mhondoro",
             "Norton" = "Chegutu",
             "Chiredzi Town" = "Chiredzi",
             "Victoria Falls" = "Hwange",
             "Plumtree" = "Mangwe",
             "Chirumanzu" = "Chirumhanzu",
             "Gokwe Centre" = "Gokwe South",
             "Redcliff" = "Kwekwe")
  ) %>%
  group_by(District) %>%
  summarise() %>%
  ungroup()

check_boundaries(zwe)

#' Simplify
zwe_simple <- zwe %>%
  rmapshaper::ms_simplify(0.2)

pryr::object_size(zwe %>% select())
pryr::object_size(zwe_simple %>% select())

check_boundaries(zwe, zwe_simple)

#' Merge province and recode former district level
#'

zwe <- zwe_simple %>%
  left_join(master) %>%
  mutate(former_district = recode(District,
                                  "Chitungwiza" = "Harare",
                                  "Mhondoro" = "Kadoma",
                                  "Sanyati" = "Kadoma",
                                  "Mbire" = "Guruve"),
         spectrum_region_code = recode(Province, !!!spectrum_region_code)) %>%
  arrange(spectrum_region_code, District)

zwe_wide <- zwe %>%
  transmute(name0 = "Zimbabwe",
            id0 = "ZWE",
            name1 = Province,
            id1 = fct_inorder(paste0("ZWE_1_", spectrum_region_code)),
            name2 = District,
            id2 = fct_inorder(paste0("ZWE_2_", as.integer(fct_inorder(District)))),
            spectrum_region_code)

zwe_long <- gather_areas(zwe_wide)

zwe_areas <- zwe_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country", `1` = "Province", `2` = "District"),
         display = TRUE,
         spectrum_level = area_level == 1,
         epp_level = area_level == 1,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 2)


zwe_areas_wide <- spread_areas(st_drop_geometry(zwe_areas)) %>%
  left_join(select(zwe_areas, area_id)) %>%
  select(-area_id)

#' # Create ADR files

zwe_area_hierarchy <- zwe_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

zwe_area_boundaries <- zwe_areas %>%
  select(area_id, geometry)

zwe_area_levels <- zwe_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries
sf::st_write(zwe_areas, "zwe_areas.geojson", delete_dsn = TRUE)
sf::st_write(zwe_area_boundaries, "zwe_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(zwe_area_hierarchy, "zwe_area_hierarchy.csv")
write_csv(zwe_area_levels, "zwe_area_levels.csv")

#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(zwe_areas)

ggsave("check/zwe-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

dev.off()


