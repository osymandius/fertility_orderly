#' ## Democratic Republic of Congo (COD)
#' Source: UNAIDs
#'   * 1: Province (26)
#'   * 2: Territoire (164)
#'   * 3: District Sanitaire (519)
#' Spectrum: 0
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

urls <- list(sh = "COD/2020-01-19/cod_shp.zip",
             dist = "COD/2020-01-19/RDC%20hierarchie.xlsx"

) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

raw <- read_sf_zip(files$sh)
dist <- readxl::read_excel(files$dist)


#' Duplicated area ids

raw %>% group_by(area_id3) %>% filter(n() > 1)

raw %>% group_by(area_id3) %>% filter(n() > 1) %>% ggplot() + geom_sf()

#' Solution: recode the missing ones

raw <- raw %>%
  mutate(area_id3 = case_when(area_id2 == "COD_2_120" & area_name3 == "Bili" ~ "COD_3_397",
                              area_id2 == "COD_2_155" & area_name3 == "Lubunga" ~ "COD_3_498",
                              TRUE ~ area_id3)) %>%
  arrange(area_name1, area_name2, area_name3) %>%
  mutate(area_id1 = fct_inorder(area_id1),
         area_id2 = fct_inorder(area_id2),
         area_id3 = fct_inorder(area_id3))

raw %>% group_by(area_id3) %>% filter(n() > 1)

#' Update from UNAIDS: 21 Jan 2022
#' * Move Manika District Sanitaire in Kambove territory and Haut katanga province ->
#'  Mutshatsha territory in Lualaba Province

# New area IDs for provincial + territory borders that have changed
cod2022 <- raw %>%
  mutate(area_id1 = recode(area_id1, "COD_1_3" = "COD_1_3xd",
                                     "COD_1_15" = "COD_1_15sl"),
         area_id2 = recode(area_id2, "COD_2_15" = "COD_2_15td",
                                     "COD_2_90" = "COD_2_90hs"))

# Change hierarchy for Manika Health zone (borders unchanged)
cod2022$area_id2[cod2022$area_name3 == "Manika"] <- "COD_2_90hs"
cod2022$area_name2[cod2022$area_name3 == "Manika"] <- "Mutshatsha"
cod2022$area_id1[cod2022$area_name3 == "Manika"] <- "COD_1_15sl"
cod2022$area_name1[cod2022$area_name3 == "Manika"] <- "Lualaba"

#' Simplify boundaries to reduce file size

cod_wide1 <- cod2022 %>%
  rmapshaper::ms_simplify(0.05, keep_shapes  = TRUE)
cod_wide2 <- cod2022 %>%
  rmapshaper::ms_simplify(0.025, keep_shapes  = TRUE)

pryr::object_size(cod2022)
pryr::object_size(cod_wide1)
pryr::object_size(cod_wide2)

p1 <- ggplot() +
  geom_sf(data = cod_wide2, color = "red", fill = NA) +
  geom_sf(data = cod2022, color = "black", fill = NA)


ggsave("check/cod-check-simplified-boundaries.png", p1, h = 7, w = 7)

cod <- cod_wide2 %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

pryr::object_size(cod)

cod_areas <- cod %>%
  mutate(area_sort_order = dplyr::row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = recode(area_level,
                                   `0` = "Pays",
                                   `1` = "Province",
                                   `2` = "Territoire",
                                   `3` = "District Sanitaire"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(cod_areas, "cod_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(cod_areas)

ggsave("check/hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

dev.off()

