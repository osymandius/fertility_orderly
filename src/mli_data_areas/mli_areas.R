#' ## Mali (MLI)
#' Source:
#'   * 1: Region (12)
#'   * 2: Cercles (50)
#'   * 3: Arrondissement (289)
#'   * 4: Commune (704)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "MLI/2021-01-21/D%C3%A9coupage%20administratif%20Mali.zip",
             id_map = "MLI/2021-01-21/mli_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw <- read_sf_zip(files$raw, "adm4.*shp$")

mli_clean <- raw %>%
  select(id0 = ISO, name0 = NAME_0,
         id1 = ID_1, name1 = NAME_1,
         id2 = ID_2, name2 = NAME_2,
         id3 = ID_3, name3 = NAME_3,
         id4 = ID_4, name4 = NAME_4
         ) %>% mutate(
           id1 = paste0("MLI_1_", id1),
           id2 = paste0("MLI_2_", id2),
           id3 = paste0("MLI_3_", id3),
           id4 = paste0("MLI_4_", id4)
         )

mli_long <- mli_clean %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

stopifnot(st_is_valid(mli_long))
stopifnot(st_geometry_type(mli_long) %in% c("POLYGON", "MULTIPOLYGON"))

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(mli_long)

mli_simple <- ms_simplify(mli_long, keep = 0.08)
pryr::object_size(mli_simple)

p_compare_boundaries <- compare_boundaries(mli_long, mli_simple) +
  ggtitle("Mali district boundaries")

ggsave("check/mli-district-boundaries-reduced.png", p_compare_boundaries, h = 6, w = 4.5)

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

mli <- mli_simple %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

mli %>% group_by(area_level) %>%
  summarise(n = n())

#' Create boundaries file
mli_areas <- mli %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "Cercles",
                  `3` = "Arrondissement",
                  `4` = "Commune"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(mli_areas, "mli_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(mli_areas)
ggsave("check/mli-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
