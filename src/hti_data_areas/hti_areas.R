#' ## Haiti (HTI)
#' Source:
#' Levels:
#'   * 0: Pays
#'   * 1: Département (10)
#'   * 2: Commune (140)
#'   * 3: Arrondissement (42)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU: District (level 2)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

file <- "HTI/2020-01-14/HaitiShape.zip"

#' Download files from SharePoint
raw_path <- URLencode(file.path(naomi_raw_path, file))
raw <- sharepoint$download(raw_path)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

raw <- read_sf_zip(raw)

hti_simple <- rmapshaper::ms_simplify(raw, 0.2)

pryr::object_size(raw)
pryr::object_size(hti_simple)

hti_simple <- hti_simple %>%
  rename(name0 = Pays,
         name1 = `Départeme`,
         name2 = Arrondisse,
         name3 = Commune) %>%
  mutate(spectrum_region_code = 0) %>%
  arrange(ADM2_PCODE)

hti_simple <- sf::st_make_valid(hti_simple)

p1 <- ggplot() +
  geom_sf(data = raw, fill = NA, color = "red") +
  geom_sf(data = hti_simple, fill = NA, color = "blue")

ggsave("check/hti-compare-simplified-boundaries.png", p1, h = 6, w = 10)

#' Generate random ids
gen_id <- function(iso3, level, nchar = 5) {
  str <- paste0(sample(c(letters, 0:9), nchar, replace = TRUE), collapse = "")
  paste0(iso3, "_", level, "_", str)
}

hti_wide <- hti_simple %>%
  mutate(id0 = "HTI") %>%
  group_by(name0, name1) %>%
  mutate(id1 = gen_id("HTI", 1)) %>%
  group_by(name0, name2) %>%
  mutate(id2 = gen_id("HTI", 2)) %>%
  group_by(name0, name3) %>%
  mutate(id3 = gen_id("HTI", 3)) %>%
  ungroup()

hti <- gather_areas(hti_wide)

hti %>%
  st_drop_geometry() %>%
  count(substr(area_id, 7, 11)) %>%
  count(n)

hti_areas <- hti %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Pays",
                  `1` = "Département",
                  `2` = "Arrondissement",
                  `3` = "Commune"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


hti_areas %>%
  filter(area_level %in% 0:2) %>%
  sf::st_write("hti_areas.geojson", delete_dsn = TRUE)

sf::st_write(hti_areas, "hti_areas_commune.geojson", delete_dsn = TRUE)

hti_area_hierarchy <- hti_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

write_csv(hti_area_hierarchy, "hti_area_hierarchy.csv")

#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(hti_areas)

ggsave("check/hti-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

