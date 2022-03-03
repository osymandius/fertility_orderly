#' ## Togo (TGO)
#' Source: MOH
#' Levels:
#'   * 1: Region (6)
#'   * 2: Prefecture  (39)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural: Yes
#' PEPFAR PSNU: District (level 2)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#' Read in files from SharePoint
raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/TGO/2020-01-19/tgo_areas.geojson"
raw_path <- URLencode(raw_path)
raw <- sharepoint$download(raw_path)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

raw <- read_sf(raw)

#' Recode types and accents
tgo <- raw %>%
  mutate(area_level = as.integer(area_level),
         display = as.logical(display),
         area_name = stringr::str_to_title(area_name) %>%
           recode("District Ii" = "District II",
                  "District Iii" = "District III",
                  "District Iv" = "District IV",
                  "Plaine De Mo" = "Plaine de Mo"))

#' Update from Togo estimates team:
#' 24 Jan 2020
#' * Golfe and Agoe Nyive prefecture are part of Lome region
#'
#' 21 Jan 2022
#' * For the Lomé région : merge these 6 préfectures (District I, District II,
#'   District III, District IV, District V, Golfe ) into one call it Golfe .
#'   This will leave Lomé region with 2 préfectures (Golfe and Agoe Nyive) .

# Reallocate Golfe and Agoe Nyive prefecture -> Lome region
tgo$parent_area_id <- if_else(tgo$area_name %in% c("Golfe", "Agoe Nyive"),
                              "TGO_1_3", tgo$parent_area_id)


# Create new boundary for Golfe combining Districts I, II, III, IV, V + Golfe
golfe <- filter(tgo, parent_area_id == "TGO_1_3", area_name != "Agoe Nyive") %>%
  st_union()

st_geometry(tgo[tgo$area_name == "Golfe",]) <- golfe



# Update with new area id
tgo$area_id[tgo$area_name == "Golfe"] <- "TGO_2_20sk"

# Remove old prefectures
tgo_2022 <- filter(tgo, !(area_name %in% c( "District I","District II",
                                           "District III","District IV" ,
                                           "District V")))

# Fragments in combined Golfe boundary
plot(filter(tgo_2022, area_name == "Golfe") %>% select(geometry))

# Able to get rid of almost all of them by simplifying
tgo_simple <- ms_simplify(tgo_2022, 0.9)
plot(filter(tgo_simple, area_name == "Golfe") %>% select(geometry))

tgo_wide <- naomi::spread_areas(tgo_simple)

pryr::object_size(tgo_wide)

spectrum_region_code <- c("Centrale" = 10,
                          "Kara" = 11,
                          "Lome" = 12,
                          "Maritime" = 13,
                          "Plateaux" = 14,
                          "Savanes" = 15)

tgo_long <- tgo_wide %>%
  mutate(spectrum_region_code = recode(area_name1, !!!spectrum_region_code),
         area_id0 = fct_inorder(area_id0),
         area_id1 = fct_inorder(area_id1),
         area_id2 = fct_inorder(area_id2)) %>%
  select(-geometry, everything(), geometry) %>%
  rename_all(~sub("area\\_", "", .)) %>%
  gather_areas() %>%
  st_make_valid()

tgo_areas <- tgo_long %>%
  mutate(area_sort_order = row_number(),
         area_level_label = area_level %>%
           recode(`0` = "Pays",
                  `1` = "Région",
                  `2` = "Préfecture"),
         display = TRUE,
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL) %>%
  select(names(tgo))


#' Save boundaries

sf::st_write(tgo_areas, "tgo_areas.geojson", delete_dsn = TRUE)

#' Areas file for national spectrum region code
tgo_areas_spectrum_national <- tgo_areas %>%
  mutate(spectrum_region_code = 0L)


sf::st_write(tgo_areas_spectrum_national, "tgo_areas_spectrum_national.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(tgo_areas)

ggsave("check/tgo-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
