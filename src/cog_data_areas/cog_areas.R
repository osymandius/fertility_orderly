#' ## Congo (COG)
#' Source:
#'   * 1: Combined Region
#'   * 2: Region (12)
#'   * 3: District (52)
#' Spectrum: 0
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "COG/2020-11-17/cog_shapefile.zip",
             id_map = "cog/2021-01-22/cog_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw <- read_sf_zip(files$raw)

#' Source file contains duplicated area ID (COG_2_45) -> from source file meta
#' data: One district (ille M’Bamou) was carved out of Ignie-Ngabe-Mayama
#' district, Pool region based on OSM basemap but area IDs were not split
#' Solution: rename area IDs

cog_clean <- raw %>%
  mutate(
    area_name1 = str_to_sentence(area_name1),
    area_id2 =
      case_when(area_name2 == "Ilé M'bamou" ~ "COG_2_46",
                area_id2 == "COG_2_46" ~ "COG_2_47",
                area_id2 == "COG_2_47" ~ "COG_2_48",
                area_id2 == "COG_2_48" ~ "COG_2_49",
                area_id2 == "COG_2_49" ~ "COG_2_50",
                area_id2 == "COG_2_50" ~ "COG_2_51",
                area_id2 == "COG_2_51" ~ "COG_2_52",
                TRUE ~ area_id2)
  )

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(cog_clean)

#' Create combined area level to use for survey
cog_long <- cog_clean %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

p_noire_kouilou <- st_union(cog_long %>% filter(area_name == "Pointe-noire") %>% .$geometry,
                            cog_long %>% filter(area_name == "Kouilou") %>% .$geometry)

pool_brazza <- st_union(cog_long %>% filter(area_name == "Pool") %>% .$geometry,
                        cog_long %>% filter(area_name == "Brazzaville") %>% .$geometry)

#' Add summed coordinates into clean shapefile
combo_areas <- cog_long %>% filter(area_level %in% 0:1,
                                     area_name != "Pool",
                                     area_name != "Pointe-noire")
#' Fix up area IDs
combo_areas$area_id[combo_areas$area_name == "Sangha"] <- "COG_1_10"

#' Replace old geometry with combined areas
st_geometry(combo_areas[combo_areas$area_name == "Kouilou",]) <- p_noire_kouilou
st_geometry(combo_areas[combo_areas$area_name == "Brazzaville",]) <- pool_brazza

combo_areas$area_name[combo_areas$area_name == "Kouilou"] <- "Pointe-noire-Kouilou"
combo_areas$area_name[combo_areas$area_name == "Brazzaville"] <- "Pool-Brazzaville"

#' Check that regions merged correctly
ggplot() +
  geom_sf(data = combo_areas, aes(fill = area_name), colour = NA) +
  geom_sf(data = cog_long %>% filter(area_level == 2), colour = "black", fill = NA)


#' Add new combined region into area hiearchy as follows:
#' * Level 1: Combined regions (10)
#' * Level 2: Regions (12)
#' * Level 3: District (52)
#' Assign new parent area IDs to combined regions in level2 so that combined
#' regions nest into level2:
#' Pool (COG_2_11) - > Pool-Brazzaville (COG_1_2)
#' Pointe-noire (COG_2_10) - > Pointe-noire-Kouilou (COG_1_5)

cog <- cog_clean %>%
  mutate(area_id1 = str_replace_all(area_id1, "COG_1","COG_2"),
         area_id2 = str_replace_all(area_id2, "COG_2","COG_3")
         ) %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas() %>%
  filter(area_level > 0) %>%
  mutate(
    parent_area_id = case_when(
    area_level == 1 ~ str_replace(area_id, "COG_2", "COG_1"),
    TRUE ~ parent_area_id),
    parent_area_id = case_when(
    area_id == "COG_2_10" ~ "COG_1_5",
    area_id == "COG_2_11" ~ "COG_1_2",
    area_id == "COG_2_12" ~ "COG_1_10",
    TRUE ~ parent_area_id),
    area_level = area_level + 1) %>%
  rbind(combo_areas) %>%
  arrange(area_level, area_id)

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

cog_2021 <- cog %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

cog_2021 %>% group_by(area_level) %>%
  summarise(n = n())

#' Create boundaries file
cog_areas <- cog_2021 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Combined Region",
                  `2` = "Region",
                  `3` = "District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)

#' Save boundaries
sf::st_write(cog_areas, "cog_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(cog_areas)
ggsave("check/cog-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

while (!is.null(dev.list())) dev.off()
