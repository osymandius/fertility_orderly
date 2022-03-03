#' ## Uganda (UGA)
#' Source: Uganda Bureau of Statistics
#' Levels:
#'   * 1: Region (10)
#'   * 2: District (136)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural: Yes
#' PEPFAR PSNU: District (level 2)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(ubos2019 = "UGA/2019-11-09/uganda_districts_2019_i.zip",
             ubos2020 = "UGA/2020-10-13/Uganda_Districts_2020_UBOS_EPSG_4326.zip",
             uga_pepfar = "UGA/2019-07-10/Uganda_PROD_5_District_DistrictLsib_2019_Jul.zip",
             ug_lakes = "UGA/2020-04-27/Ug_lakes.zip",
             ug2021area_id = "UGA/2020-11-25/uga_2021_area_id.csv"
             ) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

ubos2019 <- read_sf_zip(files$ubos2019)
ubos2020 <- read_sf_zip(files$ubos2020)
uga_pepfar <- read_sf_zip(files$uga_pepfar)


#' The file uga_2021_area_id contains the area hierarchy and area_id to assign
#' corresponding to the 2020 UBOS shape file with one new district (136 total).
#' The new shape file did not include full region hierarchy, so obtaining that
#' from this file.
#' * Arua district split into Arua and Terego
#' * Former Arua area_id retired and two new area_id assigned for (updated) Arua
#'   boundaries and Terego boundaries

area_id2021 <- read_csv(files$ug2021area_id)

#' Comparison shape files
uga_pepfar <- st_make_valid(uga_pepfar)

uga_pepfar0 <- uga_pepfar %>%
  st_union() %>%
  ms_simplify(0.05) %>%
  st_make_valid()

ggplot(uga_pepfar0) +
  geom_sf()

#' # Uganda lakes shapefile
#' * Sent by Jotham Mubangizi on 27 April 2020; unknown source
ug_lakes <- read_sf_zip(files$ug_lakes)
uga_lakes <- st_transform(ug_lakes, 4326)

#' # Shape file cleaning
uga_simple <- ubos2020 %>%
  st_transform(4326) %>%
  rmapshaper::ms_simplify(0.05) %>%
  sf::st_make_valid()

uga_simple <- area_id2021 %>%
  full_join(uga_simple, by = c("LEVEL5" = "X2020")) %>%
  st_as_sf()

stopifnot(!is.na(uga_simple$area_id3))
stopifnot(!st_is_empty(uga_simple))

#' Get rid of fragments created by st_make_valid()
uga_simple <- st_collection_extract(uga_simple, "POLYGON")

#' Review boundary discrepancies
uga_simple %>%
  summarise() %>%
  ggplot() +
  geom_sf(fill = NA) +
  th_map()

#' Discrepancy is on the boundary of Kyotera (Rakai / Kyotera / Lwengo / Masaka)
uga_simple %>%
  filter(LEVEL3 == "SOUTH BUGANDA") %>%
  ggplot() +
  geom_sf() +
  geom_sf_label(aes(label = LEVEL5)) +
  th_map()

uga_simple %>%
  filter(LEVEL3 == "SOUTH BUGANDA") %>%
  summarise() %>%
  ggplot() +
  geom_sf() +
  th_map()

bad <- uga_simple %>%
  filter(LEVEL5 %in% c("RAKAI", "KYOTERA", "LWENGO", "MASAKA")) %>%
  st_transform(3857)

uga_clean <- uga_simple %>%
  st_transform(3857) %>%
  st_snap(bad %>% filter(LEVEL5 == "RAKAI"), 500) %>%
  st_snap(bad %>% filter(LEVEL5 == "KYOTERA"), 500) %>%
  st_snap(bad %>% filter(LEVEL5 == "LWENGO"), 100) %>%
  st_snap(bad %>% filter(LEVEL5 == "MASAKA"), 100) %>%
  st_transform(4326)

uga_clean %>%
  summarise() %>%
  ggplot() +
  geom_sf(fill = NA)

uga_clean %>%
  filter(LEVEL5 %in% c("KYOTERA", "RAKAI", "LWENGO")) %>%
  ggplot() +
  geom_sf(fill = NA)

#' Review cleaning:
#' * Red lines are original boundaries.
#' * Grey lines are boundaries after st_snap().
#' * Black specs are remaining slivers.
#' Visible red line segments are places where snapping has affected the boundaries.

uga_clean %>%
  filter(LEVEL5 %in% c("RAKAI", "KYOTERA", "LWENGO", "MASAKA")) %>%
  {
    ggplot() +
      geom_sf(data = bad, color = "red", fill = NA) +
      geom_sf(data = ., color = "grey", fill = NA) +
      geom_sf(data = summarise(.), color = "black", fill = NA)
  }


#' Remove lakes
uga_all_lakes <- st_combine(uga_lakes) %>%
  st_make_valid() %>%
  st_collection_extract() %>%
  ms_simplify(0.25)

uga_clipped <- st_difference(uga_clean, uga_all_lakes)


ggplot(uga_all_lakes) +
  geom_sf(color = "lightblue4", fill = "lightblue") +
  geom_sf(data = uga_clean, fill = NA, color = NA) +
  ggtitle("All lakes removed")

ggplot(uga_clipped) +
  geom_sf(fill = "grey70")


uga_clipped %>%
  st_union() %>%
  ggplot() +
  geom_sf(fill = "grey70")


#' Recode area hierarchy
uga_wide <- uga_clipped %>%
  mutate(
    spectrum_region_code = 0
  ) %>%
  select(area_id0,
         area_name0,
         area_id1,
         area_name1,
         area_id2,
         area_name2,
         area_id3,
         area_name3,
         spectrum_region_code) %>%
  rename_all(~sub("area\\_", "", .))


uga_long <- gather_areas(uga_wide)

uga_areas <- uga_long %>%
  arrange(area_level, area_name) %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "AIS region",
                  `2` = "Sub-region",
                  `3` = "District"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = area_level == 0,
         naomi_level = area_level == 3,
         pepfar_psnu_level = area_level == 3)


uga_area_hierarchy <- uga_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

uga_area_boundaries <- uga_areas %>%
  select(area_id, geometry)

uga_area_levels <- uga_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' Save boundaries

sf::st_write(uga_areas, "uga_areas.geojson", delete_dsn = TRUE)
sf::st_write(uga_area_boundaries, "uga_area_boundaries.geojson", delete_dsn = TRUE)
write_csv(uga_area_hierarchy, "uga_area_hierarchy.csv")
write_csv(uga_area_levels, "uga_area_levels.csv")


#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(uga_areas)

ggsave("uga_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)


masaka <- filter(uga_areas, area_name == "Masaka")$geometry
kalangala <- filter(uga_areas, area_name == "Kalangala")$geometry

bridgesf <- list(rbind(c(32.075, -0.252), c(32.075, -0.255), c(32.018, -0.255), c(32.018, -0.252), c(32.075, -0.252)))%>%
  st_polygon() %>%
  st_sfc(crs = 4326)

bridgesf <- st_difference(bridgesf, masaka)

kalangala_bridge <- st_union(bridgesf, kalangala)

ggplot() + geom_sf(data = masaka) + geom_sf(data = kalangala_bridge)

uga_areas_kalangala_bridge <- uga_areas
uga_areas_kalangala_bridge$geometry[uga_areas_kalangala_bridge$area_name == "Kalangala"] <- kalangala_bridge

sf::st_write(uga_areas_kalangala_bridge, "uga_areas_kalangala-masaka-bridge.geojson", delete_dsn = TRUE)


dev.off()

