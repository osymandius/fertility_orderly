#' ## Burundi(BDI)
#' Source: UNAIDS
#' Levels:
#'   * 1: Région (18)
#'   * 2: District Sanitaire  (48)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural:
#' PEPFAR PSNU:

sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)
dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(areas2021 = "BDI/2020-01-19/bdi_areas.geojson",
             districts2022 = "BDI/2022-01-13/District.geojson"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

areas2021 <- read_sf(files$areas2021) %>%
  mutate(
    area_level = as.integer(area_level),
    display = as.logical(display),
    area_name = sub("^DS ", "", area_name),
    area_name =   stringr::str_to_title(area_name)
  )

#' Update hierarchy with 2022 boundaries
wide2021 <- naomi::spread_areas(areas2021)

districts2022 <- read_sf(files$districts2022) %>%
  select(area_name1 = parentName,
         area_name2 = name) %>%
  mutate(area_name2 = sub("^DS ", "", area_name2))

# Update area name changes
setdiff(wide2021$area_name1, districts2022$area_name1)
setdiff(districts2022$area_name1, wide2021$area_name1)
setdiff(wide2021$area_name2, districts2022$area_name2)
setdiff(districts2022$area_name2, wide2021$area_name2)

wide2022 <- districts2022 %>%
  left_join(wide2021 %>% st_drop_geometry() %>%
              mutate(area_name1 = recode(area_name1,
                                         "Bujumbura Rural" = "Bujumbura ",
                                         "Karuzi" = "Karusi"),
                     area_name2 = recode(area_name2,
                                         "Bugarama" = "Bugarama ",
                                         "Bujumbura Centre" = "Bujumbura centre",
                                         "Bujumbura Nord" = "Bujumbura nord",
                                         "Bujumbura Sud" = "Bujumbura sud")))

# Add in updated areas IDs for district that has been split
wide2022$area_id0[wide2022$area_name2 == "Rutovu"] <- "BDI"
wide2022$area_name0[wide2022$area_name2 == "Rutovu"] <- "Burundi"
wide2022$area_id1[wide2022$area_name2 == "Rutovu"] <- "BDI_1_4"
wide2022$area_name1[wide2022$area_name2 == "Rutovu"] <- "Bururi"
wide2022$area_id2[wide2022$area_name2 == "Rutovu"] <- "BDI_2_48gx"
wide2022$area_id[wide2022$area_name2 == "Rutovu"] <- "BDI_2_48gx"
wide2022$area_id[wide2022$area_name2 == "Matana"] <- "BDI_2_10lj"
wide2022$area_id2[wide2022$area_name2 == "Matana"] <- "BDI_2_10lj"


# Summary of changes 2021 -> 2022
a <- ggplot() +
  geom_sf(data = wide2022 %>% filter(area_id1 == "BDI_1_4"),
          color = "black", fill = NA) +
  geom_sf_text(data = wide2022 %>% filter(area_id1 == "BDI_1_4"),
               aes(label = area_name2, geometry = geometry)) +
  ggtitle("Bururi: district boundaries 2022")

b <- ggplot() +
  geom_sf(data = wide2021 %>% filter(area_id1 == "BDI_1_4"),
          color = "black", fill = NA) +
  geom_sf_text(data = wide2021 %>% filter(area_id1 == "BDI_1_4"),
               aes(label = area_name2, geometry = geometry)) +
  ggtitle("Bururi: district boundaries 2021")

p_2022_boundary_changes <- cowplot::plot_grid(a, b, nrow = 1, align = "hv")
ggsave("check/bdi-2022-boundary-changes.pdf", p_2022_boundary_changes, h = 6, w = 12)

# Check for overlaps
plot(wide2022 %>% st_union())
plot(wide2022 %>% filter(area_id1 == "BDI_1_4") %>% st_union())
plot(st_difference(wide2022 %>% st_union(), wide2021 %>% st_union()))

# Fragments from overlapping boundaries for new district in Bururi
# Solution:
# * Clean + simplify boundaries
# * Get valid regional border from fragmented polygon
# * Create clean file for new district = valid regional polygon - 2022 boundaries
#    for new districts

bdi_simple <- wide2022 %>% ms_simplify(0.05)

plot(bdi_simple %>% st_union())
plot(bdi_simple%>% filter(area_id1 == "BDI_1_4") %>% st_union())

# Valid provincial border from fragmented polygon
burruri_region_simple <- bdi_simple %>%
  filter(area_id1 == "BDI_1_4") %>%
  st_union()

bururi_region_multi <- burruri_region_simple[[1]]

bururi_region <- bururi_region_multi[[1]] %>%
  as.data.frame() %>%
  st_as_sf(coords = c("V1", "V2"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

plot(bururi_region)

# Create clean file for new district =
#  valid regional polygon - 2022 boundaries for new districts
matana_bururi <- bdi_simple %>%
  filter(area_id %in% c("BDI_2_9", "BDI_2_10lj")) %>%
  st_union() %>%
  st_as_sf()

rutovu <- ms_erase(bururi_region, matana_bururi)

plot(rutovu$geometry)

#' Add coordinates into  shapefile
bdi_simple_df <- as.data.frame(bdi_simple)
bdi_simple_df[bdi_simple_df$area_id == "BDI_2_48gx",]$geometry <- rutovu$geometry
bdi <- st_as_sf(bdi_simple_df)

# Still overlaps between external borders of Bururi region
plot(bdi %>% st_union())

# Slightly better with simplified geometry
bdi <- bdi %>%
  ms_simplify(0.5)
plot(bdi %>% st_union())

# Format hierarchy and
bdi_areas <- bdi %>%
  rename_with(~str_remove(., 'area_')) %>%
  mutate(spectrum_region_code= 0L) %>%
  gather_areas() %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = recode(area_level,
                                   `0` = "Pays",
                                   `1` = "Région",
                                   `2` = "District Sanitaire"),
         area_sort_order = row_number(),
         display = TRUE)

pryr::object_size(bdi)
plot_area_hierarchy_summary(bdi_areas)

#' Save boundaries
sf::write_sf(bdi_areas, "bdi_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
p_bdi_areas <- plot_area_hierarchy_summary(bdi_areas)

ggsave("bdi_area_hierarchy.png", p_bdi_areas, h = 6, w = 12)
