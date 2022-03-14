#' ## Ethiopia
#' Source: PEPFAR
#' Levels:
#'   * 1: Region (13)
#'   * 2: Zone (161)
#' PEPFAR PSNU: Woreda (level 3)
#' Spectrum: Region (level 1)
#' EPP: Region (level 1)
#' EPP Urban/Rural: Yes

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' ## Read in files from SharePoint

#' 2020 boundaries (124 zones)
path2020 <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/ETH/2020-01-19/Ethiopia_PROD_5_Zone_ZoneLsib_2019_Dec.zip"
path2020 <- URLencode(path2020)
file2020 <- sharepoint$download(path2020)
eth2020 <- read_sf_zip(file2020)

#' 2021 boundaries (160 zones)
path2021 <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/ETH/2020-12-15/RegionZoneWoredaShapefiles15Oct2020.zip"
path2021 <- URLencode(path2021)
file2021 <- sharepoint$download(path2021)
tmpd2021 <- tempfile()
unzip(file2021, exdir = tmpd2021)

woreda2021 <- file.path(tmpd2021, "RegionZoneWoredaShapefiles15Oct2020", "Woreda15Oct2020") %>%
  list.files("\\.shp", full.names = TRUE) %>%
  read_sf()

zone2021 <- file.path(tmpd2021, "RegionZoneWoredaShapefiles15Oct2020", "Zone15Oct2020") %>%
  list.files("\\.shp", full.names = TRUE) %>%
  read_sf()

all(st_is_valid(eth2020))
all(st_is_valid(zone2021))

#' 2022 boundaries (161 zones)
#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(region2022 = "ETH/2022-01-04/Ethiopia_PROD_4_Region_RegionLsib_2021_Dec.zip",
             zone2022 = "ETH/2022-01-04/Ethiopia_PROD_5_Zone_ZoneLsib_2021_Dec.zip",
             woreda2022 = "ETH/2022-01-04/Ethiopia_PROD_6_Woreda_WoredaLsib_2021_Dec.zip",
             idmap_2021 = "ETH/2020-12-16/eth-area-id-2021.csv",
             idmap_2022 = "ETH/2022-01-12/eth-area-id-2022.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

region2022 <- read_sf_zip(files$region2022)
zone2022 <- read_sf_zip(files$zone2022)
woreda2022 <- read_sf_zip(files$woreda2022)


all(st_is_valid(woreda2022))
all(st_is_valid(zone2022))

#' Area_id mapping files
idmap_2021 <- read_csv(files$idmap_2021)
idmap_2022 <- read_csv(files$idmap_2022)

# Historical code for generating 2021 boundary file
#' # Compare shapefiles and assign new zones
cmp2020 <- rename_all(eth2020, paste0, "_2020") %>%
  mutate(area_2020 = st_area(.))

cmp2021 <-   rename_all(zone2021, tolower) %>%
  rename_all(paste0, "_2021") %>%
  mutate(area_2021 = st_area(.))

cmp <- st_intersection(cmp2020, cmp2021)

cmp <- cmp %>%
  rename(geometry_intersect = geometry_2020) %>%
  mutate(area_intersect = st_area(.)) %>%
  left_join(
    select(cmp2020, level4name_2020, level5name_2020, geometry_2020) %>%
    as.data.frame()
  ) %>%
  left_join(
    select(cmp2021, level4name_2021, level5name_2021, geometry_2021) %>%
    as.data.frame()
  )


cmp2 <- select(cmp, level4name_2020, level4name_2021,
               level5name_2020, level5name_2021,
               area_2020, area_2021, area_intersect,
               starts_with("geometry"))

#' Filter out slivers from intersection
min(cmp2$area_2020)
min(cmp2$area_2021)

ggplot(cmp2, aes(x = as.numeric(area_intersect))) +
  geom_histogram() +
  scale_x_log10(breaks = 10^(-5:8), labels = paste0(10, "^", -5:8))

#' * The smallest zone is 3 million m^2.
#' * There's a clear break at just above 1000m^2
#' * Two in the range of 10,000 - 100,000 that should
#'   look at to confirm if a division.

#' ACTION: Drop intersections smaller than 5000 from further consideration

cmp2 <- filter(cmp2, as.numeric(area_intersect) > 5000)

#' New zones split over multiple previous zones:
cmp2 %>%
  group_by(level4name_2021, level5name_2021) %>%
  filter(n() > 1)

check_zone_overlap <- cmp2 %>%
  group_by(level4name_2021, level5name_2021) %>%
  filter(n() > 1)

p_check_zone_overlap <- check_zone_overlap %>%
  split(.$level5name_2021) %>%
  lapply(function(x) {
    ggplot(x) +
      geom_sf(aes(geometry = geometry_2021)) +
      geom_sf(aes(geometry = geometry_2021), fill = "blue3", alpha = 0.3, color = NA) +
      geom_sf(aes(geometry = geometry_2020), fill = "darkred", alpha = 0.3) +
      facet_wrap(~paste0("2020 Zone:" , level5name_2020, " (red)") +
                   paste0("2021 Zone: ", level5name_2021, " (blue)")) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank())
  })

pdf("check/zone-2021-non-nested-overlap.pdf", h = 3, w = 6)
p_check_zone_overlap

#' From reviewing overlaps:
#' * Konso overlap with Dereashe Sp. Zone is a sliver
#' * Bale overlap with Arsi is a sliver
#' * {South Eastern Tigray, South Tigray}: Boundary change -- both receive new area_id
#' * {Ale Sp. Zone, Segan Area People's}: Segan Area People's becomes Konso, and boundary
#'   increase to include part of of Ale Sp. Zone.
#'   * Both Ale Sp. Zone and Segan Area People's receive new area_id

cmp <- filter(cmp, as.numeric(area_intersect) > 5e4)


#' # Simplify and clean boundaries

pryr::object_size(zone2021)

eth_simple <- ms_simplify(zone2021, keep = 0.15)

stopifnot(st_is_valid(eth_simple))
stopifnot(st_geometry_type(eth_simple) %in% c("POLYGON", "MULTIPOLYGON"))


p_check_boundary <- check_boundaries(zone2021, eth_simple)
ggsave("check/eth-clean-shapefile-comparison.pdf", p_check_boundary, h = 6, w = 12)


spectrum_region_code <- c(
  "Addis Ababa" = 10,
  "Afar" = 11,
  "Amhara" = 12,
  "Benishangul-Gumuz" = 13,
  "Dire Dawa" = 14,
  "Gambella" = 15,
  "Harari" = 16,
  "Oromia" = 17,
  "SNNPR" = 18,
  "South West Ethiopia" = 18,
  "Somali" = 19,
  "Tigray" = 20,
  "Sidama" = 21

)

#' Recode area hierarchy
eth_wide2021 <- idmap_2021 %>%
  left_join(
    select(eth_simple, level5uid),
    by = c("level5uid_2021" = "level5uid")
  ) %>%
  st_as_sf() %>%
  select(
    name0 = level3name_2021,
    id0 = area_id0_2021,
    name1 = level4name_2021,
    id1 = area_id1_2021,
    uid1 = level4uid_2021,
    name2 = level5name_2021,
    id2 = area_id2_2021,
    uid2 = level5uid_2021
  ) %>%
  mutate(
    spectrum_region_code = recode(name1, !!!spectrum_region_code)
  ) %>%
  arrange(spectrum_region_code, name2)

eth_long2021 <- gather_areas(eth_wide2021)

# Compare changes between 2021 -> 2020 boundaries
#' Summary of changes:
#' * SNNPR: SNNPR -> SNNPR + South West (no zone boundary changes)
#' * Addis Abba: Addition of new zone (Lemi Kura) + change in boundary for
#'   Addis Ketema and neighbouring zones

#  SNNPR -> SNNPR + South West
a <- ggplot() +
  geom_sf(data = region2022, fill = NA, color = "red") +
  geom_sf(data = eth_long2021 %>% filter(area_level == 1), fill = NA) +
  theme_map() +
  ggtitle("Ethiopia: Region boundaries 2021 (black) vs. 2022 (red)",
          subtitle = "Addition of new region: NNPR -> SNNPR + South West") +
  theme(title = element_text(size = 8))

# SNNPR -> SNNPR + South West
# Boundaries for zones in new South West region are unchanged
b <- ggplot() +
  geom_sf(data = zone2022 %>% filter(level4name %in% c("SNNPR", "South West Ethiopia")),
          fill = NA, color = "red") +
  geom_sf(data = eth_long2021 %>% filter(parent_area_id == "ETH_1_22pj"), fill = NA) +
  theme_map() +
  ggtitle("SNNPR/ South West: Zone boundaries 2021 (black) vs. 2022 (red)",
          subtitle = "Boundaries for zones in new South West region are unchanged") +
  theme(title = element_text(size = 8))

plot_snnpr_sw_changes <- cowplot::plot_grid(a, b, nrow = 1, align = "hv")

#' Addis Abba:
#' - Addition of new zone (Lemi Kura)
#' - Change in boundary for Addis Ketema and neighbouring zones
plot_addis_changes <- ggplot() +
  geom_sf(data = zone2022 %>% filter(level4name == "Addis Ababa"),
          fill = NA, color = "red") +
  geom_sf(data = zone2022 %>% filter(level5name == "Lemi Kura"),
          fill = "red", color = "red") +
  geom_sf(data = eth_long2021 %>% filter(parent_area_id == "ETH_1_10"), fill = NA) +
  theme_map() +
  ggtitle("Addis Abbaba: Zone boundaries 2021 (black) vs. 2022 (red)",
          subtitle = "New zone (Lemi Kura) filled in red") +
  theme(title = element_text(size = 8))

pdf("check/eth-2022-boundary-changes.pdf", h = 6, w = 6)
plot_snnpr_sw_changes
plot_addis_changes



#' Format 2022 hierarchy with new area_ids
#' Update area_id for:
#' - New zones
#' - Zones with boundary changes
idmap_2022 %>% filter(id2_change == TRUE)

eth_wide2022 <- idmap_2022 %>%
  select(name1,
         id1 = id1_2022,
         name2,
         id2 = id2_2022) %>%
  left_join(zone2022 %>%
              select(name1 = level4name,
                     uid1 = level4uid,
                     name2 = level5name,
                     uid2 = level5uid)) %>%
  st_as_sf() %>%
  mutate(id0 = "ETH",
         name0 = "Ethiopia") %>%
  mutate(
    spectrum_region_code = recode(name1, !!!spectrum_region_code)
  ) %>%
  arrange(spectrum_region_code, name2) %>%
  select(name0, id0, name1, id1, uid1, name2, id2, uid2, spectrum_region_code)

eth_long2022 <- gather_areas(eth_wide2022)

eth_areas2022 <- eth_long2022 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country", `1` = "Region", `2` = "Zone"),
         display = TRUE,
         spectrum_level = area_level == 1,
         epp_level = area_level == NA,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 2)

#' Plot hierarchy
plot_area_hierarchy_summary(eth_areas2022)
# Overlapping zone boundaries -> fragments in regional + national boundaries

eth_clean <- eth_areas2022 %>%
  ms_simplify(keep = 0.15) %>%
  st_union(by_feature = TRUE) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON")

st_is_valid(eth_clean)

plot_area_hierarchy_summary(eth_clean)


#' Save boundaries

sf::st_write(eth_clean, "eth_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(eth_clean)
ggsave("eth_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)



#' Datim area map

area_map_datim <- eth_wide2022 %>%
  st_drop_geometry() %>%
  {bind_rows(
     transmute(., area_id = id1, map_level = 4, map_name = name1, map_id = uid1),
     transmute(., area_id = id2, map_level = 5, map_name = name2, map_id = uid2)
   )} %>%
  distinct() %>%
  mutate(map_source = "Datim")

write_csv(area_map_datim, "eth_area_map_datim_2022.csv", na = "")

while (!is.null(dev.list())) dev.off()
