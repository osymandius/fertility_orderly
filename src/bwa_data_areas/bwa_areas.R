#' ## Botswana (BWA)
#' Source:
#'   * 1: District (10)
#'   * 2: New Health District (18)
#'   * 3: Old Health District (27)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(pepfar_raw = "BWA/2021-01-14/PEPFAR/bwa_areas.geojson",
             dhis_raw = "BWA/2021-01-14/DHIS/areas.geojson",
             id_map = "BWA/2021-01-27/bwa_area_id_map_2021_v2021-01-27.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' Read in files from Sharepoint
dhis_raw <- read_sf(files$dhis_raw)
pepfar_raw <- read_sf(files$pepfar_raw)


#' PEPFAR facilities

datim_facilities_path <- paste0("sites/HIVInferenceGroup-WP/Shared%20Documents/",
                                "Data/PEPFAR/datim_org_hierarchy/archive/parse_hierarchy/20210115-224456-b0dfc850/datim_org_hierarchy.csv")
datim_geocoord_path <- paste0("sites/HIVInferenceGroup-WP/Shared%20Documents/",
                              "Data/PEPFAR/datim_org_hierarchy/archive/parse_hierarchy/20210115-224456-b0dfc850/datim_org_geometry.geojson")

datim_facilities_file <- sharepoint$download(datim_facilities_path)
datim_geocoord_file <- sharepoint$download(datim_geocoord_path)

fac <- read_csv(datim_facilities_file)
geo <- read_sf(datim_geocoord_file)

bwa_uid <- fac %>% filter(name == "Botswana") %>% .$id

bwa_fac <- filter(fac, level == 6, grepl(bwa_uid, path)) %>%
  left_join(
    select(fac, district_id = id, district_name = name),
    by = c("parent" = "district_id")
  ) %>%
  left_join(geo) %>%
  st_as_sf() %>%
  transmute(
    id,
    name,
    district = sub(" District", "", district_name)
  )


#' Census district boundaries

census_boundaries_path <- paste0("sites/HIVInferenceGroup-WP/Shared%20Documents/",
                                 "Data/naomi-raw/BWA/2021-01-07/Census_district_boundaries.zip")
census_boundaries_file <- sharepoint$download(census_boundaries_path)

tmpf <- tempfile()
unzip(census_boundaries_file, exdir = tmpf)
census <- read_sf(file.path(tmpf, "Census_district_boundaries.shp"))


#' Identify overlapping districts
dhis_raw %>% filter(area_level == 1) %>%
  mutate(
    is_covered = rowSums(st_covered_by(., sparse = FALSE)) > 1,
    ) %>%
  filter(is_covered == TRUE) %>%
  print(Inf)

dhis_raw %>%
  filter(area_level == 1) %>%
  mutate(
    is_covered = rowSums(st_covered_by(., sparse = FALSE)) > 1
  ) %>%
  ggplot() +
  geom_sf(aes(fill = is_covered), alpha = 0.5) +
  scale_fill_manual(values = c("FALSE" = "grey", "TRUE" = "red")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())


#' Simlify boundaries
bwa_simple <- dhis_raw %>%
  ms_simplify(0.1) %>%
  st_union(by_feature = TRUE) %>%
  st_as_sf()

gridExtra::grid.arrange(check_boundaries(dhis_raw, bwa_simple))

check_boundaries(
  filter(dhis_raw, area_name %in% c("North East", "Francistown", "Selibe-Phikwe", "Bobirwa")),
  filter(bwa_simple, area_name %in% c("North East", "Francistown", "Selibe-Phikwe", "Bobirwa"))
) %>%
  gridExtra::grid.arrange()

se_dist <- c("Gaborone", "Kweneng East", "Kgatleng", "Good Hope", "Lobatse", "Moshupa",
             "South East", "Kanye", "Jwaneng", "Southern", "Goodhope")


check_boundaries(
  filter(dhis_raw, area_name %in% se_dist),
  filter(bwa_simple, area_name %in% se_dist)
) %>%
  grid.arrange()

bwa_gantsi_charleshill <- bwa_simple %>%
  filter(area_name %in% c("Gantsi", "Charleshill")) %>%
  st_transform(3857) %>%
  st_snap(., ., tolerance = 1700) %>%
  sf::st_make_valid() %>%
  st_transform(4326)

ggplot() +
  geom_sf(data = filter(dhis_raw, area_name %in% c("Gantsi", "Charleshill")), color = "red") +
  geom_sf(data = bwa_gantsi_charleshill, fill = NA)

#' ## South East district is incomplete vs. national boundaries in the DHIS2 file.
#'

moshupa_dist <- c("Kweneng East", "Moshupa", "Kanye", "Jwaneng", "Southern", "Mabutsane", "Ngwaketse West")


p_pepfar <- pepfar_raw %>%
  filter(area_name %in% moshupa_dist,
         area_level == 3) %>%
  ggplot() +
  geom_sf(aes(fill = area_name), alpha = 0.3) +
  geom_sf_label(aes(label = area_name), label.size = NA, alpha = 0.3, size = 2) +
  labs(title = "Boundaries from PEPFAR") +
  theme_minimal(9) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


p_dhis <- dhis_raw %>%
  filter(area_name %in% moshupa_dist) %>%
  ggplot() +
  geom_sf(aes(fill = area_name), alpha = 0.7) +
  geom_sf_label(aes(label = area_name), label.size = NA, alpha = 0.3, size = 2) +
  ## geom_sf(
  ##   aes(color = district),
  ##   data = filter(bwa_fac, district %in% se_dist)
  ## ) +
  labs(title = "DHIS2 boundaries") +
  theme_minimal(9) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


p_census <- census %>%
  filter(def %in% c("Southern", "Kweneng East", "Jwaneng", "Ngwaketse West")) %>%
  ggplot() +
  geom_sf(aes(fill = def), alpha = 0.5) +
  geom_sf_label(aes(label = def),
                label.size = NA, alpha = 0.3, size = 2) +
  labs(title = "Census districts") +
  theme_minimal(9) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())


p_compare_moshupa <- gridExtra::grid.arrange(p_dhis, p_pepfar, p_census, nrow = 1)

dir.create("check")
ggsave(filename = "check/moshupa-boundaries.pdf", p_compare_moshupa, h = 4, w = 10)

south_east_diff <- filter(bwa_simple, area_level == 0) %>%
  st_difference(
    bwa_simple %>%
    filter(area_level == 1,
           area_name != "South East") %>%
    st_union()
  )

#' Sort by area and keep the largest components

south_east_spl <- south_east_diff %>%
  st_cast("POLYGON") %>%
  mutate(area = st_area(.)) %>%
  arrange(-area)

round(south_east_spl$area / 1e6)

south_east_spl %>%
  mutate(keep = row_number() <= 2) %>%
  ggplot(aes(color = keep, fill = keep)) +
  geom_sf()

south_east_new <- st_union(south_east_spl$geometry[1:2])

ggplot(south_east_new) +
  geom_sf()


#' ## Update 27 January 2021:
#'
#' After discussing with team, confident that Moshupa is South of
#' Kweneng east. Update the boundaries to use the PEPFAR boundaries
#' for Moshupa, Jwaneng, and Mabutsane.
#'
#' ACTION: update those three districts using PEPFAR boundaries
#'

## #' Moshupa district needs to be removed from Kweneng East
## #'
## #' However, southern boundary of Moshupa does not align with
## #' Kweneng East exactly.
## #'

## bwa_simple %>%
##   filter(area_name %in% c("Kweneng East", "Moshupa")) %>%
##   ggplot() +
##   geom_sf(aes(color = area_name), fill = NA)

## #' Solution: move SE corner of Moshupa south.

## moshupa <- filter(bwa_simple, area_name == "Moshupa")
## moshupa_pt <- st_coordinates(moshupa)
## moshupa_new_pt <- moshupa_pt
## moshupa_new_pt[moshupa_new_pt[ , "Y"] == -24.7513, c("X", "Y")] <- c(25.62, -24.77)
## moshupa_new_pt[1, c("X", "Y")] <- c(25.425, -24.7)
## moshupa_new_pt[8, c("X", "Y")] <- c(25.425, -24.7)


## ggplot(mapping = aes(X, Y)) +
##   geom_sf(data = filter(bwa_simple, area_name == "Kweneng East"), inherit.aes = FALSE) +
##   geom_point(data = as.data.frame(moshupa_pt) %>% mutate(rn = 1:8), color = "black") +
##   geom_point(data = as.data.frame(moshupa_new_pt), color = "red")

## moshupa_new <- st_linestring(moshupa_new_pt) %>%
##   st_zm() %>%
##   st_polygonize() %>%
##   st_sfc(crs = st_crs(bwa_simple)) %>%
##   st_collection_extract("POLYGON")

## moshupa_new3857 <- st_transform(moshupa_new, 3857)

## kweneng_east3857 <- filter(bwa_simple, area_name == "Kweneng East") %>%
##   st_transform(3857)

## kweneng_east_new <- st_difference(kweneng_east3857, moshupa_new3857) %>%
##   st_transform(4326)

## jwaneng3857 <- filter(bwa_simple, area_name == "Jwaneng") %>%
##   st_transform(3857)

## moshupa_new <- st_difference(moshupa_new3857, jwaneng3857) %>%
##   st_transform(4326)

## ggplot() +
##   geom_sf(data = st_difference(jwaneng3857, moshupa_new3857)) +
##   geom_sf(data = moshupa_new3857, fill = NA, color = "red")


## plot(st_union(filter(bwa_simple, area_name == "Jwaneng"), moshupa_new)$geometry)
## plot(st_union(kweneng_east_new, moshupa_new)$geometry)
## plot(kweneng_east_new$geometry)


## ggplot() +
##   geom_sf(data = filter(dhis_raw, area_name %in% c("Kweneng East", "Moshupa")), color = "red") +
##   geom_sf(data = moshupa_new, fill = NA) +
##   geom_sf(data = kweneng_east_new, fill = NA, color = "blue")


pepfar_southern <- pepfar_raw %>%
  filter(area_level == 3,
         area_name %in% c("Mabutsane", "Jwaneng", "Moshupa", "Southern"))

dhis_southern <- bwa_simple %>%
  filter(area_name %in% c("Mabutsane", "Jwaneng", "Kanye"))

ggplot() +
  geom_sf(data = pepfar_southern) +
  geom_sf(data = dhis_southern, fill = NA, color = "red")

mabutsane_new <- st_difference(st_union(dhis_southern),
                               st_union(filter(pepfar_southern, area_name != "Mabutsane")))

#' Multipolygon with the desired region and a few slivers; Keep the largest area which
#' is the desired region
#'
mabutsane_new <- mabutsane_new %>%
  st_cast("POLYGON")
mabutsane_new <- mabutsane_new[which.max(st_area(mabutsane_new))]


moshupa_new <- st_difference(st_union(dhis_southern),
                               st_union(filter(pepfar_southern, area_name != "Moshupa")))
moshupa_new <- moshupa_new %>%
  st_cast("POLYGON")
moshupa_new <- moshupa_new[which.max(st_area(moshupa_new))]

#' There's a little sliver left here. Deal with it later
plot(moshupa_new)

jwaneng_new <- st_difference(filter(dhis_southern, area_name == "Jwaneng"), mabutsane_new)
jwaneng_new <- st_difference(jwaneng_new, moshupa_new)

kanye_new <- st_difference(filter(dhis_southern, area_name == "Kanye"), mabutsane_new)
kanye_new <- st_difference(kanye_new, moshupa_new)

mabutsane_new %>%
  st_union(jwaneng_new) %>%
  st_union(moshupa_new) %>%
  st_union(kanye_new) %>%
  ggplot() +
  geom_sf()



#' Solution: Remove overlapping areas from nested districts
gantsi <- st_difference(filter(bwa_gantsi_charleshill, area_name == "Gantsi")$geometry,
                        filter(bwa_gantsi_charleshill, area_name == "Charleshill")$geometry)

#' Split to polygons and remove fragment
gantsi <- st_cast(gantsi, "POLYGON")

st_area(gantsi)/1e6

gantsi %>%
  st_as_sf() %>%
  mutate(rn = factor(row_number())) %>%
  ggplot() +
  geom_sf(aes(color = rn))


bobirwa <- st_difference(filter(bwa_simple, area_name == "Bobirwa")$geometry,
                         filter(bwa_simple, area_name == "Selibe-Phikwe")$geometry)

ggplot(bobirwa) +
  geom_sf()

plot(gantsi)
plot(bobirwa)

 #' Add summed coordinates into simple shapefile

bwa_simple$geometry[bwa_simple$area_name == "South East"] <- south_east_new
bwa_simple$geometry[bwa_simple$area_name == "Gantsi"] <- gantsi[1]
bwa_simple$geometry[bwa_simple$area_name == "Charleshill"] <- bwa_gantsi_charleshill$geometry[bwa_gantsi_charleshill$area_name == "Charleshill"]
bwa_simple$geometry[bwa_simple$area_name == "Bobirwa"] <- bobirwa[1]
bwa_simple$geometry[bwa_simple$area_name == "Moshupa"] <- moshupa_new
bwa_simple$geometry[bwa_simple$area_name == "Jwaneng"] <- jwaneng_new$geometry
bwa_simple$geometry[bwa_simple$area_name == "Mabutsane"] <- mabutsane_new
bwa_simple$geometry[bwa_simple$area_name == "Kanye"] <- kanye_new$geometry



bwa_snap <- bwa_simple %>%
  filter(area_level == 1) %>%
  st_transform(3857) %>%
  st_snap(
    filter(., area_name %in% c("Boteti", "Serowe", "Gantsi", "Ngamiland",
                               "Palapye", "Tutume", "Mahalpye", "Bobirwa", "Tutume")),
    tolerance = 1000
  ) %>%
  st_transform(4326)

count(bwa_snap) %>%
  ggplot() +
  geom_sf()

p <- ggplot() +
  geom_sf(data = filter(bwa_simple, area_level == 1), color = "red")

ggplot() +
  geom_sf(data = bwa_snap, fill = NA) +
  geom_sf_label(aes(label = area_name), data = bwa_snap, size = 2, label.size = NA, alpha = 0.3)



#' Format PEPFAR area heirarchy to merge with 27 district names from DHIS
#' Changes made:
#' * Match spelling to DHIS names
#' * One mismatch distrcit: Southern (PEPFAR) -> Kanya (DHIS)

pepfar_wide <- pepfar_raw %>%
  mutate(
    area_name = area_name %>%
      recode("Greater Selibe Phikw" = "Greater Selibe Phikwe",
             "Boteti_Rakops" = "Boteti-Rakops",
             "Serowe _Palapye" = "Serowe-Palapye",
             "Tutume_Gweta" = "Tutume-Gweta")
  ) %>%
  spread_areas() %>%
  st_drop_geometry() %>%
  mutate(area_name3 = recode(area_name3,
                           "Selibe Phikwe" = "Selibe-Phikwe",
                           "Goodhope" = "Good Hope",
                           "Southern" = "Kanye"))

bwa_areas <- full_join(pepfar_wide,
                       bwa_snap %>%
                       filter(area_level == 1) %>%
                       select(area_name3 = area_name)) %>%
  st_as_sf() %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()


#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

bwa_areas <- bwa_areas %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

bwa_areas %>%
  group_by(area_level) %>%
  summarise(n = n())


#' Create boundaries file
bwa_areas <- bwa_areas %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "District",
                  `2` = "New Health District",
                  `3` = "Health District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)

while (!is.null(dev.list())) dev.off()

#' Check non-overlapping: all levels have same area
level_areas <- st_area(count(bwa_areas, area_level))
stopifnot(level_areas == level_areas[1])

#' Save boundaries
st_write(bwa_areas, "bwa_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(bwa_areas)

ggsave("check/bwa_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)


