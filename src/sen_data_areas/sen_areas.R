#' ## Senegal (SEN)
#' Source:
#'   * 1: Region (14)
#'   * 2: District (76)
#' Spectrum: 0
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw79 = "SEN/2021-02-09/sen_shapefiles.zip",
             raw76 = "SEN/2021-02-01/sen_shapefiles.zip",
             hierarchy_map = "SEN/2021-02-09/Senegal%20regions%20and%20districts.xlsx",
             id_map = "SEN/2021-02-09/sen_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' Country team have provided two shape file:
#' (1) outdated file with 76 distrcits with internal boundary inconsistencies
#' (2) updated file with 79 distrcits with internal boundary inconsistencies +
#'     polygon fragments (?) that make cleaning diffcult
#' Solution: Carve additional districts in (2) out of (1) and clean - then
#' aggregate district level boundary file up to get a harmonised nested hierarchy file

#' Read in + clean 76 districts dataset
districts76 <- read_sf_zip(files$raw76, "ds_sn.*shp$") %>%
  select(area_name2 = NAME) %>%
  mutate(area_name2 = str_to_sentence(area_name2))

districts76_simple <- ms_simplify(districts76, keep = 0.5)

#' Get area heirarchy from 79 district map
raw79 <- read_sf_zip(files$raw79)
region_map <- readxl::read_excel(files$hierarchy_map, sheet = 1)

#' Read in + clean 76 districts dataset
districts79 <- raw79 %>% left_join(region_map %>% select(NAME, REGION)) %>%
  select(area_name2 = NAME, area_name1 = REGION) %>%
  mutate(region_rank = dense_rank(area_name1),
         area_id1 = paste0("SEN_1_", str_pad(region_rank, 2, pad = "0"))) %>%
  arrange(area_id1, area_name2) %>%
  mutate(area_id2 = paste0("SEN_2_", str_pad(row_number(), 2, pad = "0")),
         area_name1 = str_to_sentence(area_name1),
         area_name2 = str_to_sentence(area_name2)) %>%
  st_transform(4326)

### Carve out new areas from old areas
### Ndiob -> Fatick + Diakaho
ndiob <- filter(districts76_simple, area_name2 == "Ndiob")$geometry
fatick <- filter(districts79, area_name2 == "Fatick")$geometry

diakhao_clean <- st_difference(ndiob, fatick)
fatick_clean <- st_difference(ndiob, diakhao_clean)

ggplot() +
  geom_sf(data = fatick_clean, fill = "seagreen") +
  geom_sf(data = diakhao_clean, fill = "lightseagreen") +
  geom_sf(data = ndiob, fill = NA, colour = "white") +
  geom_sf(data = fatick, fill = NA, colour = "black")

### Rufisque -> Rufisque + Sangalkam
rufisque <- filter(districts76_simple, area_name2 == "Rufisque")$geometry
sangalkam <- filter(districts79, area_name2 == "Sangalkam")$geometry

rufisque_clean <- st_difference(rufisque, sangalkam)
sangalkam_clean <- st_difference(rufisque, rufisque_clean)

ggplot() +
  geom_sf(data = rufisque_clean, fill = "seagreen") +
  geom_sf(data = sangalkam_clean, fill = "lightseagreen") +
  geom_sf(data = rufisque, fill = NA, colour = "white")

### Rufisque -> Rufisque + Sangalkam
keur_massar76 <- filter(districts76_simple, area_name2 == "Keur-massar")$geometry
keur_massar79 <- filter(districts79, area_name2 == "Keur-massar")$geometry

yeumbeul_clean <- st_difference(keur_massar76, keur_massar79)
keur_massar_clean <- st_difference(keur_massar76, yeumbeul_clean)

ggplot() +
  geom_sf(data = keur_massar_clean, fill = "seagreen") +
  geom_sf(data = yeumbeul_clean, fill = "lightseagreen") +
  geom_sf(data = keur_massar76, fill = NA, colour = "white")


# Replace old geometries with new
geoms <- districts79 %>%
  filter(area_name2 %in% c("Fatick", "Diakhao", "Rufisque","Sangalkam",
                           "Keur-massar", "Yeumbeul"))

geoms$geometry[geoms$area_name2 == "Fatick"] <- fatick_clean
geoms$geometry[geoms$area_name2 == "Diakhao"] <- diakhao_clean
geoms$geometry[geoms$area_name2 == "Rufisque"] <- rufisque_clean
geoms$geometry[geoms$area_name2 == "Sangalkam"] <- sangalkam_clean
geoms$geometry[geoms$area_name2 == "Keur-massar"] <- keur_massar_clean
geoms$geometry[geoms$area_name2 == "Yeumbeul"] <- yeumbeul_clean

# Check for differences in area names
setdiff(districts76$area_name2, districts79$area_name2)
setdiff(districts79$area_name2, districts76$area_name2)

# Merge new areas into old dataset: removed orginal areas + add in subdivided  areas
districts79_clean <- districts76 %>% filter(!area_name2 %in%
                                              c("Ndiob", "Rufisque", "Keur-massar")) %>%
  mutate(area_name2 = recode(area_name2,
                             "Koupentoum" = "Koumpentoum",
                             "Colobane" = "Gossass")) %>%
  left_join(districts79 %>% st_drop_geometry()) %>%
  rbind(st_as_sf(geoms)) %>%
  ms_simplify(., keep = 0.4) %>%
  st_transform(3857) %>%
  st_snap(., ., tolerance = 200) %>%
  sf::st_make_valid() %>%
  st_transform(4326)

ggplot() +
  geom_sf(data = districts79_clean%>% filter(area_name1 == "Dakar"), fill = NA)

p_compare <- compare_boundaries(districts79_clean, districts79)

ggsave("check/sen-compare-boudnaries.png", p_compare, h = 6, w = 12)

#' Aggregate district dataset to national and regional levels
sen_national <- districts79_clean %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(area_name = "Senegal", area_id = "SEN",
         area_level = 0, parent_area_id = "")

ggplot() + geom_sf(data = sen_national)

sen_regions <- districts79_clean %>%
  group_by(area_id1, area_name1) %>%
  summarise(geometry = st_union(geometry))%>%
  ungroup() %>%
  mutate(area_level = 1, parent_area_id = "SEN") %>%
  rename(area_id = area_id1, area_name = area_name1)

ggplot() + geom_sf(data = sen_regions, aes(fill = area_name))

sen_districts <- districts79_clean %>%
  select(area_name = area_name2, area_id = area_id2,
         parent_area_id = area_id1) %>%
  mutate(area_level = 2)

sen_long <- rbind(sen_national, sen_regions, sen_districts) %>%
  mutate(spectrum_region_code = 0L) %>%
  arrange(area_id)

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

sen_2021 <- sen_long %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

sen_2021 %>% group_by(area_level) %>%
  summarise(n = n())


#' Create boundaries file
sen_areas <- sen_2021 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry) %>%
  st_collection_extract(., "POLYGON")

#' Save boundaries
sf::st_write(sen_areas, "sen_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(sen_areas)
ggsave("check/sen-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

while (!is.null(dev.list())) dev.off()
