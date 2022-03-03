#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw/"

#' New 113 district shape file
url_sh <- "CIV/2020-12-14/CI_DISTRICT_SANITAIRE 113.zip"
url_sh <- URLencode(file.path(naomi_raw_path, url_sh))
file_sh <- sharepoint$download(url_sh)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' Assigned area_id including Datim UID
#' * Area names sourced from Datim, accents are omitted
#'

url_area <- "CIV/2021-01-07/civ-area-id_2021.csv"
url_area <- URLencode(file.path(naomi_raw_path, url_area))
file_area <- sharepoint$download(url_area)

#' Shape file received from MOH team. Consistent with DHIS and PEPFAR shape file
sh <- read_sf_zip(file_sh)
sh <- st_transform(sh, 4326)

#' File with area name, Datim UID, NOM, and generated area_id
areas <- read_csv(file_area)

sh <- full_join(areas, sh, by = "NOM")
sh <- st_as_sf(sh)

stopifnot(nrow(sh) == nrow(areas))

#' Simplify shapefile
sh_simple <- ms_simplify(sh, 0.05)


p_simple <- bind_rows(mutate(sh, version = "original") ,
                        mutate(sh_simple, version = "simplified")) %>%
  ggplot(aes(color = version)) +
  geom_sf(fill = NA) +
  scale_color_brewer(palette = "Set1") +
  th_map()


p_simple_abidjan <- bind_rows(mutate(sh, version = "original") ,
                        mutate(sh_simple, version = "simplified")) %>%
  filter(grepl("Abidjan", area_name1)) %>%
  ggplot(aes(color = version)) +
  geom_sf(fill = NA) +
  scale_color_brewer(palette = "Set1") +
  th_map()

dir.create("check")

ggsave("check/check-simplified-boundaries.png", p_simple, h = 8, w = 8)
ggsave("check/check-simplified-boundaries_abidjan.png", p_simple_abidjan, h = 8, w = 8)


#' Clean shape file
#'
#' TODO: There are still some fragments in the shape file.

sh_clean <- sh_simple %>%
   st_transform(3857) %>%
   st_snap(., ., tolerance = 250) %>%
   sf::st_make_valid() %>%
   st_transform(4326)

ggplot(sh_simple) +
  geom_sf(size = 0.25, color = "grey") +
  geom_sf_label(aes(label = area_id2), size = 2, alpha = 0.3, label.size = NA) +
  geom_sf(data = st_union(sh_clean), fill = NA) +
  th_map()

p_clean <- bind_rows(mutate(sh_simple, version = "simplified") ,
                        mutate(sh_clean, version = "cleaned")) %>%
  ggplot(aes(color = version)) +
  geom_sf(fill = NA) +
  scale_color_brewer(palette = "Set1") +
  th_map()

p_clean_abidjan <- bind_rows(mutate(sh_simple, version = "simplified") ,
                             mutate(sh_clean, version = "cleaned")) %>%
  filter(grepl("Abidjan", area_name1)) %>%
  ggplot(aes(color = version)) +
  geom_sf(fill = NA) +
  scale_color_brewer(palette = "Set1") +
  th_map()

ggsave("check/check-simplified-boundaries.png", p_clean, h = 8, w = 8)
ggsave("check/check-simplified-boundaries_abidjan.png", p_clean_abidjan, h = 8, w = 8)


#' Create hierarchy dataset

civ_wide <- sh_clean %>%
  rename_all(~sub("area_", "", .)) %>%
  mutate(spectrum_region_code = 0L)

civ <- gather_areas(civ_wide)

civ_areas <- civ %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Pays",
                  `1` = "Region",
                  `2` = "District Sanitaire"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)



#' Save boundaries
sf::st_write(civ_areas,"civ_areas.geojson", delete_dsn = TRUE)

civ_area_hierarchy <- civ_areas %>%
  as.data.frame %>%
  select(-geometry)

write_csv(civ_area_hierarchy, "civ_area_hierarchy.csv", na = "")


#' Plot hierarchy

p_civ_areas <- plot_area_hierarchy_summary(civ_areas)

ggsave("civ_area_hierarchy.png", p_civ_areas, h = 6, w = 12)

dev.off()
