#' ## Benin (BEN)
#' Source:
#'   * 1: Department (12)
#'   * 2: District (34)
#'   * 3: Commune (82)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "BEN/2020-12-14/ben_areas.geojson",
             id_map = "BEN/2020-12-14/ben_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw <- read_sf(files$raw)
raw$area_level <- as.integer(raw$area_level)

#' Simplify boundaries to reduce file size (if > 1Mb)

pryr::object_size(raw)

#' Replace old area IDs with 2021 area IDs

id_map <- read_csv(files$id_map)

raw_2021 <- raw %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

raw_2021 %>%
  group_by(area_level, area_level_label) %>%
  summarise(n = n())

#' Add Spectrum region code

spectrum_region_code <- c("Alibori" = 10,
                          "Atacora" = 11,
                          "Atlantique" = 12,
                          "Borgou" = 13,
                          "Collines" = 14,
                          "Couffo" = 15,
                          "Donga" = 16,
                          "Littoral" = 17,
                          "Mono" = 18,
                          "Oueme" = 19,
                          "Plateau" = 20,
                          "Zou" = 21)

areas_wide <- raw_2021 %>%
  spread_areas() %>%
  rename_all(~sub("area_", "", .)) %>%
  mutate(
    spectrum_region_code = recode(name1, !!!spectrum_region_code)
  )

stopifnot(!is.na(areas_wide$spectrum_region_code))

#' Create boundaries file
ben_areas <- areas_wide %>%
  gather_areas() %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Department",
                  `2` = "District",
                  `3` = "Commune"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(ben_areas, "ben_areas.geojson", delete_dsn = TRUE)

#' Areas file for national spectrum region code
ben_areas_spectrum_national <- ben_areas %>%
  mutate(spectrum_region_code = 0L)


sf::st_write(ben_areas_spectrum_national, "ben_areas_spectrum_national.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(ben_areas)
ggsave("ben_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)
