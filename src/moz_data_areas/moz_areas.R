#' ## Mozambique (MOZ)
#' Source: Mozambique estimates team
#' Levels:
#'   * 1: Province (11))
#'   * 2: District (161)
#' Spectrum: Province (level 1)
#' EPP: Province (level 1)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: District (level 2)

dir.create("check")

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(sh = "MOZ/2020-01-22/MozDistrict.zip",
             dist = "MOZ/2019-11-19/Provincial%20and%20District%20Standardizations%2020190412.xlsx"

) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

sh <- read_sf_zip(files$sh)
dist <- read_excel(files$dist) %>%
  rename(province_code = 1,
         district_code = 4)

sh <- sh %>%
  mutate(Distrito = recode(Distrito,
                           "Alto Molócuè\r\n" = "Alto Molócuè",
                           "Chimbonila" = "Chimbunila",
                           "Chókwè" = "Chókwé",
                           "Gurue" = "Guruè",
                           "KaMpfumo" = "KaMpfumu",
                           "Liupo" = "Liúpo",
                           "Mandlakaze" = "Mandlakazi",
                           "Marínguè" = "Maríngué",
                           "Mecuburi" = "Mecubúri",
                           "Mocimboa da Praia" = "Mocímboa da Praia",
                           "Murrupula" = "Morrupula",
                           "Nacala Velha" = "Nacala-a-Velha",
                           "Nacaroa" = "Nacarôa",
                           "Namarroi" = "Namarói",
                           "Nlhamankulu" = "Nhlamankulu",
                           "Sussundenga" = "Sussendenga",
                           "Zumbo" = "Zumbu",
                           "N`Gauma" = "Ngaúma",
                           "Nacala Porto" = "Nacala"),
         dist = tolower(Distrito)) %>%
  full_join(
    dist %>%
    mutate(District = recode(District, "Cahora-Bassa" = "Cahora Bassa"),
           dist = tolower(District))
  )

moz_simple <- sh %>%
  ms_simplify(0.05) %>%
  sf::st_make_valid()

#' Get rid of fragments created by st_make_valid()
moz_simple <- st_collection_extract(moz_simple, "POLYGON")


moz_simple %>% pryr::object_size()
check_boundaries(moz_simple)

sh %>% filter(CodProv %in% 10:11) %>%
  ggplot() + geom_sf(aes(fill = Provincia))

moz_simple %>% filter(CodProv %in% 10:11) %>%
  ggplot() + geom_sf(aes(fill = Provincia))



#' # Spectrum region codes
#'

#' Create new level of hierarchy "Merged provinces" that are the same as the existing provinces in the shape file.
#' This will be admin-2.
#' Maputo Province and Maputo City will be merged to create Maputo, which will be an admin-2 level.
#' All other admin-2 areas will be identical to their parent admin-1 areas.
#'
moz_simple <- moz_simple %>%
  mutate(merge_prov = Province,
         merge_prov_code = province_code)


spectrum_region_code  <- c("Cabo Delgado" = 11,
                           # "Cidade de Maputo" = 20,
                           "Gaza" = 18,
                           "Inhambane" = 17,
                           "Manica" = 15,
                           # "Maputo Província" = 19,
                           "Nampula" = 12,
                           "Niassa" = 10,
                           "Sofala" = 16,
                           "Tete" = 14,
                           "Zambézia" = 13,
                           "Maputo" = 21)


#' Recode area hierarchy
#' Maputo Province and Maputo City are recoded to Maputo. No change to district boundaries within the province.
#' New province code for Maputo: MOZ_1_12

moz <- moz_simple %>%
  mutate(
    Province = case_when(
      Province %in% c("Maputo Província", "Cidade de Maputo") ~ "Maputo",
      TRUE ~ Province
    ),
    province_code = case_when(
      Province == "Maputo" ~ 12,
      TRUE ~ province_code
    ),
    spectrum_region_code = recode(Province, !!!spectrum_region_code)) %>%
  group_by(Province) %>%
  mutate(district_code = row_number()) %>%
  arrange(province_code, district_code)


moz_wide <- moz %>%
  transmute(name0 = "Mozambique",
            id0 = "MOZ",
            name1 = Province,
            id1 = fct_inorder(paste0("MOZ_1_", sprintf("%02d", province_code))),
            name2 = merge_prov,
            id2 = fct_inorder(paste0("MOZ_2_", sprintf("%02d",  merge_prov_code))),
            name3 = District,
            id3 = fct_inorder(paste0("MOZ_3_", sprintf("%02d%02d", province_code, district_code))),
            spectrum_region_code)

moz_long <- gather_areas(moz_wide)

moz_areas <- moz_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country", `1` = "Merged provinces", `2` = "Provincia", `3` = "Distrito"),
         display = TRUE,
         spectrum_level = area_level == 1,
         epp_level = area_level == 1,
         naomi_level = area_level == 3,
         pepfar_psnu_level = area_level == 3)


moz_areas_wide <- spread_areas(st_drop_geometry(moz_areas)) %>%
  left_join(select(moz_areas, area_id)) %>%
  select(-area_id)

#' # Create ADR files

moz_area_hierarchy <- moz_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)

moz_area_boundaries <- moz_areas %>%
  select(area_id, geometry)

moz_area_levels <- moz_areas %>%
  st_set_geometry(NULL) %>%
  count(area_level, area_level_label, display, spectrum_level,
        epp_level, naomi_level, pepfar_psnu_level, name = "n_areas") %>%
  select(area_level, n_areas, area_level_label, display, spectrum_level,
         epp_level, naomi_level, pepfar_psnu_level)

#' # Save boundaries

sf::st_write(moz_areas, "moz_areas.geojson", delete_dsn = TRUE)
sf::st_write(moz_area_boundaries, "moz_area_boundaries.geojson", delete_dsn = TRUE)

write_csv(moz_area_hierarchy, "moz_area_hierarchy.csv")
write_csv(moz_area_levels, "moz_area_levels.csv")


path <- file.path(getwd(), "moz_areas_wide.zip")
tmp <- file.path(tempdir(), "moz_areas_wide")
dir.create(tmp)
sf::st_write(moz_areas_wide, file.path(tmp, "moz_areas_wide.shp"), delete_layer = TRUE)
withr::with_dir(tmp, zip(path, list.files()))


#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(moz_areas)

ggsave("check/hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

dev.off()
