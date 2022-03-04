#' ## Kenya (KEN)
#' Source: Kenya estimates team
#' Levels:
#'   * 1: Former Province (8)
#'   * 2: County (47)
#'   * 3: Health sub-county (304)
#' Spectrum: Former province (level 1)
#' EPP: Former province (level 1)
#' EPP Urban/Rural: No
#' PEPFAR PSNU: County (level 1)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

#' ## Load source files
#'
#' Sub-county list and area IDs, manually assigned
subc_url <- URLencode(file.path(naomi_raw_path, "KEN/2020-12-06/ken-area-id-subcounty_2020-12-06.csv"))
subc_file <- sharepoint$download(subc_url)

subc <- read_csv(subc_file)
subc <- fill(subc, area_id0:area_name2, county, .direction = "down")

#' Subcounty shapefile from Kenya HIV estimates team (25 November 2020)
sh_url <- URLencode(file.path(naomi_raw_path, "KEN/2020-11-25/Shapefile.zip"))

sh_file <- sharepoint$download(sh_url)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

tmpf <- tempfile()
unzip(sh_file, exdir = tmpf)
sh <- read_sf(file.path(tmpf, "Shapefile/ke_subcounty.shp"))

#' Health sub-county shapefile prepared by Benard Mitto on behalf of UNAIDS
mitto_url <- URLencode(file.path(naomi_raw_path, "KEN/2020-12-07/Kenya.zip"))
mitto_file <- sharepoint$download(mitto_url)

tmpf <- tempfile()
unzip(mitto_file, exdir = tmpf)
mitto <- read_sf(file.path(tmpf, "ken_adm2.shp"))


#' ## Clean shape files
#'
#' Simplify boundaries
ken_simple <- ms_simplify(sh, 0.03)
ken_simple <- st_make_valid(ken_simple)
ken_simple <- st_collection_extract(ken_simple, "POLYGON")

stopifnot(st_is_valid(ken_simple))

pryr::object_size(select(sh))
pryr::object_size(select(ken_simple))

ken_county <- count(ken_simple, county) %>%
  st_collection_extract("POLYGON")


p_national <- check_boundaries(sh, ken_simple)
ggsave("check/simplified-boundaries-national.pdf", p_national, h = 7, w = 14)

p_county <- Map(check_boundaries,
                  split(sh, sh$county),
                  split(ken_simple, ken_simple$county))

pdf("check/simplified-boundaries-county.pdf", h = 5, w = 10)
Map(grid.arrange, p_county, top = names(p_county))
dev.off()

#' Mitto shapefile

mitto <- mitto %>%
  select(country = area_name0,
         province = area_name1,
         county = area_name2,
         subcounty = area_name3,
         dhis2_id)

mitto_simple <- ms_simplify(mitto, 0.03)
mitto_simple <- st_make_valid(mitto_simple)
mitto_simple <- st_collection_extract(mitto_simple, "POLYGON")

stopifnot(st_is_valid(mitto_simple))

pryr::object_size(select(mitto))
pryr::object_size(select(mitto_simple))

mitto_county <- count(mitto_simple, county) %>%
  st_collection_extract("POLYGON")


p_national <- check_boundaries(mitto, mitto_simple)
ggsave("check/unaids-simplified-boundaries-national.pdf", p_national, h = 7, w = 14)

p_county <- Map(check_boundaries,
                split(mitto, mitto$county),
                split(mitto_simple, mitto_simple$county))
pdf("check/unaids-simplified-boundaries-county.pdf", h = 5, w = 10)
Map(grid.arrange, p_county, top = names(p_county))
dev.off()


#' # Compare Kenya team and Mitto shapefiles

p_compare_county <- ggplot() +
  geom_sf(data = mitto_county, color = "red3") +
  geom_sf(data = ken_county, color = "blue3", fill = NA) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

ken_simple$county <- str_to_title(ken_simple$county)
ken_simple$subcounty <- sub(" Sub County", "", ken_simple$subcounty)
mitto_simple$subcounty <- sub(" Sub County", "", mitto_simple$subcounty)

setdiff(ken_simple$county, mitto_simple$county)
setdiff(mitto_simple$county, ken_simple$county)



compare_plot <- function(mitto, ken) {

  th <- list(theme_minimal(),
             theme(axis.text = element_blank(),
                   axis.title = element_blank(),
                   panel.grid = element_blank(),
                   plot.title = element_text(face = "bold")))

  p1 <- ggplot(mitto, aes(label = subcounty)) +
    geom_sf(data = ken, color = "red3") +
    geom_sf(color = "blue4", fill = NA) +
    geom_sf_label(alpha = 0.7, label.size = NA) +
    ggtitle(paste0(mitto$county[1], ": UNAIDS shapefile boundaries")) +
    th

  p2 <- ggplot(ken, aes(label = subcounty)) +
    geom_sf(data = mitto, color = "red3") +
    geom_sf(color = "blue4", fill = NA) +
    geom_sf_label(alpha = 0.7, label.size = NA) +
    ggtitle(paste0(ken$county[1], ": Kenya team shapefile boundaries")) +
    th

  arrangeGrob(p1, p2, nrow = 1)
}


p_compare <- Map(compare_plot,
                 mitto_simple %>% split(.$county),
                 ken_simple %>% split(.$county))

pdf("check/compare-boundaries-subcounty_unaids-kenya-team.pdf", h = 5, w = 10)
Map(grid.arrange, p_compare)
dev.off()



#' # Spectrum region codes

spectrum_region_code <- c("Central" = 10,
                          "Coast" = 11,
                          "Eastern" = 12,
                          "Nairobi" = 13,
                          "North Eastern" = 14,
                          "Nyanza" = 15,
                          "Rift Valley" = 16,
                          "Western" = 17)


#' ## Recode area hierarchy

ken_merge <- mitto_simple %>%
  mutate(
    county = sub("-", " ", county),
    subcounty = str_squish(str_to_title(subcounty)),
    subcounty = trimws(sub("Sub[ -]+County", "", subcounty)),
    subcounty = recode(subcounty,
                       "Igambang'ombe" = "Igambangombe",
                       "Tiaty" = "Tiaty East",
                       "Suba" = "Suba South",
                       "Nandi East" = "Nandi Hills",
                       "Kasipul" = "Rachuonyo South")
  )


subc$county <- sub(" County", "", subc$county)

ken_wide <- subc %>%
  mutate(subcounty = trimws(sub("Sub County", "", subcounty))) %>%
  full_join(ken_merge, by = c("county", "subcounty"))

stopifnot(!is.na(ken_wide$area_name3))
stopifnot(!is.na(ken_wide$dhis2_id))


ken_wide <- ken_wide %>%
  filter(!is.na(area_id3)) %>%
  st_as_sf() %>%
  select(starts_with("area_"), "dhis2_id") %>%
  rename_all(~sub("area_", "", .)) %>%
  mutate(spectrum_region_code = recode(name1, !!!spectrum_region_code))

ken_long <- gather_areas(ken_wide)

ken_areas <- ken_long %>%
  arrange(area_level, area_id) %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Former Province",
                  `2` = "County",
                  `3` = "Health Sub-county"),
         display = TRUE,
         spectrum_level = area_level == 1,
         epp_level = area_level == 1,
         naomi_level = area_level == 2,
         pepfar_psnu_level = area_level == 2)



#' Save boundaries
sf::st_write(ken_areas, "ken_areas_subcounty.geojson", delete_dsn = TRUE)

#' Plot hierarchy

hierarchy_plot <- plot_area_hierarchy_summary(ken_areas)
ggsave("ken_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)

dev.off()
