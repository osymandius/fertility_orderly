#' ISO3 country code
iso3 <- "COG"

areas <- read_sf("depends/cog_areas.geojson")
# areas <- read_sf("archive/cog_data_areas/20210118-064454-dc82bd12/cog_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)


surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries %>% st_make_valid())

cog2009ais_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "COG2009AIS", survey_region_id == 12),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

cog2011dhs_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "COG2011DHS", survey_region_id == 12),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

survey_region_areas <- survey_region_areas %>%
  bind_rows(cog2009ais_region1_areas, cog2011dhs_region1_areas)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

survey_clusters <- create_survey_clusters_dhs(surveys)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' No GPS coordinates in any surveys. Assign to areas by survey id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions) %>%
  mutate(geoloc_area_id = ifelse(is.na(geoloc_area_id) & !is.na(survey_region_area_id), survey_region_area_id, geoloc_area_id)) %>%
  select(-c(survey_region_name, survey_region_area_id)) 

p_coord_check <- plot_survey_coordinate_check(survey_clusters,
                                              survey_region_boundaries,
                                              survey_region_areas)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

write_csv(survey_clusters, paste0(tolower(iso3), "_dhs_clusters.csv"))
