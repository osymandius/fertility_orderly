#orderly_pull_archive("zmb_data_areas")

iso3 <- "ZMB"

areas <- read_sf("depends/zmb_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#REGVAR for ZMB2013DHS is incorrectly coded as v024, should be hv024
surveys$REGVAR[surveys$survey_id == "ZMB2013DHS"] <- "hv024"

survey_region_boundaries <- st_make_valid(survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)
validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

p_coord_check <- plot_survey_coordinate_check(survey_clusters,
                                              survey_region_boundaries,
                                              survey_region_areas)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

write_csv(survey_clusters %>% filter(!is.na(geoloc_area_id)), paste0(tolower(iso3), "_dhs_clusters.csv"))