orderly_pull_archive("uga_data_areas")

#' ISO3 country code
iso3 <- "UGA"

areas <- read_sf("depends/uga_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

# UGA 2004 AIS boundaries are restricted
surveys <- surveys %>%
  filter(survey_id != "UGA2004AIS")

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' Action: recode survey boundaries to 2= North Buganda, 3 = South Buganda

survey_region_boundaries <- survey_region_boundaries %>%
  mutate(
    survey_region_id = case_when(
      survey_id == "UGA2018MIS" & survey_region_name == "North Buganda" ~ 2,
      survey_id == "UGA2018MIS" & survey_region_name == "South Buganda" ~ 3,
      TRUE ~ survey_region_id
    )
  )

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

p_coord_check <- plot_survey_coordinate_check(survey_clusters,
                                              survey_region_boundaries,
                                              survey_region_areas)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

write.csv(survey_clusters, paste0(tolower(iso3), "_dhs_clusters.csv"))