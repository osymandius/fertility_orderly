#' ISO3 country code
iso3 <- "TGO"

areas <- read_sf("depends/tgo_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas)


survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset
# not working here...
survey_clusters <- create_survey_clusters_dhs(surveys)

survey_clusters %>% 
  filter(survey_id == "TZA1999DHS") %>%
  ggplot() +
    geom_sf(data = areas %>% filter(area_level ==0), aes(geometry = geometry)) +
    geom_point(aes(x=longitude, y=latitude, group=survey_region_id, color=factor(survey_region_id)))

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