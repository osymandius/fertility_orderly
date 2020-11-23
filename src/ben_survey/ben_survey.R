#' ISO3 country code
iso3 <- "BEN"

areas <- read_sf("depends/ben_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

#' 2001 survey missing 2 districts near Cotonou
survey_region_areas <- survey_region_areas %>%
  mutate(
    survey_id = replace_na(survey_id, "BEN2001DHS"),
    survey_region_id = replace_na(survey_region_id, 2),
    survey_region_name = replace_na(survey_region_name, "atlantique"),
    REGVAR = replace_na(REGVAR, "hv024")
    
  )
  

validate_survey_region_areas(survey_region_areas %>% st_as_sf)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys)


#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

# Filter out 4 clusters from 2012 DHS that have been placed on ~0 lat, 0 long
survey_clusters <- survey_clusters %>%
  filter(geoloc_distance < 6)
  

p_coord_check <- plot_survey_coordinate_check(survey_clusters,
                                              survey_region_boundaries,
                                              survey_region_areas)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

write_csv(survey_clusters, paste0(tolower(iso3), "_dhs_clusters.csv"))