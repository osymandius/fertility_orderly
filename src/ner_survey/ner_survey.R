#' ISO3 country code
iso3 <- "NER"

areas <- read_sf("depends/ner_areas.geojson")
# areas <- read_sf("archive/ner_data_areas/20210113-104829-b7daf12b/ner_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)


surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide %>% st_make_valid(), survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' 2006 and 2012 DHS have no GPS coordinates. Assign clusters to areas using survey region id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions %>% 
              filter(survey_id %in% c("NER2006DHS", "NER2012DHS"))) %>%
  mutate(geoloc_area_id = ifelse(is.na(geoloc_area_id) & !is.na(survey_region_area_id), survey_region_area_id, geoloc_area_id)) %>%
  select(-c(survey_region_name, survey_region_area_id)) 

survey_clusters <- survey_clusters %>%
  filter(!is.na(geoloc_area_id)) %>%
  bind_rows(
    survey_clusters %>%
      filter(is.na(geoloc_area_id)) %>%
      select(-geoloc_area_id) %>%
      left_join(survey_regions %>% select(survey_id, survey_region_id, geoloc_area_id = survey_region_area_id))
  )

p_coord_check <- plot_survey_coordinate_check(survey_clusters,
                                              survey_region_boundaries,
                                              survey_region_areas)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

write_csv(survey_clusters, paste0(tolower(iso3), "_dhs_clusters.csv"))
