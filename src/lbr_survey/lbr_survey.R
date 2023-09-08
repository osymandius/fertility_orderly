#' ISO3 country code
iso3 <- "LBR"

areas <- readRDS("resources/areas.rds")[[iso3]]
# areas <- read_sf("archive/lbr_data_areas/20201215-073800-5f037e73/lbr_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' Region IDs are incorrectly coded for 2011 and 2016 MIS surveys. Correct region naming obrtained from spatial data repository 
#' https://spatialdata.dhsprogram.com/boundaries/#view=map&countryId=LB&surveyId=509&level=1
#' 
#' This process doesn't fix the problem of survey clusters being assigned outside the region boundaries because the hrd cluster dataset is wrong too..?? Investigate further at some point.

survey_region_boundaries %>%
  filter(survey_id == "LBR2016MIS") %>%
  mutate(centroid = st_centroid(geometry)) %>%
  ggplot() +
    geom_sf(aes(fill=factor(survey_region_id))) +
    geom_sf_label(aes(geometry = centroid, label = survey_region_name))

survey_region_boundaries <- survey_region_boundaries %>%
  mutate(survey_region_name = 
           case_when(
             survey_id == "LBR2016MIS" & survey_region_id == 6 ~ "North Western",
             survey_id == "LBR2016MIS" & survey_region_id == 5 ~ "South Eastern B",
             survey_id == "LBR2016MIS" & survey_region_id == 4 ~ "South Central",
             survey_id == "LBR2016MIS" & survey_region_id == 3 ~ "South Eastern A",
             survey_id == "LBR2016MIS" & survey_region_id == 2 ~ "North Central",
             survey_id == "LBR2016MIS" & survey_region_id == 1 ~ "Greater Monrovia",
             
             survey_id == "LBR2011MIS" & survey_region_id == 6 ~ "south central",
             survey_id == "LBR2011MIS" & survey_region_id == 5 ~ "north western",
             survey_id == "LBR2011MIS" & survey_region_id == 4 ~ "south eastern b",
             survey_id == "LBR2011MIS" & survey_region_id == 3 ~ "south eastern a",
             survey_id == "LBR2011MIS" & survey_region_id == 2 ~ "north central",
             survey_id == "LBR2011MIS" & survey_region_id == 1 ~ "monrovia",
             
             TRUE ~ survey_region_name
             
           ))


surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

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

while (!is.null(dev.list())) dev.off()