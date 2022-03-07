#' ISO3 country code
iso3 <- "KEN"

areas <- read_sf("depends/ken_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
survey_region_boundaries <- st_make_valid(survey_region_boundaries)

#' Note: don't worry about NA in survey_region_boundaries in 1998 DHS: the North Eastern region was excluded from the survey

#' 2015 MIS region boundaries on Spatial Data Repository are 5 malaria zones
#' However, there are 8 survey_region_ids that map to administrative 1 areas in naomi shapefiles.
#' Replace survey_region_boundaries for 2015 MIS survey with our boundaries

survey_region_boundaries <- survey_region_boundaries %>%
  filter(survey_id != "KEN2015MIS") %>%
  bind_rows(
    areas %>%
      filter(area_level ==1) %>%
      select(area_name, geometry) %>%
      transmute(survey_region_name = tolower(area_name)) %>%
      left_join(data.frame(survey_region_id = c(1:5, 7:9),
                           survey_region_name = c("coast", "north eastern", "eastern", "central", "rift valley", "western", "nyanza", "nairobi")
      )) %>%
      mutate(survey_id = "KEN2015MIS",
             REGVAR = "hv024"
      )
  )

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' 1998 DHS has no GPS coordinates. Assign clusters to areas using survey region id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions %>% 
              filter(survey_id == "KEN1998DHS")) %>%
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

mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      survey_id == "KEN2009MICS" & mics_area_name_label == "Coast Province" ~ "Coast",
      TRUE ~ mics_area_name_label
    )
  )

#' 2009 Survey sampled Coast province only
#' 2011 survey sampled Nyanza province only.
mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))