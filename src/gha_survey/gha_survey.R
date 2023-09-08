#' ISO3 country code
iso3 <- "GHA"

areas <- readRDS("resources/areas.rds")[[iso3]]
# areas <- read_sf("archive/gha_data_areas/20210106-153710-70b4d1a5/gha_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#Upper East and Upper West have the boundary codes swapped in 2014 DHS. Revert.
survey_region_boundaries <- survey_region_boundaries %>%
  mutate(survey_region_id = case_when(
    survey_region_name == "Upper East" & survey_id == "GHA2014DHS" ~ 9,
    survey_region_name == "Upper West" & survey_id == "GHA2014DHS" ~ 10,
    TRUE ~ survey_region_id
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

## Ghana has lots of MICS surveys but haven't done them yet. Drowning in DHS surveys anyway..

# # ics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
# mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
#   pivot_longer(-c(label, id, filetype), names_to = "survey_id")
# 
# mics_survey_data <- create_surveys_mics(iso3, mics_indicators)
# 
# fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)
# 
# fertility_mics_data$hh <- fertility_mics_data$hh %>%
#   mutate(
#     mics_area_name_label = case_when(
#       mics_area_name_label == "Hauts-Bassins" ~ "Hauts Bassins",
#       mics_area_name_label == "Cascade" ~ "Cascades",
#       mics_area_name_label == "Plateau-Central" ~ "Plateau Central",
#       TRUE ~ mics_area_name_label
#     )
#   ) 
# 
# mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)
# 
# asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)
# 
# write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
# write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))