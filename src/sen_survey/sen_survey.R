#' ISO3 country code
iso3 <- "SEN"

areas <- read_sf("depends/sen_areas.geojson") %>%
  st_make_valid()
# areas <- read_sf("archive/sen_data_areas/20210210-182303-1e692476/sen_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994,
         SurveyId != "SN2018DHS",
         survey_id != "SEN2020MIS") ## REMOVE THIS!!

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' 2006 MIS and 2017 DHS have no GPS coordinates. Assign clusters to areas using survey region id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions %>% 
              filter(survey_id %in% c("SEN2006MIS", "SEN2017DHS"))) %>%
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

## There's a MICS for Dakar City to be added.

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