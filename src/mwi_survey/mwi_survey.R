#' ISO3 country code
iso3 <- "MWI"

areas <- read_sf("depends/mwi_areas.geojson")
# areas <- read_sf("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/naomi-data/MWI/data/mwi_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' #' Action: recode survey boundaries to 2= North Buganda, 3 = South Buganda
#' 
#' survey_region_boundaries <- survey_region_boundaries %>%
#'   mutate(
#'     survey_region_id = case_when(
#'       survey_id == "UGA2018MIS" & survey_region_name == "North Buganda" ~ 2,
#'       survey_id == "UGA2018MIS" & survey_region_name == "South Buganda" ~ 3,
#'       TRUE ~ survey_region_id
#'     )
#'   )

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

survey_region_areas <- survey_region_areas %>%
  mutate(survey_region_id = case_when(
    survey_id == "MWI2015DHS" & area_id5 == "MWI_5_08" ~ 107,
    survey_id == "MWI2015DHS" & area_id5 == "MWI_5_18" ~ 210,
    survey_id == "MWI2015DHS" & area_id5 == "MWI_5_25" ~ 314,
    survey_id == "MWI2015DHS" & area_id5 == "MWI_5_33" ~ 315,
    TRUE ~ survey_region_id),
    survey_region_name = case_when(
      survey_id == "MWI2015DHS" & area_id5 == "MWI_5_08" ~ "Mzuzu City",
      survey_id == "MWI2015DHS" & area_id5 == "MWI_5_18" ~ "Lilongwe City",
      survey_id == "MWI2015DHS" & area_id5 == "MWI_5_25" ~ "Zomba City",
      survey_id == "MWI2015DHS" & area_id5 == "MWI_5_33" ~ "Blantyre City",
      TRUE ~ survey_region_name))

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

write_csv(survey_clusters, paste0(tolower(iso3), "_dhs_clusters.csv"))

mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

# mics_indicators <- read_csv("../../global/MICS_indicators.csv") %>%
#   pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      survey_id == "MWI2006MICS" & mics_area_name_label == "Nkhata Bay" ~ "Nkhatabay",
      survey_id == "MWI2019MICS" & mics_area_name_label == "North" ~ "Northern",
      survey_id == "MWI2019MICS" & mics_area_name_label == "South" ~ "Southern",
      TRUE ~ mics_area_name_label
    )
  )

#' Likoma not sampled in either survey
#' Mzimba North/South not present because joined Mzima is - corrected below
#' Neno: Divided into two districts - unclear how to deal with this. 
mics_survey_areas <- join_survey_areas(fertility_mics_data, areas, warn=TRUE)

#' 2013 MICS is largely at area_level = 5 except for Mzimba. Set warn=TRUE in join_survey_areas above to permit invalid data to complete, and add area_id in for Mzimba manually below
mics_survey_areas$hh <- mics_survey_areas$hh %>%
  mutate(
    area_id = case_when(
      survey_id == "MWI2013MICS" & mics_area_name_label == "Mzimba" ~ "MWI_3_05",
      TRUE ~ area_id
    )
  )

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))