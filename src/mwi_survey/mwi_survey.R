#' ISO3 country code
iso3 <- "MWI"

areas <- read_sf("depends/mwi_areas.geojson")
areas <- read_sf("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/naomi-data/MWI/data/mwi_areas.geojson")
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

validate_survey_region_areas(survey_region_areas)

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

# devtools::load_all("~/Documents/GitHub/naomi.utils/")