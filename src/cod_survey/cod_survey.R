#' ISO3 country code
iso3 <- "COD"

areas <- read_sf("depends/cod_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)
survey_region_boundaries <- st_make_valid(survey_region_boundaries)

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

write_csv(survey_clusters, paste0(tolower(iso3), "_dhs_clusters.csv"))

mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)


#' "Province Orientale", "Kasai Occidental" have since been divided. No good way to assign clusters without coordinates to nested districts. Drop for now.
fertility_mics_data$hh <- fertility_mics_data$hh %>%
mutate(
  mics_area_name_label = case_when(
    
    # Survey has admin 1 information but since disaggregated. Revisit with better tools to allocate clusters w/o GPS coordinates. Use national for now.
    survey_id == "COD2010MICS" ~ "Democratic Republic of The Congo",
    
    survey_id == "COD2017MICS" & mics_area_name_label == "Kongo Central" ~ "Kongo-Central",
    survey_id == "COD2017MICS" & mics_area_name_label == "Sud Ubangi" ~ "Sud-Ubangi",
    survey_id == "COD2017MICS" & mics_area_name_label == "Nord Ubangi" ~ "Nord-Ubangi",
    survey_id == "COD2017MICS" & mics_area_name_label == "Bas Uele" ~ "Bas-Uele",
    survey_id == "COD2017MICS" & mics_area_name_label == "Haut Uele" ~ "Haut-Uele",
    survey_id == "COD2017MICS" & mics_area_name_label == "Nord Kivu" ~ "Nord-Kivu",
    survey_id == "COD2017MICS" & mics_area_name_label == "Sud Kivu" ~ "Sud-Kivu",
    survey_id == "COD2017MICS" & mics_area_name_label == "Haut Lomami" ~ "Haut-Lomami",
    survey_id == "COD2017MICS" & mics_area_name_label == "Haut Katanga" ~ "Haut-Katanga",
    survey_id == "COD2017MICS" & mics_area_name_label == "Kasai Oriental" ~ "Kasaï-Oriental",
    survey_id == "COD2017MICS" & mics_area_name_label == "Kasai Central" ~ "Kasaï-Central",
    survey_id == "COD2017MICS" & mics_area_name_label == "Maindombe" ~ "Maï-Ndombe",
    survey_id == "COD2017MICS" & mics_area_name_label == "Kasai" ~ "Kasaï",
    
    TRUE ~ mics_area_name_label
  )
) %>%
  filter(!fertility_mics_data$hh$mics_area_name_label %in% c("Province Orientale", "Kasai Occidental"))

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

#' Remove women without areas as a result of removing Province Orientale", "Kasai Occidental" from 2017 MICS
asfr_input_data$wm <- filter(asfr_input_data$wm, !is.na(area_id))
asfr_input_data$births_to_women <- filter(asfr_input_data$births_to_women, unique_id %in% unique(asfr_input_data$wm$unique_id))

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))