#' ISO3 country code
iso3 <- "TGO"

# areas <- read_sf("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/naomi-data/TGO/data/tgo_areas.geojson")
areas <- read_sf("depends/tgo_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

# survey_id survey_region_id    survey_region_name
# TGO1998DHS                1                  lome
# TGO2017MIS                1 Agglomération de Lomé

survey_region_boundaries %>%
  filter(survey_id == "TGO2017MIS",
         survey_region_name == "Agglomération de Lomé") %>%
  ggplot() +
    geom_sf() +
    geom_sf(data = areas %>% filter(parent_area_id == "TGO_1_3"), aes(fill=area_name, geometry = geometry), alpha=0.3)

survey_region_boundaries %>%
  filter(survey_id == "TGO1998DHS",
         survey_region_name == "lome") %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = areas %>% filter(parent_area_id == "TGO_1_3"), aes(fill=area_name, geometry = geometry), alpha=0.3)

survey_region_areas <- survey_region_areas %>%
  bind_rows(
    data.frame(
      survey_id = c("TGO1998DHS", "TGO2017MIS"),
      survey_region_id = c(1,1),
      survey_region_name = c("lome", "Agglomération de Lomé"),
      REGVAR = rep("hv024",2),
      area_id = rep("TGO_1_3",2),
      area_id2 = rep("TGO_2_20sk",2),
      geometry = rep(filter(areas, area_id == "TGO_2_20sk")$geometry,2)
    ))

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

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

#### MICS SURVEYS

mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(mics_area_name_label = recode(mics_area_name_label,
                                 "Lomé\u0082" = "Lome",
                                 "Lomé" = "Lome",
                                 "Maritime (Sans Lomé)" = "Maritime"),
         mics_area_name_label = case_when(
           survey_id == "TGO2017MICS" & mics_area_name_label == "Lomé Commune" ~ "Lome",
           survey_id == "TGO2017MICS" & mics_area_name_label == "Golfe Urbain" ~ "Lome",
           TRUE ~ mics_area_name_label
         )
  )

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))