#' ISO3 country code
iso3 <- "CMR"

# areas <- read_sf("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/naomi-data/CMR/data/cmr_areas.geojson")
areas <- read_sf("depends/cmr_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

# Yaounde missing from 1998 boundaries
survey_region_boundaries <- survey_region_boundaries %>%
  mutate(
    survey_region_name = ifelse(
      survey_id == "CMR1998DHS" & is.na(survey_region_name),
      "yaounde",
      survey_region_name
    ),
    REGVAR = "hv024"
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
              filter(survey_id == "CMR1998DHS")) %>%
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

mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
# mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      
      #' The 2000 survey groups the admin1 areas into 5 regions. When we can aggregate to any area of choice, coming back and improve this mapping
      survey_id == "CMR2000MICS" ~ "Cameroon",
      
      # survey_id == "CMR2006MICS" & mics_area_name_label == "Centre" ~ "Centre (sans Yaoundé)",
      survey_id == "CMR2006MICS" & mics_area_name_label == "Centre" ~ "Centre (sans YaoundÃ©)",
      survey_id == "CMR2006MICS" & mics_area_name_label == "Extreme Nord" ~ "Extreme-Nord",
      survey_id == "CMR2006MICS" & mics_area_name_label == "Littoral" ~ "Littoral (sans Douala)",
      survey_id == "CMR2006MICS" & mics_area_name_label == "Nord Ouest" ~ "Nord-Ouest",
      survey_id == "CMR2006MICS" & mics_area_name_label == "Sud Ouest" ~ "Sud-Ouest",
      # survey_id == "CMR2006MICS" & mics_area_name_label == "Yaounde" ~ "Yaoundé",
      survey_id == "CMR2006MICS" & mics_area_name_label == "Yaounde" ~ "YaoundÃ©",
      
      # survey_id == "CMR2014MICS" & mics_area_name_label == "Centre (Sans Yaoundã©)" ~ "Centre (sans Yaoundé)",
      survey_id == "CMR2014MICS" & mics_area_name_label == "Centre (Sans Yaoundã©)" ~ "Centre (sans YaoundÃ©)",
      survey_id == "CMR2014MICS" & mics_area_name_label == "Littoral (Sans Douala)" ~ "Littoral (sans Douala)",
      survey_id == "CMR2014MICS" & mics_area_name_label == "Extrãªme-Nord" ~ "Extreme-Nord",
      # survey_id == "CMR2014MICS" & mics_area_name_label == "Yaoundã©" ~ "Yaoundé",
      survey_id == "CMR2014MICS" & mics_area_name_label == "Yaoundã©" ~ "YaoundÃ©",
      
      TRUE ~ mics_area_name_label
    )
  )

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))
