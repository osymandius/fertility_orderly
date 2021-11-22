#' ISO3 country code
iso3 <- "GMB"

areas <- read_sf("depends/gmb_areas.geojson")
# areas <- read_sf("archive/gmb_data_areas/20210114-155305-1014e3b3/gmb_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)


surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' No GPS coordinates in any surveys. Assign to areas by survey id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions) %>%
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
# mics_indicators <- read_csv("global/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id") %>%
  filter(survey_id != "CIV2000MICS")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      survey_id == "GMB2005MICS" & mics_area_name_label == "Banjul South" ~ "Banjul City",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Banjul North" ~ "Banjul City",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Banjul Central" ~ "Banjul City",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Niamina Dankuku" ~ "Niamina Dankunku",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Upper Baddibu" ~ "Upper Badibu",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Lower Baddibu" ~ "Lower Badibu",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Central Baddibu" ~ "Central Badibu",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Kanifing" ~ "Kanifing Municipal",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Foni Bintang Karania" ~ "Foni Bintang Karanai",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Lower Nuimi" ~ "Lower Niumi",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Upper Nuimi" ~ "Upper Niumi",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Janjanburay" ~ "Janjan Bureh",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Fulladu West" ~ "Central River",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Fulladu East" ~ "Central River",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Wuli" ~ "Upper River",
      
      survey_id == "GMB2010MICS" & mics_area_name_label == "Banjul" ~ "Banjul/Kanifing/Brikama",
      survey_id == "GMB2010MICS" & mics_area_name_label == "Kanifing" ~ "Banjul/Kanifing/Brikama",
      survey_id == "GMB2010MICS" & mics_area_name_label == "Brikama" ~ "Banjul/Kanifing/Brikama",
      survey_id == "GMB2010MICS" & mics_area_name_label == "Kuntaur" ~ "Kuntaur/Janjanbureh",
      survey_id == "GMB2010MICS" & mics_area_name_label == "Janjanburay" ~ "Kuntaur/Janjanbureh",
      
      survey_id == "GMB2018MICS" & mics_area_name_label == "Banjul" ~ "Banjul/Kanifing/Brikama",
      survey_id == "GMB2018MICS" & mics_area_name_label == "Kanifing" ~ "Banjul/Kanifing/Brikama",
      survey_id == "GMB2018MICS" & mics_area_name_label == "Brikama" ~ "Banjul/Kanifing/Brikama",
      survey_id == "GMB2018MICS" & mics_area_name_label == "Kuntaur" ~ "Kuntaur/Janjanbureh",
      survey_id == "GMB2018MICS" & mics_area_name_label == "Janjanbureh" ~ "Kuntaur/Janjanbureh",
      
      TRUE ~ mics_area_name_label
    )
  ) 

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas, warn=TRUE)

# Required because the MICS indicator sheet only takes a single area level to use rather than multiple levels - this needs improving.
mics_survey_areas$hh <- mics_survey_areas$hh %>%
  mutate(
    area_id = case_when(
      survey_id == "GMB2005MICS" & mics_area_name_label == "Central River" ~ "GMB_1_1ng",
      survey_id == "GMB2005MICS" & mics_area_name_label == "Upper River" ~ "GMB_1_4ui",
      TRUE ~ area_id
    )
  )

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))
