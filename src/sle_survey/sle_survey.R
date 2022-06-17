#' ISO3 country code
iso3 <- "SLE"

areas <- read_sf("depends/sle_areas.geojson")
# areas <- read_sf("archive/sle_data_areas/20210105-223311-1cbdb398/sle_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994,
         SurveyId != "SL2016MIS")

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' Some 2019 DHS district codes are wrong or missing. Should be:
    # 11: Kailahun
    # 12: Kenema
    # 13: Kono
    # 21: Bombali [missing]
    # 22: Falaba [coded as 26]
    # 23: Koinadugu [coded as 27]
    # 24: Tonkolli [coded as 25]
    # 31: Kambia [coded as 22]
    # 32: Karene [coded as 28]
    # 33: Port Loko [coded as 30]
    # 41: Bo [coded as 31]
    # 42: Bonthe [missing]
    # 43: Moyamba [coded as 33]
    # 44: Pujehun [coded as 34]
    # 51: Western Area Rural [coded as 41]
    # 52: Western Area Urban [coded as 42]

survey_region_boundaries <- survey_region_boundaries %>%
  mutate(survey_region_id = case_when(
    survey_id == "SLE2019DHS" & survey_region_name == "Bombali" ~ 21,
    survey_id == "SLE2019DHS" & survey_region_name == "Falaba" ~ 22,
    survey_id == "SLE2019DHS" & survey_region_name == "Koinadugu" ~ 23,
    survey_id == "SLE2019DHS" & survey_region_name == "Tonkolili" ~ 24,
    survey_id == "SLE2019DHS" & survey_region_name == "Kambia" ~ 31,
    survey_id == "SLE2019DHS" & survey_region_name == "Karene" ~ 32,
    survey_id == "SLE2019DHS" & survey_region_name == "Port Loko" ~ 33,
    survey_id == "SLE2019DHS" & survey_region_name == "Bo" ~ 41,
    survey_id == "SLE2019DHS" & survey_region_name == "Bonthe" ~ 42,
    survey_id == "SLE2019DHS" & survey_region_name == "Moyamba" ~ 43,
    survey_id == "SLE2019DHS" & survey_region_name == "Pujehun" ~ 44,
    survey_id == "SLE2019DHS" & survey_region_name == "Western Area Rural" ~ 51,
    survey_id == "SLE2019DHS" & survey_region_name == "Western Area Urban" ~ 52,
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

## 2010 and 2018 MICS to be added

# ics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

# Surveys have N, S, E, W. Newly created province of NW from the N province does not exist (2017). All of North assigned to Northern, and none to North Western
fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      mics_area_name_label == "East" ~ "Eastern",
      mics_area_name_label == "North" ~ "Northern",
      mics_area_name_label == "South" ~ "Southern",
      mics_area_name_label == "West" ~ "Western",
      TRUE ~ mics_area_name_label
    )
  )

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))