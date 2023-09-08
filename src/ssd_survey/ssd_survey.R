#' ISO3 country code
iso3 <- "SSD"

areas <- read_sf("depends/ssd_areas.geojson")
# areas <- read_sf("archive/caf_data_areas/20210224-072623-9420edea/caf_areas.geojson")


####### MICS

mics_indicators <- read_csv("resources/MICS_indicators.csv", show_col_types = F) %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
        mics_area_name_label == "Warap" ~ "Warrap",
        # mics_area_name_label == "Northern Bahr el Ghazal" ~ "Northern Bahr El Ghazal", 
        # mics_area_name_label == "Western Bahr el Ghazal" ~ "Western Bahr El Ghazal",
        mics_area_name_label == "Northern Bahr El Ghazal" ~ "Northern Bahr el Ghazal",
        mics_area_name_label == "Western Bahr El Ghazal" ~ "Western Bahr el Ghazal",
      TRUE ~ mics_area_name_label
    )
  ) 

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))
