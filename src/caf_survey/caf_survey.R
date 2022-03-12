#' ISO3 country code
iso3 <- "CAF"

areas <- read_sf("depends/caf_areas.geojson")
# areas <- read_sf("archive/caf_data_areas/20210224-072623-9420edea/caf_areas.geojson")


####### MICS

mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
        mics_area_name_label == "Ombella Mpoko" ~ "Ombella M'Poko",
        mics_area_name_label == "Nana Mambere" ~ "Nana-Mambere",
        mics_area_name_label == "Nana Mambéré" ~ "Nana-Mambere",
        mics_area_name_label == "Mambere Kadei" ~ "Mambere-Kadei",
        mics_area_name_label == "Sangha Mbaere" ~ "Sangha-Mbarere",
        mics_area_name_label == "Baminigui Bangoran" ~ "Bamingui-Bangoran",
        mics_area_name_label == "Bamingui Bangoran" ~ "Bamingui-Bangoran",
        mics_area_name_label == "Kémo" ~ "Kemo",
        mics_area_name_label == "Nana Grebizi" ~ "Nana-Gribizi",
        mics_area_name_label == "Basse Kotto" ~ "Basse-Kotto",
        mics_area_name_label == "Haute Kotto" ~ "Haute-Kotto",
        mics_area_name_label == "Haut Mbomou" ~ "Haut-Mbomou",
        mics_area_name_label == "Région 1" ~ "RS1",
        mics_area_name_label == "Région 2" ~ "RS2",
        mics_area_name_label == "Région 3" ~ "RS3",
        mics_area_name_label == "Région 4" ~ "RS4",
        mics_area_name_label == "Région 5" ~ "RS5",
        mics_area_name_label == "Région 6" ~ "RS6",
        mics_area_name_label == "Région 7" ~ "RS7",
      TRUE ~ mics_area_name_label
    )
  ) 

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))
