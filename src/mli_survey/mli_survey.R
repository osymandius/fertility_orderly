#' ISO3 country code
iso3 <- "MLI"

areas <- read_sf("depends/mli_areas.geojson") %>%
  st_make_valid()
# areas <- read_sf("archive/mli_data_areas/20210107-163124-750caa70/mli_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

survey_region_boundaries <- survey_region_boundaries %>%
  bind_rows(
    data.frame(
      survey_id = rep("MLI1996DHS", 2),
      survey_region_id = c(6,7),
      survey_region_name = c("Tombouctou", "Gao"),
      REGVAR = rep("hv024", 2)
    ) %>%
      left_join(
        filter(areas, area_level == 1) %>%
          mutate(area_name = recode(area_name, "Timbuktu" = "Tombouctou")) %>%
          select(area_name),
        by=c("survey_region_name" = "area_name")
      )
    ,
    data.frame(
      survey_id = rep("MLI2001DHS", 3),
      survey_region_id = c(6,7,8),
      survey_region_name = c("Tombouctou", "Gao", "Kidal"),
      REGVAR = rep("hv024", 3)
    ) %>%
      left_join(
        filter(areas, area_level == 1) %>%
          mutate(area_name = recode(area_name, "Timbuktu" = "Tombouctou")) %>%
          select(area_name),
        by=c("survey_region_name" = "area_name")
      )
  ) %>%
  filter(!is.na(survey_region_name))

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries %>% st_make_valid())

validate_survey_region_areas(survey_region_areas, survey_region_boundaries, warn=TRUE)

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

# mics_indicators <- read_csv("global/MICS_indicators.csv") %>%
mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id") %>%
  filter(survey_id != "CIV2000MICS")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      mics_area_name_label == "Sã©Gou" ~ "Ségou",
      mics_area_name_label == "Segou" ~ "Ségou",
      TRUE ~ mics_area_name_label
    )
  ) 

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))