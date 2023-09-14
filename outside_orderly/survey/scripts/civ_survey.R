#' ISO3 country code
iso3 <- "CIV"

areas <- readRDS("global/areas.rds")[[iso3]]
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' 1998 DHS has no boundaries, but separates the country into Abidjan, and rest of country by urban/rural.
#' Take Abidjan boundaries from 2005 AIS, subtract it from the 1998 DHS shapefile to produce the rest of the country
#' Collapse urban/rural rest of country into single category (1), recode Abidjan to survey_region_id = 2

abidjan <- survey_region_boundaries$geometry[survey_region_boundaries$survey_id == "CIV2005AIS" & survey_region_boundaries$survey_region_name == "ville d'abidjan"]
country <- survey_region_boundaries$geometry[survey_region_boundaries$survey_id == "CIV1998DHS"]

survey_region_boundaries <- survey_region_boundaries %>%
  filter(survey_id != "CIV1998DHS") %>%
  bind_rows(
    data.frame(
      survey_id = rep("CIV1998DHS", 2),
      survey_region_id = c(1,2),
      survey_region_name = c("country without abidjan", "ville d'abidjan"), 
      REGVAR = rep("hv024", 2),
      geometry = c(st_difference(country, abidjan), 
                   survey_region_boundaries$geometry[survey_region_boundaries$survey_id == "CIV2005AIS" & survey_region_boundaries$survey_region_name == "ville d'abidjan"]
      )
    )
  )

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

#' For 1998 DHS: Collapse urban/rural rest of country into single category (1), recode Abidjan to survey_region_id = 2
survey_clusters <- survey_clusters %>%
  mutate(
    survey_region_id = as.numeric(survey_region_id),
    survey_region_id = case_when(
      survey_id == "CIV1998DHS" & survey_region_id == 0 ~ 2,
      survey_id == "CIV1998DHS" & survey_region_id == 3 ~ 1,
      TRUE ~ survey_region_id
    )
  )


#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' 2005 AIS has no GPS coordinates. Assign clusters to areas using survey region id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions %>% 
              filter(survey_id == "CIV2005AIS")) %>%
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

#p_coord_check <- plot_survey_coordinate_check(survey_clusters,
#                                               survey_region_boundaries,
#                                               survey_region_areas)

# dir.create("check")
# pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
# p_coord_check
# dev.off()

write_csv(survey_clusters, paste0("outside_orderly/survey/outputs/", tolower(iso3), "_dhs_clusters.csv"))

mics_indicators <- read_csv("global/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id") %>%
  filter(survey_id != "CIV2000MICS")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

#' Both surveys have more geograpgic information than at national level but do not clearly map to hierarchy. More work required.
fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      survey_id == "CIV2000MICS" ~ "Cote d'Ivoire",
      survey_id == "CIV2006MICS" ~ "Cote d'Ivoire",
      TRUE ~ mics_area_name_label
    )
  ) 

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0("outside_orderly/survey/outputs/", tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0("outside_orderly/survey/outputs/", tolower(iso3), "_mics_births_to_women.csv"))