orderly_pull_archive("civ_data_areas")
#' ISO3 country code
iso3 <- "CIV"

areas <- read_sf("depends/civ_areas.geojson")
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

survey_clusters <- create_survey_clusters_dhs(surveys)

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
              filter(survey_id == "CIV2005")) %>%
  mutate(geoloc_area_id = ifelse(is.na(geoloc_area_id) & !is.na(survey_region_area_id), survey_region_area_id, geoloc_area_id)) %>%
  select(-c(survey_region_name, survey_region_area_id)) 

p_coord_check <- plot_survey_coordinate_check(survey_clusters,
                                              survey_region_boundaries,
                                              survey_region_areas)

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
p_coord_check
dev.off()

write.csv(survey_clusters, paste0(tolower(iso3), "_dhs_clusters.csv"))
