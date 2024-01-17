#' ISO3 country code
iso3 <- "COG"

areas <- readRDS("global/areas.rds")[[iso3]] %>%
  mutate(area_name = str_to_sentence(area_name))

# areas <- read_sf("archive/cog_data_areas/20210118-064454-dc82bd12/cog_areas.geojson") %>%
#   mutate(area_name = str_to_sentence(area_name))

areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)


surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region
# debugonce(allocate_areas_survey_regions)
survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries %>% st_make_valid())

cog2009ais_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "COG2009AIS", survey_region_id == 12),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

cog2011dhs_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "COG2011DHS", survey_region_id == 12),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

survey_region_areas <- survey_region_areas %>%
  bind_rows(cog2009ais_region1_areas, cog2011dhs_region1_areas)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' No GPS coordinates in any surveys. Assign to areas by survey id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions) %>%
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

## Code assigns Brazzaville in 2005 survey to Admin1 area Pool-Brazzaville. However - 100% of clusters are urban and the survey area is not much bigger than the Naomi boundary city. Better matched to admin2 Brazzaville.
survey_region_boundaries %>%
  filter(survey_id == "COG2005DHS", survey_region_id == 1) %>%
  ggplot() + 
  geom_sf(color = "red") + 
  geom_sf(data = areas %>% filter(parent_area_id == "COG_2_02fq"), fill = NA)

## Code matches "Pool" to "Pool-Brazzaville", but 100% of clusters are rural. Assign to admin1 Pool, excluding the city of Brazaville
survey_region_boundaries %>%
  filter(survey_id == "COG2011DHS", survey_region_id == 5) %>%
  ggplot() + 
  geom_sf(color = "red") + 
  geom_sf(data = areas %>% filter(parent_area_id == "COG_2_11wj"), fill = NA)

## Similarly - code matches Kouliou to Point-Noire-Koiliou. Clusters are all rural - assign to Kouliou district.
survey_region_boundaries %>%
  filter(survey_id == "COG2011DHS", survey_region_id == 1) %>%
  ggplot() + 
  geom_sf(color = "red") + 
  geom_sf(data = areas %>% filter(parent_area_id == "COG_2_05pk"), fill = NA)

survey_clusters <- survey_clusters %>%
  mutate(geoloc_area_id = case_when(
    survey_id == "COG2005DHS" & geoloc_area_id == "COG_1_02yo" ~ "COG_2_02fq",
    survey_id == "COG2011DHS" & area_id == "COG_1_05dt" ~ "COG_2_05pk",
    survey_id == "COG2011DHS" & area_id == "COG_1_02yo" ~ "COG_2_11wj",
    TRUE ~ geoloc_area_id)
  )

#p_coord_check <- plot_survey_coordinate_check(survey_clusters,
#                                               survey_region_boundaries,
#                                               survey_region_areas)

# dir.create("check")
# pdf(paste0("check/", tolower(iso3), "_dhs-cluster-check.pdf"), h = 5, w = 7)
# p_coord_check
# dev.off()

write_csv(survey_clusters, paste0("outside_orderly/survey/outputs/", tolower(iso3), "_dhs_clusters.csv"))

# mics_indicators <- read_csv("global/MICS_indicators.csv") %>%
mics_indicators <- read_csv("global/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id") %>%
  filter(survey_id != "CIV2000MICS")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

fertility_mics_data <- transform_mics(mics_survey_data, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      mics_area_name_label == "Cuvette Ouest" ~ "Cuvette-ouest",
      mics_area_name_label == "Pointe-Noire" ~ "Pointe-noire",
      TRUE ~ mics_area_name_label
    )
  ) 

mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

asfr_input_data <- make_asfr_inputs(mics_survey_areas, mics_survey_data)

write_csv(asfr_input_data$wm, paste0("outside_orderly/survey/outputs/", tolower(iso3), "_mics_women.csv"))
write_csv(asfr_input_data$births_to_women, paste0("outside_orderly/survey/outputs/", tolower(iso3), "_mics_births_to_women.csv"))


