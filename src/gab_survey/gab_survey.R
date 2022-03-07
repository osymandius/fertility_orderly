#' ISO3 country code
iso3 <- "GAB"

areas <- read_sf("depends/gab_areas.geojson")
# areas <- read_sf("archive/gab_data_areas/20210118-065000-fca70c18/gab_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)


surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries %>% st_make_valid())

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

# Survey regions contained no areas:
#   survey_id survey_region_id     survey_region_name
# GAB2000DHS                1             big cities
# GAB2012DHS                1 libreville-port-gentil

#' Manually add the districts that intersect libreville-port-gentil
#' survey region to survey_region_areas
#' 
gab2000dhs_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "GAB2000DHS", survey_region_id == 1),
    left = FALSE
  ) %>%
    select(all_of(names(survey_region_areas)))

gab2012dhs_region1_areas <- areas_wide %>%
  st_join(
    survey_region_boundaries %>%
      filter(survey_id == "GAB2012DHS", survey_region_id == 1),
    left = FALSE
  ) %>%
  select(all_of(names(survey_region_areas)))

survey_region_areas <- survey_region_areas %>%
  bind_rows(gab2000dhs_region1_areas, gab2012dhs_region1_areas)

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
