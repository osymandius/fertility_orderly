iso3 <- "TZA"

areas <- read_sf("depends/tza_areas.geojson")
areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

survey_meta <- create_survey_meta_dhs(surveys)

survey_region_boundaries <- create_survey_boundaries_dhs(surveys)

#' 1999 DHS has 3 admin areas in the Spatial Data Repository: Mainland, Pemba & Unguja.
#' Boundaries dataset codes 9999 NA as the mainland, and the survey dataset codes 1 and 2 as urban/rural mainland.
#' Collapse 1 and 2 into a single mainland boundary [1], recode pemba and unguja as 2 and 3, and recode cluster dataset below to reflect changes.
survey_region_boundaries <- survey_region_boundaries %>%
  mutate(
    survey_region_id = case_when(
      survey_id == "TZA1999DHS" & survey_region_id == 9999 ~ 1,
      survey_id == "TZA1999DHS" & survey_region_name == "pemba" ~ 2,
      survey_id == "TZA1999DHS" & survey_region_name == "unguja" ~ 3,
      TRUE ~ survey_region_id
    ),
    survey_region_name = case_when(
      survey_id == "TZA1999DHS" & is.na(survey_region_name) ~ "mainland",
      TRUE ~ survey_region_name
    )
  )


#' Boundary file for 2007 AIS has single region for Pemba and single region for Unguja, both coded 9999
#' But hv024 in the HR dataset for 2007 AIS has five regions (survey_region_id = 51:55); these are the
#' same as the 2010 DHS.
#' ACTION: Replace regions coded 9999 from the 2007 AIS with regions 51:55 from the 2010 DHS.

hrd <- dhs_datasets(surveyIds = "TZ2007AIS", fileType = "HR", fileFormat = "FL")
tz2007hr <- readRDS(get_datasets(hrd, clear_cache = TRUE)[[1]])

count(tz2007hr, hv024)

survey_region_boundaries <- survey_region_boundaries %>%
  filter( !(survey_id == "TZA2007AIS" & survey_region_id == 9999) ) %>%
  bind_rows(
    survey_region_boundaries %>%
    filter(survey_id == "TZA2010DHS",  survey_region_id %in% 51:55) %>%
    mutate(survey_id = "TZA2007AIS")
  )

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)


#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys, clear_rdhs_cache = TRUE)

survey_clusters <- survey_clusters %>%
  mutate(
    survey_region_id = as.double(survey_region_id),
    survey_region_id = case_when(
      survey_id == "TZA1999DHS" & survey_region_id == 2 ~ 1,
      survey_id == "TZA1999DHS" & survey_region_id == 3 ~ 2,
      survey_id == "TZA1999DHS" & survey_region_id == 4 ~ 3,
      TRUE ~ survey_region_id
    )
  )

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' 1996 and 2004 DHS have survey region ids as part of boundary and cluster files, but no GPS coordinates
#' Assign clusters to best matching area IDs without using GPS coordinates
survey_clusters <- survey_clusters %>%
  left_join(survey_regions %>% 
              filter(survey_id %in% c("TZA1996DHS", "TZA2004DHS"))) %>%
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
