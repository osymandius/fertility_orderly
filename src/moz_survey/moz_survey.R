# orderly_pull_archive("moz_data_areas")

#' ISO3 country code
iso3 <- "MOZ"

areas <- read_sf("depends/moz_areas.geojson")
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

#' 1997 and 2003 DHS have no GPS coordinates. Assign clusters to areas using survey region id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions %>% 
              filter(survey_id %in% c("MOZ1997DHS", "MOZ2003DHS"))) %>%
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

## MICS SURVEYS

#' Code for single survey: 2008 MICS
#' The MICS code is different here compared to other countries because the survey has a unique_id column - "memid"
#' The birth history datasets do not have women line number, so code adapted to use the built in unique_id instead.

mics_indicators <- read_csv("resources/MICS_indicators.csv") %>%
  pivot_longer(-c(label, id, filetype), names_to = "survey_id")

mics_survey_data <- create_surveys_mics(iso3, mics_indicators)

filter_mics_moz <- function(dat, mics_indicators) {
  
  indicators <- filter(mics_indicators,
                       survey_id == "MOZ2008MICS",
                       label != "dataset name"
  )
  
  wm <- dat$wm
  colnames(wm) <- tolower(colnames(wm))
  wm <- wm %>%
    select(c(filter(indicators, filetype == "wm")$value), "memid")
  colnames(wm) <- c(filter(indicators, filetype == "wm")$id, "unique_id")
  
  bh <- dat$bh
  colnames(bh) <- tolower(colnames(bh))
  bh <- bh %>%
    select(filter(indicators, filetype == "bh")$value)
  colnames(bh) <- c("unique_id", "cdob")
  
  
  hh <- dat$hh
  colnames(hh) <- tolower(colnames(hh))
  hh <- hh %>%
    select(filter(indicators, filetype == "hh")$value)
  colnames(hh) <- filter(indicators, filetype == "hh")$id
  
  df <- list()
  df$wm <- wm %>%
    mutate(survey_id = "MOZ2008MICS") %>%
    filter(!is.na(wdob), !is.na(unique_id), !is.na(doi))
  
  df$bh <- bh %>%
    mutate(survey_id = "MOZ2008MICS")
  
  df$hh <- hh %>%
    mutate(survey_id = "MOZ2008MICS",
           area_level = as.numeric(filter(indicators, id == "mics_area_level")$value)
    )
  
  return(df)
  
  
}

fertility_mics_data <- filter_mics_moz(mics_survey_data[[1]], mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  left_join(data.frame(mics_area_name = attr(fertility_mics_data$hh$mics_area_name, "labels"),
                           mics_area_name_label = str_to_title(
                             names(attr(fertility_mics_data$hh$mics_area_name, "labels")))
      )
  ) %>%
  select(-mics_area_name)

# fertility_mics_data <- transform_mics(mics_dat, mics_indicators)

fertility_mics_data$hh <- fertility_mics_data$hh %>%
  mutate(
    mics_area_name_label = case_when(
      survey_id == "MOZ2008MICS" & mics_area_name_label == "Maputo Cidade" ~ "Maputo",
      survey_id == "MOZ2008MICS" & mics_area_name_label == "Maputo Província" ~ "Maputo",
      TRUE ~ mics_area_name_label
    )
  )
 
mics_survey_areas <- join_survey_areas(fertility_mics_data, areas)

wm <- mics_survey_areas$wm %>%
  left_join(mics_survey_areas$hh %>% select(survey_id, cluster, hh_number, area_id))

births_to_women <- mics_survey_areas$wm %>%
  left_join(mics_survey_areas$bh) %>%
  select(survey_id, unique_id, cdob) %>%
  filter(!is.na(cdob))

write_csv(wm, paste0(tolower(iso3), "_mics_women.csv"))
write_csv(births_to_women, paste0(tolower(iso3), "_mics_births_to_women.csv"))