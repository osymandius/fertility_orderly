#' Set country ISO code

iso3 <- "HTI"

# areas <- read_sf("depends/ben_areas.geojson")
areas <- read_sf("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/naomi-data/HTI/data/hti_areas.geojson")

areas_wide <-  spread_areas(st_drop_geometry(areas)) %>%
  left_join(
    select(areas, area_id),
    by = "area_id") %>%
  st_as_sf()

surveys <- dhs_surveys(countryIds = dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3],
                       surveyType = c("DHS", "AIS", "MIS")) %>%
  filter(SurveyYear > 1995) %>%
  mutate(survey_id = paste0(iso3, SurveyYear, SurveyType),
         survey_mid_calendar_quarter = get_mid_calendar_quarter(as.Date(FieldworkStart)+15, as.Date(FieldworkEnd)+15))

#' DHS survey boundaries dataset
#' * Read DHS shapefiles to get boundaries

geom_raw <- Map(download_boundaries, surveyId = surveys$SurveyId, method = "sf") %>%
  unlist(recursive = FALSE)

geom <- geom_raw %>%
  lapply(select, DHSCC, SVYID, REG_ID, MULTLEVEL, LEVELRNK, REGVAR, REGCODE, REGNAME, OTHREGVAR, OTHREGCO, OTHREGNA) %>%
  do.call(rbind, .) %>%
  mutate_at(vars(-geometry), ~replace(., . == "NULL", NA)) %>%
  left_join(surveys %>% select(SurveyId, SurveyNum),
            by = c("SVYID" = "SurveyNum"))

#' Surveys with multiple areas: choose level with larger number of areas unless reason not to.
geom <- geom %>%
  inner_join(
    geom %>%
      as.data.frame %>%
      count(SurveyId, MULTLEVEL, LEVELRNK) %>%
      group_by(SurveyId) %>%
      filter(n == max(n)) %>%
      select(-n),
    by = c("MULTLEVEL", "LEVELRNK", "SurveyId")
  )

survey_regions <- geom %>%
  left_join(
    select(surveys, survey_id, SurveyId),
    by = "SurveyId"
  ) %>%
  select(survey_id,
         survey_region_id = REGCODE,
         survey_region_name = REGNAME)

#' Allocate each area to survey region
survey_region_areas <- survey_regions %>%
  st_drop_geometry() %>%
  full_join(
    survey_regions %>%
      split(.$survey_id) %>%
      lapply(st_make_valid) %>%
      Map(st_join, list(select(areas_wide, contains("area_id"))), ., largest = TRUE) %>%
      do.call(rbind, .),
    by = c("survey_id", "survey_region_id", "survey_region_name")
  )

#' Check any unallocated areas
survey_region_areas %>%
  filter(is.na(survey_region_id))

stopifnot(!is.na(survey_region_areas$survey_region_id))

#' Check any survey regions with no areas
survey_region_areas %>%
  filter(is.na(area_id))

stopifnot(!is.na(survey_region_areas$area_id))


#' Identify survey_region_area_id
survey_regions <- survey_region_areas %>%
  select(survey_id, survey_region_id, survey_region_name,
         dplyr::matches("area_id[0-9]+")) %>%
  gather(level, survey_region_area_id,
         contains("area_id"), factor_key = TRUE) %>%
  mutate(level = fct_rev(level)) %>%
  distinct() %>%
  group_by(survey_id, survey_region_id, level) %>%
  filter(n() == 1) %>%
  arrange(survey_id, survey_region_id, level) %>%
  group_by(survey_id, survey_region_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(-level)

#' # DHS clusters dataset

hrd <- dhs_datasets(surveyIds = surveys$SurveyId, fileType = "HR", fileFormat = "flat") %>%
  mutate(path = get_datasets(.) %>% unlist) %>%
  left_join(
    geom %>%
      st_drop_geometry() %>%
      distinct(SurveyId, REGVAR) %>%
      mutate(REGVAR = sub("^v024", "hv024", REGVAR)),
    by = "SurveyId"
  ) %>%
  left_join(
    select(surveys, SurveyId, survey_id),
    by = "SurveyId"
  )

extract_clusters <- function(path, survey_id, REGVAR){
  
  print(survey_id)
  
  hr <- readRDS(path)
  class(hr) <- "data.frame"
  val <- hr %>%
    transmute(survey_id,
              cluster_id = hv001,
              survey_region_id = .data[[REGVAR]],
              restype = factor(hv025, 1:2, c("urban", "rural"))) %>%
    distinct() %>%
    type.convert()
  
  val
}

# extract_clusters(hrd$path[5], hrd$survey_id[5], hrd$REGVAR[5]) %>% type.convert %>%str

hrclust <- Map(extract_clusters, hrd$path, hrd$survey_id, hrd$REGVAR) %>%
  bind_rows()

#' Check that all region IDs appear in survey_regions dataset
hrclust %>%
  left_join(survey_regions) %>%
  filter(is.na(survey_id))

#' Add geo-coordinates
ged <- dhs_datasets(surveyIds = surveys$SurveyId,
                    fileType = "GE", fileFormat = "flat") %>%
  left_join(select(surveys, SurveyId, survey_id)) %>%
  mutate(path = get_datasets(.) %>% unlist)

ge <- lapply(ged$path, readRDS) %>%
  lapply(as_tibble) %>%
  Map(f = mutate,
      survey_id = ged$survey_id,
      SurveyYear = ged$SurveyYear,
      SurveyType = ged$SurveyType,
      CountryName = ged$CountryName) %>%
  Map(replace, ., lapply(., `==`, "NULL"), NA) %>%
  lapply(type.convert) %>%
  bind_rows() %>%
  st_as_sf(coords = c("LONGNUM", "LATNUM"), remove = FALSE)

#' Clusters missing coordinates
ge %>%
  group_by(survey_id) %>%
  summarise(n(),
            sum(LONGNUM == 0))

survey_clusters <- hrclust %>%
  left_join(
    ge %>%
      filter(LONGNUM != 0) %>%
      select(survey_id,
             cluster_id = DHSCLUST,
             longitude = LONGNUM,
             latitude = LATNUM)
  )

#' ## Snap coordinates to areas
#'
#' survey_region_areas is a list of candidate location areas
#' for each cluster. Join candidate areas and then select
#' the nearest area based on distance. Usually the coordinate
#' should be contained (distance = 0)

survey_clusters <- survey_clusters %>%
  mutate(regcode_match = if_else(is.na(longitude), NA_integer_, survey_region_id)) %>%
  left_join(
    survey_region_areas %>%
      select(survey_id, regcode_match = survey_region_id, area_id,
             geometry_area = geometry)
  ) %>%
  mutate(distance = parallel::mcMap(st_distance, geometry, geometry_area, mc.cores = 1) %>% unlist)

#' Check clusters contained in >1 area
survey_clusters %>%
  filter(distance == 0) %>%
  group_by(survey_id, cluster_id) %>%
  filter(n() > 1)

survey_clusters <- survey_clusters %>%
  arrange(distance) %>%
  group_by(survey_id, cluster_id) %>%
  filter(row_number() == 1) %>%
  ungroup %>%
  transmute(survey_id,
            cluster_id = cluster_id,
            restype,
            survey_region_id,
            longitude,
            latitude,
            geoloc_area_id = area_id,
            geoloc_distance = distance)

write_csv(survey_clusters %>% filter(!is.na(geoloc_area_id)), "gin_survey_clusters.csv")