
#' ## Survey meta data

iso3 <- "CIV"
country <- "Cote d'Ivoire"
survey_id  <- "CIV2017PHIA"
survey_mid_calendar_quarter <- "CY2017Q4"
fieldwork_start <- NA
fieldwork_end <- NA


#' ## Load area hierarchy
areas <- readRDS("resources/areas.rds")[[iso3]]

#' ## Load PHIA datasets
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

phia_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/household surveys/PHIA/datasets/CIV/datasets"

paths <- list(geo = "CIPHIA 2017-2018 PR Geospatial Data 20210804.zip",
              hh = "102 CIPHIA 2017-2018 Household Dataset (DTA).zip",
              ind = "202 CIPHIA 2017-2018 Adult Interview Dataset (DTA).zip",
              bio = "302 CIPHIA 2017-2018 Adult Biomarker Dataset (DTA).zip",
              chind = "205 CIPHIA 2017-2018 Child Interview Dataset (DTA).zip",
              chbio = "305 CIPHIA 2017-2018 Child Biomarker Dataset (DTA).zip") %>%
  lapply(function(x) file.path(phia_path, x)) %>%
  c(civ_gadm1 = "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/shape%20files/gadm/v3.6/gadm36_CIV_1_sf.rds") %>%
  lapply(URLencode)

phia_files <- lapply(paths, sharepoint$download)

geo <- rdhs::read_zipdata(phia_files$geo)

hh <- rdhs::read_zipdata(phia_files$hh)
bio <- rdhs::read_zipdata(phia_files$bio)
ind <- rdhs::read_zipdata(phia_files$ind)
chbio <- rdhs::read_zipdata(phia_files$chbio)
chind <- rdhs::read_zipdata(phia_files$chind)


#' Note: religion and ethnicity not asked

phia <- ind %>%
  filter(indstatus == 1) %>%  # Respondent
  select(centroidid, district, urban, householdid,
         personid, surveystyear, surveystmonth,
         intwt0, gender, age,
         mcstatus, mcage, mcwho) %>%
  full_join(
    bio %>%
    filter(bt_status == 1) %>%
    select(personid, btwt0, hivstatusfinal, arvstatus, artselfreported,
           vls, cd4count, recentlagvlarv),
    by = "personid"
  ) %>%
  mutate(survey_id = survey_id,
         cluster_id = centroidid)

chphia <- chind %>%
  filter(indstatus == 1) %>%
  select(centroidid, district, urban, householdid,
         personid, surveystyear, surveystmonth,
         intwt0, gender, age, agem) %>%
  full_join(
    chbio %>%
    filter(bt_status == 1) %>%
    select(personid, btwt0, hivstatusfinal, arvstatus, pedartparentreported,
           vls, cd4count, recentlagvlarv),
    by = "personid"
  ) %>%
  mutate(survey_id = survey_id,
         cluster_id = centroidid)



#' ## Survey regions
#'
#' This table identifies the smallest area in the area hierarchy which contains each
#' region in the survey stratification, which is the smallest area to which a cluster
#' can be assigned with certainty.

hh$survey_region_id <- hh$district # different in each survey dataset depending on survey stratification

survey_region_id <- c("Abidjan" = 1,
                      "Yamoussoukro" = 2,
                      "Bas-Sassandra" = 3,
                      "Comoé" = 4,
                      "Denguélé" = 5,
                      "Gôh-Djiboua" = 6,
                      "Lacs" = 7,
                      "Lagunes" = 8,
                      "Montagnes" = 9,
                      "Sassandra-Marahoué" = 10,
                      "Savanes" = 11,
                      "Vallée du Bandama" = 12,
                      "Woroba" = 13,
                      "Zanzan" = 14)

areas %>%
  filter(area_level == 1) %>%
  select(area_id, area_name) %>%
  st_drop_geometry() %>%
  print(n = Inf)

civ_gadm1 <- readRDS(phia_files$civ_gadm1)

survey_regions <- civ_gadm1 %>%
  transmute(survey_id = survey_id,
            survey_region_name = NAME_1,
            survey_region_id = recode(survey_region_name, !!!survey_region_id)) %>%
  st_as_sf()



#' Allocate each area to survey region
areas_wide <- naomi::spread_areas(areas)

survey_region_areas <- survey_regions %>%
  st_drop_geometry() %>%
  full_join(
    select(areas_wide, contains("area_id")) %>%
    st_join(survey_regions, largest = TRUE)
  )

#' Check any unallocated areas
stopifnot(nrow(filter(survey_region_areas, is.na(survey_region_id))) == 0)

#' Check any survey regions with no areas
stopifnot(nrow(filter(survey_region_areas, is.na(area_id))) == 0)

p_survey_regions <- survey_region_areas %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = survey_region_name), alpha = 0.7, size = 0.25, color = "grey60") +
  geom_sf(data = civ_gadm1, fill = NA) +
  th_map() +
  ggtitle("Fill = aggregated health districts\nLines = GADM survey region boundaries")

ggsave("civ2017phia_survey-region-alignment.pdf", p_survey_regions, h = 7, w = 8)

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


#' *** Should not require edits beyond this point ***

#' ## Survey clusters dataset
#'
#' This data frame maps survey clusters to the highest level in the area hiearchy
#' based on geomasked cluster centroids, additionally checking that the geolocated
#' areas are contained in the survey region.

survey_clusters <- hh %>%
  transmute(survey_id = survey_id,
            cluster_id = centroidid,
            cluster_id,
            res_type = factor(urban, 1:2, c("urban", "rural")),
            survey_region_id) %>%
  distinct() %>%
  left_join(geo, by = c("cluster_id" = "centroidid")) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), remove = FALSE) %>%
  sf::`st_crs<-`(4326)


#' Snap clusters to areas
#' 
#' This is slow because it maps to the lowest level immediately
#' It would be more efficient to do this recursively through
#' the location hierarchy tree -- but not worth the effort right now.

#' Create a list of all of the areas within each survey region
#' (These are the candidate areas where a cluster could be located)

survey_region_areas  <- survey_region_areas %>%
  select(survey_region_id, area_id, geometry_area = geometry)

#' Calculate distance to each candidate area for each cluster

survey_clusters <- survey_clusters %>%
  left_join(survey_region_areas, by = "survey_region_id") %>%
  mutate(
    distance = unlist(Map(sf::st_distance, geometry, geometry_area))
  )

#' Keep the area with the smallest distance from cluster centroid.
#' (Should be 0 for almost all)

survey_clusters <- survey_clusters %>%
  arrange(distance) %>%
  group_by(survey_id, cluster_id) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame() %>%
  transmute(survey_id,
            cluster_id,
            res_type,
            survey_region_id,
            longitude,
            latitude,
            geoloc_area_id = area_id,
            geoloc_distance = distance)

#' Review clusters outside admin area

survey_clusters %>%
  filter(geoloc_distance > 0) %>%
  arrange(-geoloc_distance) %>%
  left_join(survey_regions) 

#' ## Survey individuals dataset

mcstatus_lables <- c(`1` = "Yes",
                     `2` = "No",
                     `-8` = "Don't know",
                     `-9` = "Refused")

mcwho_labels = c(`1` = "Doctor, clinical officer, or nurse",
                 `2` = "Traditional practitioner / circumciser",
                 `3` = "Midwife",
                 `4` = "Family/Friend",
                 `96` = "Other",
                 `-8` = "Don't know",
                 `-9` = "Refused")

mcwho_recode = c(`1` = "Healthcare worker",
                 `2` = "Traditional practitioner",
                 `3` = "Traditional practitioner",   ## TODO: UNSURE ON THIS
                 `4` = "Traditional practitioner",   ## TODO: UNSURE ON THIS
                 `96` = "Traditional practitioner",
                 `-8` = NA_character_,
                 `-9` = NA_character_)


#' Create individuals data

survey_individuals <-
  bind_rows(
    phia %>%
    transmute(
      survey_id,
      cluster_id,
      individual_id = personid,
      household = householdid,
      line = personid,
      interview_cmc = 12 * (surveystyear - 1900) + surveystmonth,
      sex = factor(gender, 1:2, c("male", "female")),
      age,
      dob_cmc = NA,
      indweight = intwt0
    ),
    chphia %>%
    transmute(
      survey_id,
      cluster_id,
      individual_id = personid,
      household = householdid,
      line = personid,
      interview_cmc = 12 * (surveystyear - 1900) + surveystmonth,
      sex = factor(gender, 1:2, c("male", "female")),
      age,
      dob_cmc = interview_cmc - agem,
      indweight = intwt0
    )
  ) %>%
  mutate(age = as.integer(age),
         indweight = indweight / mean(indweight, na.rm=TRUE)) 


survey_biomarker <-
  bind_rows(
    phia %>%
    filter(!is.na(hivstatusfinal)) %>%
    transmute(
      survey_id,
      individual_id = personid,
      hivweight = btwt0,
      hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                            hivstatusfinal == 2 ~ 0),
      arv = case_when(arvstatus == 1 ~ 1,
                      arvstatus == 2 ~ 0),
      artself = case_when(artselfreported == 1 ~ 1,
                          artselfreported == 2 ~ 0),
      vls = case_when(vls == 1 ~ 1,
                      vls == 2 ~ 0),
      cd4 = cd4count,
      recent = case_when(recentlagvlarv == 1 ~ 1,
                         recentlagvlarv == 2 ~ 0)
    )
   ,
    chphia %>%
    filter(!is.na(hivstatusfinal)) %>%
    transmute(
      survey_id,
      individual_id = personid,
      hivweight = btwt0,
      hivstatus = case_when(hivstatusfinal == 1 ~ 1,
                            hivstatusfinal == 2 ~ 0),
      arv = case_when(arvstatus == 1 ~ 1,
                      arvstatus == 2 ~ 0),
      artself = case_when(pedartparentreported == 1 ~ 1,
                          pedartparentreported == 2 ~ 0),
      vls = case_when(vls == 1 ~ 1,
                      vls == 2 ~ 0),
      cd4 = cd4count,
      recent = case_when(recentlagvlarv == 1 ~ 1,
                         recentlagvlarv == 2 ~ 0)
    )
  ) %>%
  mutate(hivweight = hivweight / mean(hivweight, na.rm = TRUE))


survey_circumcision <- phia %>%
  filter(gender == 1) %>%
  transmute(
    survey_id,
    individual_id = personid,
    circumcised = recode(mcstatus, `2` = 0L , `1` = 1L, .default = NA_integer_),
    circ_age = mcage,
    circ_where = NA_character_,
    circ_who = recode(mcwho, !!!mcwho_recode)
  )



survey_meta <- survey_individuals %>%
  group_by(survey_id) %>%
  summarise(female_age_min = min(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            female_age_max = max(if_else(sex == "female", age, NA_integer_), na.rm=TRUE),
            male_age_min = min(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            male_age_max = max(if_else(sex == "male", age, NA_integer_), na.rm=TRUE),
            .groups = "drop") %>%
  mutate(iso3 = substr(survey_id, 1, 3),
         country = country,
         survey_type = "PHIA",
         survey_mid_calendar_quarter = recode(iso3, "MWI" = survey_mid_calendar_quarter),
         fieldwork_start = fieldwork_start,
         fieldwork_end   = fieldwork_end)

#' ## Save survey datasets

write_csv(survey_meta, paste0(tolower(survey_id), "_survey_meta.csv"), na = "")
write_csv(survey_regions, paste0(tolower(survey_id), "_survey_regions.csv"), na = "")
write_csv(survey_clusters, paste0(tolower(survey_id), "_survey_clusters.csv"), na = "")
write_csv(survey_individuals, paste0(tolower(survey_id), "_survey_individuals.csv"), na = "")
write_csv(survey_biomarker, paste0(tolower(survey_id), "_survey_biomarker.csv"), na = "")
write_csv(survey_circumcision, paste0(tolower(survey_id), "_survey_circumcision.csv"), na = "")
