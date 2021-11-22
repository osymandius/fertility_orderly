#' ISO3 country code
iso3 <- "RWA"

areas <- read_sf("depends/rwa_areas.geojson")

areas_wide <- spread_areas(areas)

surveys <- create_surveys_dhs(iso3, survey_characteristics = NULL) %>%
  filter(as.numeric(SurveyYear) > 1994)

# survey_meta <- create_survey_meta_dhs(surveys)

#' This is the contents of naomi.utils::create_survey_meta_dhs. Unpacked to get around 2 reports for 2005 DHS which fails the duplicated check
publications <- rdhs::dhs_publications(surveyIds = surveys$SurveyId)
final_rep <- dplyr::filter(publications, PublicationTitle == "Final Report")
final_rep <- dplyr::select(final_rep, SurveyId, report_url = PublicationURL) %>%
  dplyr::group_by(SurveyId) %>%
  dplyr::filter(row_number() == 1)

surveys <- dplyr::left_join(surveys, final_rep, by = "SurveyId")
surveys$dataset_url <- paste0("https://dhsprogram.com/methodology/survey/survey-display-", 
                              surveys$SurveyNum, ".cfm")
survey_meta <- dplyr::transmute(surveys, survey_id, country = CountryName, 
                                survey_type = SurveyType, survey_year = SurveyYear, 
                                fieldwork_start = FieldworkStart, fieldwork_end = FieldworkEnd, 
                                survey_mid_calendar_quarter, female_age_min = MinAgeWomen, 
                                female_age_max = MaxAgeWomen, male_age_min = MinAgeMen, 
                                male_age_max = MaxAgeMen, report_ref = NA_character_, 
                                report_url = report_url, dataset_url, notes = NA_character_)


#' The 2005 DHS was initially conducted based on 12 former prefectures. The data
#' were later remapped to the five new admin1 provinces. This provides a better
#' alignment to the current districts. 
survey_region_boundaries <-
  create_survey_boundaries_dhs(surveys,
                               levelrnk_select = c("RWA2005DHS" = 1))


#' Boundary file is missing from DHS spatial data repository for RW2017MIS.
#' From reviewing the HR dataset, shdistrict is included with 30 regions

hrd <- dhs_datasets(surveyIds = "RW2017MIS", fileType = "HR", fileFormat = "FL")
rw2015hr <- readRDS(get_datasets(hrd)[[1]])

as_factor(rw2015hr[c("hv024", "shdistrict")]) %>% distinct()
filter(survey_region_boundaries, survey_id == "RWA2015DHS") %>%
  st_drop_geometry() %>%
  arrange(survey_region_id)

rwa2017mis_reg <- rw2015hr %>%
  distinct(hv024, shdistrict) %>%
  transmute(survey_id = "RWA2017MIS", 
            survey_region_id = shdistrict,
            survey_region_name = as_factor(shdistrict),
            REGVAR = "shdistrict")

rwa2017mis_reg <-  survey_region_boundaries %>%
  filter(survey_id == "RWA2015DHS") %>%
  select(-survey_id) %>%
  right_join(rwa2017mis_reg,
             by = c("survey_region_id", "survey_region_name", "REGVAR"))

stopifnot(!st_is_empty(rwa2017mis_reg))

survey_region_boundaries <- rbind(survey_region_boundaries, rwa2017mis_reg)

surveys <- surveys_add_dhs_regvar(surveys, survey_region_boundaries)

#' Allocate each area to survey region

survey_region_areas <- allocate_areas_survey_regions(areas_wide, survey_region_boundaries)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries, warn = TRUE)

ggplot() +
  geom_sf(aes(fill = paste0(area_id2, ": ", area_name2)),
          data = filter(areas_wide, area_name1 == "Kigali City")) +
  geom_sf(data = survey_region_boundaries %>%
            filter(survey_id == "RWA2000DHS",
                   survey_region_name %in% c("kigali ville (pvk)", "kigali rurale")),
          fill=NA, size=1.5) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

dev.off()

#' The three current Kigali City districts overlap both the Kigali City and Kigali
#' Rural regions in the 2000 DHS.
#' Add additioanl region-area mappings for Kigali City region to RWA_2_11, RWA_2_12,
#' and RWA_2_13 to allow mapping of Kigali City clusters to any of these areas.


kigali_region_areas <- survey_region_areas %>%
  filter(survey_id == "RWA2000DHS",
         area_id1 %in% "RWA_1_1") %>%
  mutate(survey_region_id = 9,
         survey_region_name = "kigali ville (pvk)")

survey_region_areas <- bind_rows(survey_region_areas, kigali_region_areas)

validate_survey_region_areas(survey_region_areas, survey_region_boundaries)

survey_regions <- create_survey_regions_dhs(survey_region_areas)

#' # Survey clusters dataset

survey_clusters <- create_survey_clusters_dhs(surveys)

#' Snap survey clusters to areas

survey_clusters <- assign_dhs_cluster_areas(survey_clusters, survey_region_areas)

#' 2000 DHS, 2013 and 2017 MIS has no GPS coordinates. Assign clusters to areas using survey region id
survey_clusters <- survey_clusters %>%
  left_join(survey_regions %>% 
              filter(survey_id %in% c("RWA2000DHS", "RWA2013MIS", "RWA2017MIS"))
            ) %>%
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
