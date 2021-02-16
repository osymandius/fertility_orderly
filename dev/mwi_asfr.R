library(sf)
library(tidyverse)
library(dfertility)
library(naomi.utils)
library(naomi)
library(orderly)
library(rdhs)
library(demogsurv)

iso3 <- "MWI"

areas <- read_sf("archive/mwi_data_areas/20200929-213435-8f7790a3/mwi_areas.geojson")
clusters <- read_csv("archive/mwi_survey/20201203-162231-0619858e/mwi_dhs_clusters.csv")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

clusters <- clusters %>%
  mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3]))

surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id"))

cluster_list <- clusters %>%
  left_join(areas_wide %>% st_drop_geometry, by=c("geoloc_area_id" = "area_id")) %>%
  rename(area_id = area_id1) %>%
  select(survey_id, cluster_id, area_id) %>%
  group_by(survey_id) %>%
  group_split

names(cluster_list) <- surveys$survey_id

ir <- get_fertility_surveys(surveys)
names(ir) <- names(cluster_list)

dat <- map_ir_to_areas(ir, cluster_list)

asfr <- Map(calc_asfr, dat$ir,
            by = list(~survey_id + survtype + survyear + area_id),
            tips = dat$tips_surv,
            agegr= list(3:10*5),
            period = list(1995:2020),
            strata = list(NULL),
            varmethod = list("none"),
            counts = TRUE) %>%
  bind_rows %>%
  type.convert %>%
  filter(period<=survyear) %>%
    # rename(age_group = agegr) %>%
  mutate(iso3 = iso3) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

##

iso3 <- "UGA"

areas <- read_sf("archive/uga_data_areas/20201026-075718-2c4dfbae/uga_areas.geojson")
clusters <- read_csv("archive/uga_survey/20201203-162536-ff0b857c/uga_dhs_clusters.csv")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

clusters <- clusters %>%
  mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3])) %>%
  filter(survey_id != "UGA2011AIS")

surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id"))

cluster_list <- clusters %>%
  left_join(areas_wide %>% st_drop_geometry, by=c("geoloc_area_id" = "area_id")) %>%
  rename(area_id = area_id1) %>%
  select(survey_id, cluster_id, area_id) %>%
  group_by(survey_id) %>%
  group_split

names(cluster_list) <- surveys$survey_id

ir <- get_fertility_surveys(surveys)
names(ir) <- names(cluster_list)

dat <- map_ir_to_areas(ir, cluster_list)

asfr <- Map(calc_asfr, dat$ir,
            by = list(~survey_id + survtype + survyear + area_id),
            tips = dat$tips_surv,
            agegr= list(3:10*5),
            period = list(1995:2020),
            strata = list(NULL),
            varmethod = list("none"),
            counts = TRUE) %>%
  bind_rows %>%
  type.convert %>%
  filter(period<=survyear) %>%
  # rename(age_group = agegr) %>%
  mutate(iso3 = iso3) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

asfr <- asfr %>%
  filter(survtype == "DHS")
