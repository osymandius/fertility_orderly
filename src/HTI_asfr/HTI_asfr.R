areas_long <- "pointr_link_to_areas"
clusters <- "depends/HTI_dhs_survey.csv"

## Get surveys for which we have clusters. Split into country list.
surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% 
              select(c(DHS_survey_id, survey_id, iso3)) %>% 
              distinct, 
            by=c("SurveyId" = "DHS_survey_id")) %>%
  filter(iso3 == "HTI")

cluster_areas <- assign_cluster_area(clusters, 2)

dat <- clusters_to_surveys(surveys, cluster_areas, single_tips = TRUE)

asfr <- Map(calc_asfr, dat$ir,
            by = list(~country + surveyid + survtype + survyear + area_id),
            tips = dat$tips_surv,
            agegr= list(3:10*5),
            period = list(1995:2017),
            strata = list(NULL),
            counts = TRUE) %>%
  bind_rows %>%
  type.convert %>%
  filter(period<=survyear) %>%
  # rename(age_group = agegr) %>%
  mutate(iso3 = "HTI") %>%
  select(-country)

write_csv(asfr, "HTI_dhs_asfr.csv")