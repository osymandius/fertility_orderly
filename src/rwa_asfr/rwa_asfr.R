areas_long <- "pointr_link_to_areas"
clusters <- read.csv("depends/RWA_dhs_survey.csv")

## Get surveys for which we have clusters. Split into country list.
surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% 
              select(c(DHS_survey_id, survey_id, iso3)) %>% 
              distinct, 
            by=c("SurveyId" = "DHS_survey_id")) %>%
  filter(iso3 == "RWA")

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
  rename(age_group = agegr) %>%
  mutate(iso3 = "RWA") %>%
  select(-country)

write_csv(asfr, "RWA_dhs_asfr.csv")

mics_dat <- read.csv("depends/RWA_mics_dat.csv")

mics_asfr <- Map(calc_asfr, mics_dat$wm,
                 by = list(~area_id + survey_id),
                 tips = list(c(0,15)),
                 agegr= list(3:10*5),
                 period = list(1995:2019),
                 clusters = list(~cluster),
                 strata = list(NULL),
                 id = list("unique_id"),
                 dob = list("wdob"),
                 intv = list("doi"),
                 weight = list("weight"),
                 varmethod = list("none"),
                 bhdata = bh_df,
                 bvars = list("cdob"),
                 counts = TRUE) %>%
  bind_rows %>%
  type.convert() %>%
  separate(col=survey_id, into=c(NA, "survyear", NA), sep=c(3,7), remove = FALSE, convert = TRUE) %>%
  filter(period <= survyear) %>%
  rename(age_group = agegr)

write_csv(mics_asfr, "RWA_mics_asfr.csv")