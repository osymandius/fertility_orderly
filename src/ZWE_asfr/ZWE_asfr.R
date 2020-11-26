areas <- read_sf("depends/zwe_areas.geojson")
clusters <- read.csv("depends/zwe_dhs_clusters.csv")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

iso3 <- "ZWE"

## Get surveys for which we have clusters. Split into country list.
clusters <- clusters %>%
  mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3]))

surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id"))

cluster_areas <- assign_cluster_area(clusters, areas_wide, 2)

dat <- clusters_to_surveys(iso3, surveys, cluster_areas, level = 2, single_tips = TRUE)

asfr <- Map(calc_asfr, dat$ir,
            by = list(~survey_id + survtype + survyear + area_id),
            tips = dat$tips_surv,
            agegr= list(3:10*5),
            period = list(1995:2017),
            strata = list(NULL),
            varmethod = list("none"),
            counts = TRUE) %>%
  bind_rows %>%
  type.convert %>%
  filter(period<=survyear) %>%
  rename(age_group = agegr) %>%
  mutate(iso3 = iso3)

write_csv(asfr, "zwe_dhs_asfr.csv")



mics_wm <- read.csv("depends/zwe_mics_women.csv") %>%
# mics_wm <- read.csv("draft/zwe_survey/20201123-204826-542293e7/zwe_mics_women.csv") %>%
  arrange(survey_id) %>%
  group_by(survey_id) %>%
  group_split()

mics_births_to_women <- read.csv("depends/zwe_mics_births_to_women.csv") %>%
# mics_births_to_women <- read.csv("draft/zwe_survey/20201123-204826-542293e7/zwe_mics_births_to_women.csv") %>% 
  arrange(survey_id) %>%
  group_by(survey_id) %>%
  group_split()

mics_asfr <- Map(calc_asfr, mics_wm,
                 by = list(~area_id + survey_id),
                 tips = list(c(0:15)),
                 agegr= list(3:10*5),
                 period = list(1995:2019),
                 clusters = list(~cluster),
                 strata = list(NULL),
                 id = list("unique_id"),
                 dob = list("wdob"),
                 intv = list("doi"),
                 weight = list("weight"),
                 varmethod = list("none"),
                 bhdata = mics_births_to_women,
                 bvars = list("cdob"),
                 counts = TRUE) %>%
  bind_rows %>%
  type.convert() %>%
  separate(col=survey_id, into=c(NA, "survyear", NA), sep=c(3,7), remove = FALSE, convert = TRUE) %>%
  filter(period <= survyear) %>%
  rename(age_group = agegr) %>%
  mutate(survtype = "MICS",
         iso3 = iso3
  )

write_csv(mics_asfr, "zwe_mics_asfr.csv")

asfr <- asfr %>%
  bind_rows(mics_asfr)

write_csv(asfr, "zwe_asfr.csv")