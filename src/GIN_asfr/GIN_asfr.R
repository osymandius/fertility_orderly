iso3 <- "GIN"

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
clusters <- read.csv(paste0("depends/", tolower(iso3), "_dhs_clusters.csv"))
source("resources/utility_funs.R")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

clusters <- clusters %>%
  mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3]))

surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id"))

# cluster_areas <- assign_cluster_area(clusters, areas_wide, 3)
# 
# dat <- clusters_to_surveys(iso3, surveys, cluster_areas, level = 3, single_tips = TRUE)

cluster_list <- clusters %>%
  rename(area_id = geoloc_area_id) %>%
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

write_csv(asfr, paste0(tolower(iso3), "_dhs_asfr.csv"))

### ADMIN-1 ASFR FOR PLOTTING

cluster_list_admin1 <- clusters %>%
  left_join(areas_wide %>% st_drop_geometry, by=c("geoloc_area_id" = "area_id")) %>%
  rename(area_id = area_id1) %>%
  select(survey_id, cluster_id, area_id) %>%
  group_by(survey_id) %>%
  group_split

names(cluster_list_admin1) <- surveys$survey_id

dat_admin1 <- map_ir_to_areas(ir, cluster_list_admin1, single_tips = FALSE)
dat_admin1$ir <- lapply(dat_admin1$ir, zap_labels)



asfr_admin1 <- Map(calc_asfr, dat_admin1$ir,
                   by = list(~survey_id + survtype + survyear + area_id),
                   tips = dat_admin1$tips_surv,
                   agegr= list(3:10*5),
                   period = list(1995:2020),
                   strata = list(NULL),
                   varmethod = list("none"),
                   counts = TRUE) %>%
  bind_rows %>%
  type.convert %>%
  filter(period<=survyear) %>%
    # rename(age_group = agegr) %>%
  mutate(iso3 = iso3,
         variable = "asfr") %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

# tfr_admin1 <- Map(calc_tfr, dat_admin1$ir,
#                   by = list(~survey_id + survtype + survyear + area_id),
#                   tips = dat_admin1$tips_surv,
#                   agegr= list(3:10*5),
#                   period = list(1995:2020)) %>%
#   bind_rows %>%
#   type.convert %>%
#   filter(period<=survyear) %>%
#   mutate(iso3 = iso3,
#          variable = "tfr")

tfr_admin1 <- asfr_admin1 %>%
  group_by(iso3, survey_id, survtype, survyear, area_id, period, tips, variable) %>%
  summarise(tfr = 5*sum(asfr)) %>%
  mutate(variable = "tfr")

#' ### MICS DATA
#' 
#' mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
#' mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))
#' 
#' mics_wm_asfr <- mics_wm %>%
#'   type.convert() %>%
#'   arrange(survey_id) %>%
#'   group_by(survey_id) %>%
#'   group_split()
#' 
#' mics_births_asfr <- mics_births_to_women %>%
#'   left_join(mics_wm) %>%
#'   arrange(survey_id) %>%
#'   group_by(survey_id) %>%
#'   group_split()
#' 
#' #' For model:
#' mics_asfr <- Map(calc_asfr, mics_wm_asfr,
#'                  by = list(~area_id + survey_id),
#'                  tips = list(c(0:15)),
#'                  agegr= list(3:10*5),
#'                  period = list(1995:2019),
#'                  clusters = list(~cluster),
#'                  strata = list(NULL),
#'                  id = list("unique_id"),
#'                  dob = list("wdob"),
#'                  intv = list("doi"),
#'                  weight = list("weight"),
#'                  varmethod = list("none"),
#'                  bhdata = mics_births_asfr,
#'                  bvars = list("cdob"),
#'                  counts = TRUE) %>%
#'   bind_rows %>%
#'   type.convert() %>%
#'   separate(col=survey_id, into=c(NA, "survyear", NA), sep=c(3,7), remove = FALSE, convert = TRUE) %>%
#'   filter(period <= survyear) %>%
#'   # rename(age_group = agegr) %>%
#'   mutate(survtype = "MICS",
#'          iso3 = iso3
#'   )  %>%
#'   left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
#'   select(-agegr)
#' 
#' #' MICS surveys in West Africa around 2005 only recorded up to 5 years preceding survey
#' mics_asfr <- mics_asfr %>%
#'   filter(!(survey_id == "BFA2006MICS" & period <= 2001))
#' 
#' # For plotting:
#' mics_asfr_plot <- Map(calc_asfr, mics_wm_asfr,
#'                       by = list(~area_id + survey_id),
#'                       tips = list(c(0,15)),
#'                       agegr= list(3:10*5),
#'                       period = list(1995:2019),
#'                       clusters = list(~cluster),
#'                       strata = list(NULL),
#'                       id = list("unique_id"),
#'                       dob = list("wdob"),
#'                       intv = list("doi"),
#'                       weight = list("weight"),
#'                       varmethod = list("none"),
#'                       bhdata = mics_births_asfr,
#'                       bvars = list("cdob"),
#'                       counts = TRUE) %>%
#'   bind_rows %>%
#'   type.convert() %>%
#'   separate(col=survey_id, into=c(NA, "survyear", NA), sep=c(3,7), remove = FALSE, convert = TRUE) %>%
#'   filter(period <= survyear) %>%
#'   # rename(age_group = agegr) %>%
#'   mutate(survtype = "MICS",
#'          iso3 = iso3,
#'          variable = "asfr"
#'   )  %>%
#'   left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
#'   select(-agegr)
#' 
#' mics_asfr_plot <- mics_asfr_plot %>%
#'   filter(!(survey_id == "BFA2006MICS" & period <= 2001))
#' 
#' mics_wm_tfr <- mics_wm_asfr %>%
#'   bind_rows %>%
#'   arrange(survey_id, area_id) %>%
#'   group_split(survey_id, area_id)
#' 
#' mics_births_tfr <- mics_births_asfr %>%
#'   bind_rows %>%
#'   arrange(survey_id, area_id) %>%
#'   group_split(survey_id, area_id)
#' 
#' mics_tfr <- Map(calc_tfr, mics_wm_tfr,
#'                 by = list(~area_id + survey_id),
#'                 tips = list(c(0,15)),
#'                 period = list(1995:2019),
#'                 clusters = list(~cluster),
#'                 strata = list(NULL),
#'                 id = list("unique_id"),
#'                 dob = list("wdob"),
#'                 intv = list("doi"),
#'                 weight = list("weight"),
#'                 bhdata = mics_births_tfr,
#'                 bvars = list("cdob")) %>%
#'   bind_rows %>%
#'   type.convert %>%
#'   mutate(iso3 = iso3,
#'          survtype = "MICS",
#'          variable = "tfr")
#' 
#' mics_tfr <- mics_tfr %>%
#'   filter(!(survey_id == "BFA2006MICS" & period <= 2001))
# 
# write_csv(mics_asfr, paste0(tolower(iso3), "_mics_asfr.csv"))
# 
# asfr <- asfr %>%
#   bind_rows(mics_asfr)

write_csv(asfr, paste0(tolower(iso3), "_asfr.csv"))

plot <- asfr_admin1 %>%
  # bind_rows(mics_asfr_plot) %>%
  select(-c(births, pys)) %>%
  rename(value = asfr) %>%
  bind_rows(
    bind_rows(tfr_admin1
              # , mics_tfr
              ) %>%
      # select(-se_tfr) %>%
      rename(value = tfr)
  )

write_csv(plot, paste0(tolower(iso3), "_fr_plot.csv"))