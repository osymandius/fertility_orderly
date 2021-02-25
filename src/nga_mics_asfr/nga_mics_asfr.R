iso3 <- "NGA"

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

### MICS DATA

mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))

mics_wm_asfr <- mics_wm %>%
  type.convert() %>%
  arrange(survey_id, area_id) %>%
  group_split(survey_id, area_id)

mics_births_asfr <- mics_births_to_women %>%
  left_join(mics_wm) %>%
  arrange(survey_id, area_id) %>%
  group_split(survey_id, area_id)

#' For model:
mics_asfr <- Map(calc_asfr, mics_wm_asfr,
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
                 bhdata = mics_births_asfr,
                 bvars = list("cdob"),
                 counts = TRUE) %>%
  bind_rows %>%
  type.convert() %>%
  separate(col=survey_id, into=c(NA, "survyear", NA), sep=c(3,7), remove = FALSE, convert = TRUE) %>%
  filter(period <= survyear) %>%
  # rename(age_group = agegr) %>%
  mutate(survtype = "MICS",
         iso3 = iso3
  ) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

#' MICS surveys in West Africa around 2005-2010 only recorded up to 5 years preceding survey
mics_asfr <- mics_asfr %>%
  filter(!(survey_id == "NGA2007MICS" & period <= 2002),
         !(survey_id == "NGA2011MICS" & period <= 2006))

# For plotting:
mics_asfr_plot <- Map(calc_asfr, mics_wm_asfr,
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
                      bhdata = mics_births_asfr,
                      bvars = list("cdob"),
                      counts = TRUE) %>%
  bind_rows %>%
  type.convert() %>%
  separate(col=survey_id, into=c(NA, "survyear", NA), sep=c(3,7), remove = FALSE, convert = TRUE) %>%
  filter(period <= survyear) %>%
  # rename(age_group = agegr) %>%
  mutate(survtype = "MICS",
         iso3 = iso3,
         variable = "asfr"
  ) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

mics_asfr_plot <- mics_asfr_plot %>%
  filter(!(survey_id == "NGA2007MICS" & period <= 2002),
         !(survey_id == "NGA2011MICS" & period <= 2006))

mics_wm_tfr <- mics_wm_asfr %>%
  bind_rows %>%
  arrange(survey_id, area_id) %>%
  group_split(survey_id, area_id)

mics_births_tfr <- mics_births_asfr %>%
  bind_rows %>%
  arrange(survey_id, area_id) %>%
  group_split(survey_id, area_id)

mics_tfr <- Map(calc_tfr, mics_wm_tfr,
                by = list(~area_id + survey_id),
                tips = list(c(0,15)),
                period = list(1995:2019),
                clusters = list(~cluster),
                strata = list(NULL),
                id = list("unique_id"),
                dob = list("wdob"),
                intv = list("doi"),
                weight = list("weight"),
                bhdata = mics_births_tfr,
                bvars = list("cdob")) %>%
  bind_rows %>%
  type.convert %>%
  mutate(iso3 = iso3,
         survtype = "MICS",
         variable = "tfr")

mics_tfr <- mics_tfr %>%
  filter(!(survey_id == "NGA2007MICS" & period <= 2002),
         !(survey_id == "NGA2011MICS" & period <= 2006))

write_csv(mics_asfr, paste0(tolower(iso3), "_mics_asfr.csv"))
write_csv(mics_tfr, paste0(tolower(iso3), "_mics_tfr_admin1.csv"))
write_csv(mics_asfr_plot, paste0(tolower(iso3), "_mics_asfr_admin1.csv"))