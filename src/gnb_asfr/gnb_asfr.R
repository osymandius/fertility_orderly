iso3 <- "GNB"

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
# clusters <- read.csv(paste0("depends/", tolower(iso3), "_dhs_clusters.csv"))
source("resources/utility_funs.R")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

### MICS DATA

mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

mics_wm_asfr <- mics_wm %>%
  type.convert() %>%
  arrange(survey_id) %>%
  group_by(survey_id) %>%
  group_split()

mics_births_asfr <- mics_births_to_women %>%
  left_join(mics_wm) %>%
  arrange(survey_id) %>%
  group_by(survey_id) %>%
  group_split()

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

mics_wm_tfr <- mics_wm_asfr %>%
  lapply(aggregate_mics_admin1, areas, areas_wide, admin1_lvl) %>%
  bind_rows() %>%
  arrange(survey_id, area_id) %>%
  mutate(survey_id = factor(survey_id),
         area_id = factor(area_id)) %>%
  group_split(survey_id, area_id)

mics_births_tfr <- mics_births_asfr %>%
  lapply(aggregate_mics_admin1, areas, areas_wide, admin1_lvl) %>%
  bind_rows() %>%
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


write_csv(mics_asfr %>%
            filter(!(survey_id == "GNB2018MICS" & period == 2003)), paste0(tolower(iso3), "_mics_asfr.csv"))

asfr <- mics_asfr

write_csv(asfr %>%
            filter(!(survey_id == "GNB2018MICS" & period == 2003)), paste0(tolower(iso3), "_asfr.csv"))

plot_dat <- mics_asfr_plot %>%
  select(-c(births, pys)) %>%
  rename(value = asfr) %>%
  bind_rows(
    bind_rows(mics_tfr) %>%
      select(-se_tfr) %>%
      rename(value = tfr)
  )

plot <- plot_dat %>%
  filter(variable == "tfr", value <10) %>%
  ggplot(aes(x=period, y=value, color=survey_id)) +
  geom_point() +
  facet_wrap(~area_id, ncol=5) +
  labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size=14)
  )

dir.create("check")
pdf("check/tfr_admin1.pdf", h = 12, w = 20)
plot
dev.off()

write_csv(plot_dat %>%
            filter(!(survey_id == "GNB2018MICS" & period == 2003)), paste0(tolower(iso3), "_fr_plot.csv"))