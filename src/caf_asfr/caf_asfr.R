iso3 <- "CAF"

source("resources/utility_funs.R")

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

areas_wide <- spread_areas(areas)

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
  )  %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

#' MICS surveys in West Africa around 2005 only recorded up to 5 years preceding survey
mics_asfr <- mics_asfr %>%
  filter(tips<5)

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
  )  %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label), by=c("agegr" = "age_group_label")) %>%
  select(-agegr)

mics_asfr_plot <- mics_asfr_plot %>%
  group_by(survey_id) %>%
  filter(period >= survyear-4)

write_csv(mics_asfr, paste0(tolower(iso3), "_mics_asfr.csv"))

asfr <- mics_asfr

write_csv(asfr, paste0(tolower(iso3), "_asfr.csv"))

if(!exists("mics_asfr_plot"))
  mics_asfr_plot <- data.frame()

if(!exists("asfr_plot_raw"))
  asfr_plot_raw <- data.frame()

asfr_plot_raw <- asfr_plot_raw %>% bind_rows(mics_asfr_plot)

asfr_plot <- asfr_plot_raw %>%
  aggregate_to_admin(c("survey_id", "survyear", "period", "survtype", "iso3", "age_group"), c("births", "pys"), 1, areas) %>%
  bind_rows(
    asfr_plot_raw %>% 
      aggregate_to_admin(c("survey_id", "survyear", "period", "survtype", "iso3", "age_group"), c("births", "pys"), 0, areas)
  ) %>%
  mutate(value = births/pys,
         variable = "asfr")

tfr_plot <- asfr_plot %>%
  group_by(iso3, survey_id, area_id, area_name, period, survtype) %>%
  summarise(value = 5*sum(value)) %>%
  mutate(variable = "tfr")

plot_dat <- bind_rows(asfr_plot, tfr_plot)

plot <- plot_dat %>%
  filter(variable == "tfr", value<15) %>%
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

write_csv(plot_dat, paste0(tolower(iso3), "_fr_plot.csv"))