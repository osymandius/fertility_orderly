iso3 <- "AGO"

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
clusters <- read.csv(paste0("depends/", tolower(iso3), "_dhs_clusters.csv"))
source("resources/utility_funs.R")

# areas <- read_sf("archive/ago_data_areas/20210105-150243-778fa342/ago_areas.geojson")
# clusters <- read.csv("archive/ago_survey/20210121-173659-217468e6/ago_dhs_clusters.csv")

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

convert_to_aggregate_tips <- function(tips_list) {
  c(first(tips_list), last(tips_list))
}

tips_surv_aggregate <- lapply(dat$tips_surv, convert_to_aggregate_tips)

asfr_plot_raw <- Map(calc_asfr, dat$ir,
            by = list(~survey_id + survtype + survyear + area_id),
            tips = tips_surv_aggregate,
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
write_csv(asfr, paste0(tolower(iso3), "_asfr.csv"))