iso3 <- "RWA"

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

tfr_admin1 <- Map(calc_tfr, dat_admin1$ir,
                  by = list(~survey_id + survtype + survyear + area_id),
                  tips = dat_admin1$tips_surv,
                  agegr= list(3:10*5),
                  period = list(1995:2020)) %>%
  bind_rows %>%
  type.convert %>%
  filter(period<=survyear) %>%
  mutate(iso3 = iso3,
         variable = "tfr")


plot_dat <- asfr_admin1 %>%
  select(-c(births, pys)) %>%
  rename(value = asfr) %>%
  bind_rows(
    tfr_admin1 %>%
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

write_csv(plot_dat, paste0(tolower(iso3), "_fr_plot.csv"))
write_csv(asfr, paste0(tolower(iso3), "_asfr.csv"))