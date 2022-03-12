iso3_c <- iso3
areas <- read_sf("depends/naomi_areas.geojson")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

mapping <- read_csv("resources/iso_mapping_fit.csv")
dhs_flag <- filter(mapping, iso3 == iso3_c)$dhs
mics_flag <- filter(mapping, iso3 == iso3_c)$mics

if(dhs_flag == 1) {
  clusters <- read.csv(paste0("depends/", tolower(iso3), "_dhs_clusters.csv"))
  
  # ' MOZ 2009 AIS has no complete birth history, only summary.
  clusters <- clusters %>%
    filter(survey_id != "MOZ2009AIS") %>%
    mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3]))
  
  surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
    left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id")) %>%
    filter(survey_id != "MOZ2009AIS")
  
  dhs_fr <- calculate_dhs_fertility(iso3, surveys, clusters, areas_wide)
  
}

if(mics_flag == 1) {
  
  mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
  mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))
  
  lvl_map <- read.csv("resources/iso_mapping_fit.csv")
  lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
  admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]
  
  mics_fr <- calculate_mics_fertility(iso3, mics_wm, mics_births_to_women)
  
}

if(dhs_flag == 1 & mics_flag == 1){
  asfr <- dhs_fr$asfr %>%
    bind_rows(mics_fr$mics_asfr)
  
  plot_dat <- dhs_fr$plot %>%
    bind_rows(mics_fr$mics_plot)
  
} else if(dhs_flag == 1 & mics_flag == 0) {
  asfr <- dhs_fr$asfr
  plot_dat <- dhs_fr$plot
} else {
  asfr <- mics_fr$mics_asfr
  plot_dat <- mics_fr$mics_plot
}

write_csv(asfr, "asfr.csv")

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

write_csv(plot_dat, "fr_plot.csv")