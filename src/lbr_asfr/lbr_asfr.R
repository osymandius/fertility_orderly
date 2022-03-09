iso3 <- "LBR"

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
clusters <- read.csv(paste0("depends/", tolower(iso3), "_dhs_clusters.csv"))
source("resources/utility_funs.R")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

clusters <- clusters %>%
  mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3]))

surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id"))

dhs_fr <- calculate_dhs_fertility(iso3, surveys, clusters, areas_wide)

if(exists("mics_fr")) {
  
  asfr <- dhs_fr$asfr %>%
    bind_rows(mics_fr$mics_asfr)
  
  plot_dat <- dhs_fr$plot %>%
    bind_rows(mics_fr$plot)
  
} else {
  asfr <- dhs_fr$asfr
  plot_dat <- dhs_fr$plot
}

write_csv(asfr, paste0(tolower(iso3), "_asfr.csv"))

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