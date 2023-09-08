packageVersion('dfertility')

iso3_c <- iso3
areas <- read_sf("depends/naomi_areas.geojson")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

mapping <- read_csv("resources/iso_mapping_fit.csv", show_col_types = F)
dhs_flag <- filter(mapping, iso3 == iso3_c)$dhs
mics_flag <- filter(mapping, iso3 == iso3_c)$mics

if(dhs_flag == 1) {
  clusters <- read.csv(paste0("depends/", tolower(iso3), "_dhs_clusters.csv"))
  
  # ' MOZ 2009 AIS has no complete birth history, only summary.
  clusters <- clusters %>%
    filter(!survey_id %in% c("MOZ2009AIS", "COG2009AIS", "TZA2003AIS", "UGA2011DHS", "UGA1995DHS", "UGA2011AIS", "ETH2000DHS")) %>%
    mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3])) %>%
    dplyr::rename(area_id = geoloc_area_id) %>%
    left_join(areas_wide)
  
  surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
    left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id")) %>%
    filter(!survey_id %in% c("MOZ2009AIS", "COG2009AIS", "TZA2003AIS", "UGA2011DHS", "UGA1995DHS", "UGA2011AIS", "ETH2000DHS"))
  
  individual_recode_data <- get_fertility_surveys(surveys, clusters)
  
  # debugonce(calculate_dhs_fertility)
  dhs_fr <- calculate_dhs_fertility(iso3, individual_recode_data, mapping)
  
  # saveRDS(dhs_fr$nrow_ir, "nrow_ir.rds")
}

if(mics_flag == 1) {
  
  mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
  mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))
  mics_fr <- calculate_mics_fertility(iso3, mics_wm, mics_births_to_women, mapping)
  
}

asfr <- list() 
plot <- list()

if(dhs_flag == 1 & mics_flag == 1){
  
 asfr$national <- bind_rows(dhs_fr$national$asfr, mics_fr$national$asfr)
 asfr$provincial <- bind_rows(dhs_fr$provincial$asfr, mics_fr$provincial$asfr)
 asfr$district <- bind_rows(dhs_fr$district$asfr, mics_fr$provincial$asfr)

 plot$national <- bind_rows(dhs_fr$national$plot, mics_fr$national$plot)
 plot$provincial <- bind_rows(dhs_fr$provincial$plot, mics_fr$provincial$plot)
  
} else if(dhs_flag == 1 & mics_flag == 0) {
  
  asfr$national <- dhs_fr$national$asfr
  asfr$provincial <- dhs_fr$provincial$asfr
  asfr$district <- dhs_fr$district$asfr
  
  plot$national <- dhs_fr$national$plot
  plot$provincial <- dhs_fr$provincial$plot
  
} else {
  asfr$national <- mics_fr$national$asfr
  asfr$provincial <- mics_fr$provincial$asfr
  asfr$district <- mics_fr$district$asfr
  
  plot$national <- mics_fr$national$plot
  plot$provincial <- mics_fr$provincial$plot
  
  # saveRDS(data.frame(), "nrow_ir.rds")
}


valid_years <- asfr$national %>%
  group_by(survey_id, period) %>%
  summarise(births = sum(births),
            pys = sum(pys)
  ) %>%
  filter(births > 50) %>%
  mutate(keep = T) %>%
  select(survey_id, period, keep)

asfr <- lapply(asfr, function(x) left_join(x, valid_years) %>% filter(!is.na(keep)))
plot <- lapply(plot, function(x) left_join(x, valid_years) %>% filter(!is.na(keep)))

saveRDS(asfr, "asfr.rds")

check_plot <- plot$provincial %>%
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
check_plot
dev.off()

saveRDS(plot, "fr_plot.rds")