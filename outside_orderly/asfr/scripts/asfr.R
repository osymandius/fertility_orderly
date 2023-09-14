asfr <- function(iso3_c) {
  
  # library(tidyverse)
  # library(rdhs)
  # library(naomi)
  # library(demogsurv)
  # library(sf)
  # library(dfertility)
  
  message(iso3_c)
  
  # iso3_c <- iso3
  areas <- readRDS("global/areas.rds")[[iso3_c]]
  
  areas_wide <- spread_areas(areas)
  areas_long <- areas %>% st_drop_geometry
  
  mapping <- read_csv("global/iso_mapping_fit.csv", show_col_types = F)
  dhs_flag <- filter(mapping, iso3 == iso3_c)$dhs
  mics_flag <- filter(mapping, iso3 == iso3_c)$mics
  
  if(dhs_flag == 1) {
    
    max_level <- max(areas$area_level)
    
    complete_areas_wide <- lapply(1:max_level, function(x) {
      areas_wide <- areas %>%
        filter(area_level <= x) %>%
        spread_areas()
    }) %>%
      bind_rows()
    
    clusters <- read.csv(paste0("outside_orderly/survey/outputs/", tolower(iso3_c), "_dhs_clusters.csv"))
    
    # ' MOZ 2009 AIS has no complete birth history, only summary.
    clusters <- clusters %>%
      filter(!survey_id %in% c("MOZ2009AIS", "COG2009AIS", "TZA2003AIS", "UGA2011DHS", "UGA1995DHS", "UGA2011AIS", "ETH2000DHS")) %>%
      mutate(DHS_survey_id = str_replace(survey_id, iso3_c, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3_c])) %>%
      dplyr::rename(area_id = geoloc_area_id) %>%
      left_join(complete_areas_wide)
    
    surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
      left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id")) %>%
      filter(!survey_id %in% c("MOZ2009AIS", "COG2009AIS", "TZA2003AIS", "UGA2011DHS", "UGA1995DHS", "UGA2011AIS", "ETH2000DHS"))
    
    individual_recode_data <- get_fertility_surveys(surveys, clusters)
    
    # debugonce(calculate_dhs_fertility)
    dhs_fr <- calculate_dhs_fertility(iso3_c, individual_recode_data, mapping)
    
    # saveRDS(dhs_fr$nrow_ir, "nrow_ir.rds")
  }
  
  if(mics_flag == 1) {
    
    mics_births_to_women <- read.csv(paste0("outside_orderly/survey/outputs/", tolower(iso3_c), "_mics_births_to_women.csv"))
    mics_wm <- read.csv(paste0("outside_orderly/survey/outputs/", tolower(iso3_c), "_mics_women.csv"))
    mics_fr <- calculate_mics_fertility(iso3_c, mics_wm, mics_births_to_women, mapping, areas)
    
  }
  
  asfr <- list() 
  plot <- list()
  
  if(dhs_flag == 1 & mics_flag == 1){
    
    asfr$national <- bind_rows(dhs_fr$national$asfr, mics_fr$national$asfr)
    asfr$provincial <- bind_rows(dhs_fr$provincial$asfr, mics_fr$provincial$asfr)
    asfr$district <- bind_rows(dhs_fr$district$asfr, mics_fr$district$asfr)
    
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
  
  saveRDS(asfr, paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c, "_asfr.rds"))
  
  check_plot <- plot$provincial %>%
    filter(variable == "tfr", value<15) %>%
    ggplot(aes(x=period, y=value, color=survey_id)) +
    geom_point() +
    facet_wrap(~area_id, ncol=5) +
    labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3_c, "| Provincial TFR")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )
  
  path <- paste0("outside_orderly/asfr/outputs/", iso3_c, "/check/tfr_admin1.pdf")
  pdf(path, h = 12, w = 20)
  print(check_plot)
  dev.off()
  
  saveRDS(plot, paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c, "_fr_plot.rds"))
}
