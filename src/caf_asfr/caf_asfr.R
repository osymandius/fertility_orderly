iso3 <- "CAF"

source("resources/utility_funs.R")

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

areas_wide <- spread_areas(areas)

mics_fr <- calculate_mics_fertility(iso3, mics_wm, mics_births_to_women)

if(exists("mics_fr")){
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