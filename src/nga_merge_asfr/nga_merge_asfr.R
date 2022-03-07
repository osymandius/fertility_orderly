iso3 <- "NGA"


dhs_asfr <- read.csv(paste0("depends/", tolower(iso3), "_dhs_asfr.csv"))
mics_asfr <- read.csv(paste0("depends/", tolower(iso3), "_mics_asfr.csv"))

asfr_admin1 <- read.csv(paste0("depends/", tolower(iso3), "_asfr_admin1.csv"))
mics_asfr_admin1 <- read.csv(paste0("depends/", tolower(iso3), "_mics_asfr_admin1.csv"))

tfr_admin1 <- read.csv(paste0("depends/", tolower(iso3), "_tfr_admin1.csv"))
mics_tfr_admin1 <- read.csv(paste0("depends/", tolower(iso3), "_mics_tfr_admin1.csv"))


asfr <- dhs_asfr %>%
  bind_rows(mics_asfr)

write_csv(asfr, paste0(tolower(iso3), "_asfr.csv"))
write_csv(dhs_asfr, paste0(tolower(iso3), "_dhs_asfr.csv"))
write_csv(mics_asfr, paste0(tolower(iso3), "_mics_asfr.csv"))

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