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

plot_dat <- asfr_admin1 %>%
  bind_rows(mics_asfr_admin1) %>%
  select(-c(births, pys)) %>%
  rename(value = asfr) %>%
  bind_rows(
    bind_rows(tfr_admin1, mics_tfr_admin1) %>%
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