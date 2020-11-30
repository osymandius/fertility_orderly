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

plot <- asfr_admin1 %>%
  bind_rows(mics_asfr_admin1) %>%
  select(-c(births, pys)) %>%
  rename(value = asfr) %>%
  bind_rows(
    bind_rows(tfr_admin1, mics_tfr_admin1) %>%
      select(-se_tfr) %>%
      rename(value = tfr)
  )

write_csv(plot, paste0(tolower(iso3), "_fr_plot.csv"))