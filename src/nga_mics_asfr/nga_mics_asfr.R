iso3 <- "NGA"

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))

source("resources/utility_funs.R")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry
### MICS DATA

mics_births_to_women <- read.csv(paste0("depends/", tolower(iso3), "_mics_births_to_women.csv"))
mics_wm <- read.csv(paste0("depends/", tolower(iso3), "_mics_women.csv"))

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

mics_fr <- calculate_mics_fertility(mics_wm, mics_births_to_women)

write_csv(mics_fr$mics_plot, paste0(tolower(iso3), "_mics_plot.csv"))
write_csv(mics_fr$mics_asfr, paste0(tolower(iso3), "_mics_asfr.csv"))