iso3 <- "NGA"

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

dhs_fr <- calculate_dhs_fertility(iso3, surveys, clusters, areas_wide)

write_csv(dhs_fr$plot_dat, paste0(tolower(iso3), "_plot.csv"))
write_csv(dhs_fr$asfr, paste0(tolower(iso3), "_asfr.csv"))