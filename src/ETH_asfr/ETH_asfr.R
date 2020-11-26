areas <- read_sf("depends/eth_areas.geojson")
clusters <- read.csv("depends/eth_dhs_clusters.csv")

areas_wide <- spread_areas(areas)
areas_long <- areas %>% st_drop_geometry

iso3 <- "ETH"

clusters <- clusters %>%
  filter(!is.na(longitude)) %>%
  mutate(DHS_survey_id = str_replace(survey_id, iso3, dhs_countries()$DHS_CountryCode[dhs_countries()$ISO3_CountryCode == iso3])) %>%
  filter(survey_id != "ETH2000DHS")

surveys <- dhs_surveys(surveyIds = unique(clusters$DHS_survey_id)) %>%
  left_join(clusters %>% select(survey_id, DHS_survey_id) %>% distinct, by=c("SurveyId" = "DHS_survey_id")) %>%
  filter(survey_id != "ETH2000DHS")

# cluster_areas <- assign_cluster_area(clusters, areas_wide, 3)
# 
# dat <- clusters_to_surveys(iso3, surveys, cluster_areas, level = 3, single_tips = TRUE)

cluster_list <- clusters %>%
  rename(area_id = geoloc_area_id) %>%
  group_by(survey_id) %>%
  group_split

names(cluster_list) <- surveys$survey_id

ird <- dhs_datasets(fileType = "IR", fileFormat = "flat", surveyIds = surveys$SurveyId)

ird <- ird %>%
  mutate(path = unlist(get_datasets(.))) %>%
  bind_rows()

ir <- lapply(ird$path, readRDS) %>%
  lapply(function(x) {class(x) <- "data.frame"
  return(x)}) %>%
  Map(function(ir, surveys) {
    mutate(ir,
           surveyid = surveys$survey_id,
           country = surveys$CountryName,
           survyear = surveys$SurveyYear,
           survtype = surveys$SurveyType)
  }, ., group_split(surveys, SurveyId))

names(ir) <- names(cluster_list)

ir <- Map(ir_by_area, ir, cluster_list[names(ir)], n=1:length(ir), total=length(ir)) %>%
  unlist(recursive = FALSE)

survey_type <- ir %>%
  lapply("[", "survtype") %>%
  lapply(unique) %>%
  bind_rows

tips_surv <- list("DHS" = c(0:15), "MIS" = c(0:5), "AIS" = c(0:5))[survey_type$survtype]

dat <- list()
dat$ir <- ir
dat$tips_surv <- tips_surv

asfr <- Map(calc_asfr, dat$ir,
            by = list(~survey_id + survtype + survyear + area_id),
            tips = dat$tips_surv,
            agegr= list(3:10*5),
            period = list(1995:2017),
            strata = list(NULL),
            varmethod = list("none"),
            counts = TRUE) %>%
  bind_rows %>%
  type.convert %>%
  filter(period<=survyear) %>%
  rename(age_group = agegr) %>%
  mutate(iso3 = iso3)

write_csv(asfr, "eth_dhs_asfr.csv")