area_label_df <- areas_list %>%
  lapply(select, -any_of("spectrum_region_code")) %>%
  bind_rows() %>% 
  st_drop_geometry() %>% 
  separate(area_id, into=c("iso3", NA), sep=3) %>% 
  distinct(iso3, area_level, area_level_label)

area_label_df %>%
  left_join(mapping %>%
  select(iso3, fertility_fit_level, admin1_level) %>%
  pivot_longer(-iso3, values_to = "area_level")
  ) %>%
  filter(!is.na(name)) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  write.csv("~/Downloads/admin_level_label.csv")

###

fe <- fr_plot %>%
  lapply(ungroup) %>%
  bind_rows() %>%
  filter(iso3 %in% c("NER", "GIN"), variable == "tfr", area_level ==0) %>% 
  separate(survey_id, into=c(NA, "survtype"), sep=7, remove=FALSE) %>%
  group_by(iso3) %>%
  group_split() %>%
  lapply(function(x) {
    iso3_c <- unique(x$iso3)
    cntry_name <- countrycode::countrycode(iso3_c, "iso3c", "country.name")
    
    x %>%
      arrange(desc(period), survey_id) %>% 
      group_by(survey_id) %>% 
      mutate(tips = row_number()-1) %>%
      ggplot(aes(x=period, y=value, color=survey_id)) + 
      geom_point() + 
      geom_line(size=1) +
      moz.utils::standard_theme() +
      ggrepel::geom_label_repel(aes(label = tips), show.legend = FALSE) +
      facet_wrap(~survey_id) +
      labs(y="TFR", x=element_blank(), title = cntry_name)
  })
  
###

asfr_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_asfr", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

grep("20220325", unlist(asfr_id[!is.na(asfr_id)]), value=TRUE)

nrow_ir <- lapply(paste0("archive/aaa_asfr/", grep("20220325", unlist(asfr_id[!is.na(asfr_id)]), value=TRUE), "/nrow_ir.rds"), readRDS)

nrow_ir <- unlist(nrow_ir, recursive=FALSE)

asfr <- lapply(paste0("archive/aaa_asfr/", asfr_id[!is.na(asfr_id)], "/asfr.csv"), read.csv) 

asfr_sum <- asfr %>%
  bind_rows() %>%
  bind_rows(read.csv("global/phia_asfr.csv")) %>%
  group_by(survey_id) %>%
  summarise(births = round(sum(births)),
            pys = round(sum(pys)))

asfr %>%
  bind_rows() %>%
  bind_rows(read.csv("global/phia_asfr.csv")) %>%
  separate(survey_id, into=c("iso3", NA), sep=3, remove=FALSE) %>%
  distinct(iso3, survey_id) %>%
  count(iso3) %>%
  filter(n==4)

phia_nrow <- data.frame(survey_id = c(
  "CIV2017PHIA",
  "CMR2017PHIA", 
  "LSO2017PHIA",
  "MWI2017PHIA", 
  "NAM2017PHIA",
  "RWA2019PHIA", 
  "SWZ2017PHIA",
  "TZA2017PHIA", 
  "UGA2017PHIA", 
  "ZMB2016PHIA", 
  "ZWE2016PHIA"
),
n = c(8430,
      12676,
      6509,
      10115,
      8830,
      14690,
      5094,
      15074,
      14521,
      10961,
      11005))

mics_nrow <- lapply(list.files(paste0("archive/aaa_asfr/", asfr_id[1], "/depends"), pattern = "mics_women", full.names = TRUE), read.csv) %>%
  bind_rows() %>%
  count(survey_id)

old_areal_surveys <- data.frame(survey_id = c("MOZ1997DHS",
"MOZ2003DHS",
"KEN1998DHS",
"RWA2000DHS",
"RWA2013MIS",
"RWA2017MIS",
"ZAF1998DHS",
"SEN2006MIS",
"SEN2017DHS",
"CIV2005AIS",
"NER2006DHS",
"NER2012DHS",
"CMR1998DHS",
"TZA1996DHS",
"TZA2004DHS",
"TCD2004DHS"),
data_type = "Areal")

long_mics <- asfr %>%
  bind_rows() %>%
  group_by(survey_id) %>%
  filter(tips == max(tips)) %>%
  distinct(survey_id, tips) %>%
  filter(str_detect(survey_id, "MICS"), tips > 5) %>%
  mutate(tips = 15)


data.frame(
  survey_id = names(nrow_ir),
  n = as.integer(nrow_ir)
) %>%
  bind_rows(mics_nrow) %>%
  bind_rows(phia_nrow) %>%
  left_join(asfr_sum) %>%
  separate(survey_id, into=c("iso3", "survyear", "survtype"), sep=c(3, 7), remove=FALSE, convert=TRUE) %>%
  arrange(iso3, survyear) %>%
  left_join(old_areal_surveys) %>%
  left_join(long_mics) %>%
  mutate(data_type = case_when(
    is.na(data_type) & survtype != "MICS" ~ "Point",
    is.na(data_type) & survtype == "MICS" ~ "Areal",
    TRUE ~ data_type),
    tips = case_when(
      survtype == "DHS" ~ 15,
      is.na(tips) & survtype == "MICS" ~ 5,
      survtype == "PHIA" ~ 1,
      survtype %in% c("AIS", "MIS") ~ 5,
      TRUE ~ tips
    ),
    country = countrycode::countrycode(iso3, "iso3c", "country.name")
    ) %>%
  select(Country = country, `Survey Year` = survyear, `Survey Type` = survtype, `Data Type` = data_type, `TIPS` = tips, `Number of women`  = n,
         `Number of births` = births, `Person-years` = pys) %>%
  
  write.csv("~/Downloads/survey_summary.csv")
