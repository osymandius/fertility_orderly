area_label_df <- areas_list %>%
  lapply(select, -any_of("spectrum_region_code")) %>%
  bind_rows() %>% 
  st_drop_geometry() %>% 
  separate(area_id, into=c("iso3", NA), sep=3) %>% 
  count(iso3, area_level, area_level_label)

area_label_df %>%
  left_join(mapping %>%
  select(iso3, fertility_fit_level, admin1_level) %>%
  pivot_longer(-iso3, values_to = "area_level")
  ) %>%
  filter(!is.na(name)) %>%
  mutate(area_level_label = paste0(area_level_label, " (", n, ")")) %>%
  select(-c(area_level, n)) %>%
  pivot_wider(names_from=name, values_from = area_level_label) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  select(iso3, country, everything()) %>%
  write_csv("~/Downloads/admin_level_label.csv")

####

mwi_15_dhs <- fr_plot[["MWI"]] %>%
  filter(area_level ==0, survey_id == "MWI2015DHS", variable == "tfr") %>%
  arrange(desc(period)) %>%
  mutate(tips = as.integer(row_number()-1),
         tips = ifelse(tips %in% c(5,6), tips, NA_integer_))

tips_overlap1 <- mwi_15_dhs %>%
  filter(period > 2004) %>%
  ggplot(aes(x=period, y=value)) +
    geom_line(size =1) +
    geom_point(size=2) +
    ggrepel::geom_label_repel(aes(label = tips), 
                              min.segment.length = 0,
                              segment.linetype = 2,
                              point.size=1, nudge_y = 0.5, 
                              nudge_x = 0.5, show.legend = FALSE) +
    standard_theme() +
    scale_x_continuous(breaks = seq(2005, 2015, 2), labels = as.character(seq(2005, 2015, 2))) +
    expand_limits(y=c(2, 8)) +
    labs(y="TFR", x=element_blank(), title = "Malawi | 2015 DHS")

tips_overlap2 <- fr_plot[["MWI"]] %>%
  filter(area_level ==0, survey_id != "MWI2015DHS", variable == "tfr", value > 4) %>%
  ggplot(aes(x=period, y=value, group=survey_id)) +
  geom_line(size =1, alpha =0.3) +
  geom_point(size=2, alpha = 0.3) +
  geom_line(data = mwi_15_dhs,
            size =1, color="red") +
  geom_point(data = mwi_15_dhs,
            size =2, color="red") +
  ggrepel::geom_label_repel(data = mwi_15_dhs, aes(label = tips), 
                            min.segment.length = 0,
                            segment.linetype = 2,
                            point.size=1, nudge_y = 1, 
                            nudge_x = 1, show.legend = FALSE, color="red") +
  standard_theme() +
  # scale_x_continuous(breaks = seq(2005, 2015, 2), labels = as.character(seq(2005, 2015, 2))) +
  expand_limits(y=c(2, 8)) +
  labs(y="TFR", x=element_blank(), title = "Malawi | All surveys")

ggpubr::ggarrange(tips_overlap1, tips_overlap2)

###

fr_plot %>%
  lapply(ungroup) %>%
  bind_rows() %>%
  separate(area_id, into=c(NA, "area_level", NA), sep=c(4,5), remove=FALSE, convert=TRUE) %>%
  separate(area_id, into=c("iso3", NA), sep=3, remove= FALSE) %>%
  separate(survey_id, into=c(NA, "survtype"), sep=7, remove=FALSE) %>%
  filter(survey_id %in% c("NER2006DHS", "NER2012DHS",
                          "BFA2010DHS",
                          "COD2013DHS",
                          # "ETH2019DHS",
                          "GHA2014DHS",
                          "MOZ2008MICS",
                          "NGA2018DHS",
                          "TGO2013DHS",
                          "SWZ2006DHS",
                          "NAM2006DHS"),
         area_id == iso3,
         variable == "tfr") %>%
  group_by(survey_id) %>%
  arrange(desc(period), survey_id, .by_group = TRUE) %>% 
  mutate(tips = as.integer(row_number()-1),
         tips = case_when(
           survey_id == "BFA2010DHS" & tips %in% c(5,6) ~ tips,
           survey_id == "COD2013DHS" & tips %in% c(10) ~ tips,
           survey_id == "GHA2014DHS" & tips %in% c(5,6, 10) ~ tips,
           survey_id == "NAM2006DHS" & tips %in% c(5,6) ~ tips,
           survey_id == "NER2012DHS" & tips %in% c(0, 5, 6) ~ tips,
           survey_id == "NGA2018DHS" & tips %in% c(10) ~ tips,
           survey_id == "SWZ2006DHS" & tips %in% c(5,6) ~ tips,
           survey_id == "MOZ2008MICS" & tips %in% c(10) ~ tips,
           survey_id == "NER2006DHS" & tips %in% c(0,5,6) ~ tips,
           survey_id == "TGO2013DHS" & tips %in% c(5,6) ~ tips,
           TRUE ~ NA_integer_
         )) %>%
  ggplot(aes(x=period, y=value, color=survey_id)) + 
  geom_point(show.legend = FALSE) + 
  geom_line(size=1, show.legend = FALSE) +
  moz.utils::standard_theme() +
  ggrepel::geom_label_repel(aes(label = tips), min.segment.length = 0,segment.linetype = 2,  point.size=1, nudge_y = 3, nudge_x = 1.5, show.legend = FALSE) +
  facet_wrap(~survey_id, nrow=2) +
  labs(y="TFR", x=element_blank())

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

pdf(paste0("~/Downloads/fe.pdf"), h = 12, w = 20)
fe
dev.off()
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


survey_summary <- data.frame(
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

surv <- read.csv("~/Downloads/Book3.csv")

surv %>%
  group_by(Country) %>%
  filter(Survey.Year == max(Survey.Year)) %>%
  ungroup() %>%
  summarise(quantile(Survey.Year, c(0.25, 0.5, 0.75)))

surv %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  summarise(quantile(n, c(0.25, 0.5, 0.75)))

surv %>%
  group_by(Country) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
