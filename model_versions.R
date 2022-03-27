#' Model 1: TIPS RW [DHS], FE at 5, 6, 10
#' 

m1_id <- c("20220316-105515-e3e78488",
"20220316-105423-57a08daf",
"20220316-104420-bb792f94",
"20220316-104251-3b79cccb",
"20220316-104049-d1c92afb",
"20220316-103435-f6e7e9f9",
"20220316-103408-aebb90cb",
"20220316-103212-04da390e",
"20220316-102106-62328f9f",
"20220316-101916-85f3d036",
"20220316-101811-23dc93b1",
"20220316-101521-7842b4be",
"20220316-101243-d216d329",
"20220316-100930-9887fb1b",
"20220316-100856-48aebdda",
"20220316-100648-6518c040",
"20220316-100330-2a102702",
"20220316-095949-639a4c7d",
"20220316-095553-4db2c299",
"20220316-095036-5d2d35a3",
"20220316-094901-3af39493",
"20220316-094038-3b066748",
"20220316-093651-d1ff488e",
"20220316-093415-8cb4ef86",
"20220316-093203-ab2edec9",
"20220316-092723-db065e87")

#' Model 2: TIPS RW [DHS], FE at 5, 6, 10. Informed TIPS RW prior (log_prec ~ 7)
#' 
m2_id <- c("20220316-132714-7399741d",
"20220316-132541-58b09718",
"20220316-132448-fe447bca",
"20220316-131417-35611f5b",
"20220316-131243-04a1c3ec",
"20220316-131038-24ac0fef",
"20220316-130403-13d12655",
"20220316-130207-1a803c00",
"20220316-125049-37a70f81",
"20220316-124859-02bdc9da",
"20220316-124750-76ac6e78",
"20220316-124446-62716191",
"20220316-124151-e7ab85a5",
"20220316-123832-bfdf9ed9",
"20220316-123617-8fe5634e",
"20220316-123256-cb8438f3",
"20220316-122905-6dd475ce",
"20220316-122505-64dfdf13",
"20220316-121941-74ad9851",
"20220316-121807-b025c94e",
"20220316-120924-eebb5c01",
"20220316-120520-fa10d6a4",
"20220316-120206-b9e05a3c",
"20220316-120001-c68ccc50",
"20220316-115510-72390e22")
#' 
#' Model 3: TIPS AR1 [DHS], FE at 0, 5, 6, 10. 
m3_id <- c("20220316-172009-541c06a1",
"20220316-171915-f2c226ed",
"20220316-170906-a5aabc45",
"20220316-170734-4b4b54ea",
"20220316-170528-29878fbe",
"20220316-165845-49cc1479",
"20220316-165643-4c1907d3",
"20220316-164522-4351b73e",
"20220316-164334-f6571fab",
"20220316-164226-c3be8ce8",
"20220316-163938-ebc91bb4",
"20220316-163707-d1700ee6",
"20220316-163355-35d49e44",
"20220316-163142-eadaf0f4",
"20220316-162819-6c0af68c",
"20220316-162437-1fda6dda",
"20220316-162039-130182f1",
"20220316-161517-c91ef1c8",
"20220316-161348-013f7392",
"20220316-160516-e413b847",
"20220316-160128-136c1d1a",
"20220316-155850-96845111",
"20220316-155638-46ddab9d",
"20220316-155138-9761fc9b")

#' Model 4: As Model 3. AR1 on age [remove a constraint], FE 0,5,6,10 by survey. 
#' FE 0 and 10 extended to all surveys
#' 
m4_id <- c("20220316-200122-2e127eee", "20220316-195935-efbb8478", 
"20220316-195837-e68045ba", "20220316-194720-2f0c0b6f", 
"20220316-194542-cd98b1b2", "20220316-194326-f385b897", 
"20220316-193619-0b3632f7", "20220316-193416-0cff8875", 
"20220316-192140-4495a3b1", "20220316-191945-d47ba184", 
"20220316-191835-6f04c5fe", "20220316-191537-fecd7fbd", 
"20220316-191219-af59dcf2", "20220316-190850-0f390a60", 
"20220316-190625-02228e81", "20220316-190248-ed10954b", 
"20220316-185835-8edcae8d", "20220316-185412-0b780162", 
"20220316-184838-df087359", "20220316-184659-31473e7f", 
"20220316-183741-bed21add", "20220316-183341-9ce6f96d", 
"20220316-183046-66c86f16", "20220316-182826-3832012e", 
"20220316-182314-493d9dfd")
#' Model 5: As Model 4. No TIPS RW
#' 

m5_id <- c("20220316-214423-9bd22085", 
"20220316-214247-79f2f5d3", 
"20220316-214155-42f8f55b", 
"20220316-213147-b03ced11", 
"20220316-213018-d5f8eba8", 
"20220316-212819-0838ab1f", 
"20220316-212143-e0a1f384", 
"20220316-211945-160d1fc8", 
"20220316-210835-74337b32", 
"20220316-210700-0a55b2c6", 
"20220316-210553-fe3de526", 
"20220316-210313-20e58bc2", 
"20220316-210036-59bc172c", 
"20220316-205721-a5f6884e", 
"20220316-205508-9512655e", 
"20220316-205153-a7f40d71", 
"20220316-204810-39077708", 
"20220316-204412-91685fc7", 
"20220316-203916-6d5293ac", 
"20220316-203747-479bfe01", 
"20220316-202920-f1b6b16f", 
"20220316-202535-6cb97c82", 
"20220316-202259-cbb04a4e", 
"20220316-202049-d073359d", 
"20220316-201603-8f8fb420")

#' 
#' Model 6: As Model 4. TIPS RW on all surveys [one set of coefficients]
#' 
m6_id <- c("20220316-233421-47c0ad1e",
"20220316-233237-0d4d9daf",
"20220316-233138-969620bf",
"20220316-232031-34c31d50",
"20220316-231851-63d9d7e4",
"20220316-231634-3710cc7f",
"20220316-230941-23563ae9",
"20220316-230737-67e83095",
"20220316-225451-8b138a7f",
"20220316-225259-9dc6659f",
"20220316-225147-bce6480a",
"20220316-224847-d28f3cce",
"20220316-224600-e1cd840a",
"20220316-224225-512e3fd9",
"20220316-223954-f2975bb4",
"20220316-223552-3701cfb5",
"20220316-223140-609bdfae",
"20220316-222709-c40d4ff5",
"20220316-222132-3e7b704c",
"20220316-221949-41658f55",
"20220316-221038-ca150fa7",
"20220316-220630-e78679a5",
"20220316-220327-f907f394",
"20220316-220102-be28b7a9",
"20220316-215538-9702a986")

m1_dat <- lapply(paste0("archive/aaa_fit/", m1_id, "/fr.csv"), read.csv) %>%
  bind_rows() %>%
  mutate(source = "Model 1")

m2_dat <- lapply(paste0("archive/aaa_fit/", m2_id, "/fr.csv"), read.csv) %>%
  bind_rows() %>%
  mutate(source = "Model 2")

m3_dat <- lapply(paste0("archive/aaa_fit/", m3_id, "/fr.csv"), read.csv) %>%
  bind_rows() %>%
  mutate(source = "Model 3")

m4_dat <- lapply(paste0("archive/aaa_fit/", m4_id, "/fr.csv"), read.csv) %>%
  bind_rows() %>%
  mutate(source = "Model 4")

m5_dat <- lapply(paste0("archive/aaa_fit/", m5_id, "/fr.csv"), read.csv) %>%
  bind_rows() %>%
  mutate(source = "Model 5")

m6_dat <- lapply(paste0("archive/aaa_fit/", m6_id, "/fr.csv"), read.csv) %>%
  bind_rows() %>%
  mutate(source = "Model 6")

dat <- bind_rows(m1_dat, m2_dat, m3_dat, m4_dat, m5_dat, m6_dat) %>%
  filter(!iso3 %in% c("GNB", "CAF")) %>%
  arrange(iso3) %>%
  group_by(iso3)

dat <- dat %>%
  group_split() %>%
  setNames(unique(dat$iso3))

fr_plot <- lapply(paste0("archive/aaa_fit/", m4_id, "/depends/fr_plot.csv"), read.csv)

name_key <- m1_dat %>%
  bind_rows() %>%
  distinct(area_id, area_name)

subnational_surveys <- c("KEN2009MICS", "KEN2011MICS")

fr_plot <- fr_plot %>%
  bind_rows() %>%
  bind_rows(read.csv("global/phia_tfr_admin0.csv") %>%
              rename(value = median) %>%
              select(-tips) %>%
              separate(survey_id, into=c("iso3", NA), sep=3, remove=FALSE) %>%
              mutate(area_id = iso3,
                     area_level =0)
  )%>%
  separate(survey_id, into=c("iso3", NA), sep=3, remove=FALSE) %>%
  arrange(iso3) %>%
  left_join(name_key) %>%
  filter(!(area_id == iso3 & survey_id %in% subnational_surveys)) %>%
  separate(area_id, into=c(NA, "area_level", NA), sep=c(4,5), remove=FALSE, convert=TRUE) %>%
  mutate(area_level = ifelse(is.na(area_level), 0, area_level))

fr_plot <- fr_plot %>%
  group_by(iso3) %>%
  group_split() %>%
  setNames(unique(fr_plot$iso3))

comparison_fr <- read.csv("global/comparison_fr.csv") %>%
  arrange(iso3) %>%
  group_by(iso3)

comparison_plot <- comparison_fr %>%
  group_split() %>%
  setNames(unique(comparison_fr$iso3))

get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  # xmin = gb$layout$panel_params[[1]]$x.range[1]
  # xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(ymin = ymin, ymax = ymax)
}

plots <- Map(function(dat1, fr_plot, comparison_plot) {
  iso3_c <- unique(dat1$iso3)
  cntry_name <- countrycode::countrycode(iso3_c, "iso3c", "country.name")
  
  remove_survey <- c("CIV2005AIS", "MLI2009MICS", "MLI2015MICS", "SLE2010MICS", "TGO2006MICS", "BEN1996DHS", "KEN2009MICS", "COD2017MICS", "TZA2007AIS", "TZA2012AIS")
  
  
  fr_plot <- fr_plot %>%
    mutate(area_id = ifelse(is.na(area_id), iso3_c, area_id)) %>%
    filter(value > 0.5, area_level ==0)
  
  tfr_plot <- dat1 %>%
    filter(area_level == 0, variable == "tfr", period > 1999) %>%
    bind_rows(data.frame(source = c("WPP2019", "GBD2019"))) %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1, aes(color=source)) +
    # geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, value > 1.5, period > 1999, !survey_id %in% remove_survey), aes(y=value)) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, value > 1.5, period > 1999, survey_id %in% remove_survey), aes(y=value), shape=22, fill=NA) +
    # facet_wrap(~area_id, ncol=5) +
    labs(y="TFR", x=element_blank(), title=paste(iso3_c, "| National TFR")) +
    moz.utils::standard_theme()
  
  limits <- get_plot_limits(tfr_plot)
  
  comparison_plot <- dat1 %>%
    # mutate(source = "dfertility") %>%
    filter(area_level == 0, variable == "tfr", period > 1999) %>%
    bind_rows(
      comparison_plot %>% 
        filter(area_level == 0, 
               variable == "tfr", 
               source == "GBD2019") %>%
        rename(median = val)
    ) %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1, aes(color=source)) +
    # geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=0.5, show.legend = FALSE) +
    geom_step(data = comparison_plot %>% filter(area_level == 0, variable == "tfr", source == "WPP2019"), aes(y=val, color=source), size=1) +
    labs(y="TFR", x=element_blank(), title=paste(iso3_c, "| National TFR")) +
    lims(y=c(limits$ymin, limits$ymax)) +
    moz.utils::standard_theme()
  
  ggpubr::ggarrange(tfr_plot, comparison_plot, nrow = 1)
  
}, dat, fr_plot[names(dat)], comparison_plot[names(dat)])


pdf(paste0("~/Downloads/m1_6_comparison.pdf"), h = 6, w = 12)
plots
dev.off()
