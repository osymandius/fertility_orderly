hyper_sd <- read_csv("hyper_sd_report.csv")

hyper_sd <- sd_report %>%
  mutate(source = "foo")

hyper_sd$X1 <- NULL
colnames(hyper_sd) <- c("est", "se", "hyper", "iso3", "source")

hyper_sd <- hyper_sd %>%
  mutate(lower = est - 1.96*se,
         upper = est + 1.96*se)
  
hyper_sd %>%
  filter(hyper %in% c("log_prec_rw_period", "lag_logit_phi_arima_period", "log_prec_rw_age", "log_prec_spatial", "lag_logit_phi_period", "beta_period"),
         !hyper %in% c("beta_tips_dummy", "u_tips", "eta1", "eta3", "eta2", "u_period", "u_age", "u_spatial_str"),
         # str_detect(hyper, "eta1|rw_age|eta3"),
         !str_detect(source, "Eta2"),
         !str_detect(hyper, "spike")) %>%
  mutate(hyper = factor(hyper, levels = c("log_prec_rw_period",
                                          "lag_logit_phi_arima_period",
                                          "lag_logit_phi_period",
                                          "beta_period",
                                          "log_prec_rw_age",
                                          "log_prec_spatial",
                                          "log_prec_rw_tips",
                                          "log_prec_eta1",
                                          "lag_logit_eta1_phi_period",
                                          "lag_logit_eta1_phi_age",
                                          "log_prec_eta2",
                                          "logit_eta2_phi_period",
                                          "log_prec_eta3",
                                          "lag_logit_eta3_phi_age"
  ))) %>%
  ggplot(aes(x=est, y=forcats::fct_rev(iso3), color=iso3)) +
    geom_pointrange(aes(xmin = lower, xmax = upper), size=0.4, show.legend = FALSE) +
    geom_vline(aes(xintercept = 0), linetype = 3) +
    scale_color_manual(values = c(
      "#ff5362",
       "#59d552",
       "#bb53da",
       "#4f8900",
       "#4f70fb",
       "#c0a600",
       "#0068cc",
       "#dbc746",
       "#e141bf",
       "#009338",
       "#ec0780",
       "#00b473",
       "#c61915",
       "#46deab",
       "#9f1948",
       "#01ac9d",
       "#a02400",
       "#46c3ff",
       "#ff882f",
       "#57d7ea",
       "#994800",
       "#dca6ff",
       "#7a6a00",
       "#ff7b99",
       "#cacb67",
       "#94505d",
       "#93a873",
       "#ffb2a1",
       "#664f20"
    )) +
    facet_wrap(~hyper, nrow=1) +
    moz.utils::standard_theme() +
    labs(x=element_blank(), y=element_blank()) +
    theme(panel.background = element_rect(fill=NA, color="black"))

hyper_sd %>%
  filter(hyper == "u_period") %>%
  group_by(iso3, source) %>%
  mutate(n = row_number()) %>%
  ggplot(aes(x=n, y=est, color=source)) +
    geom_point() +
    geom_line() +
    facet_wrap(~iso3)


sd_report %>%
  filter(hyper == "eta2") %>%
  mutate(idx = rep(1:26, times = 135),
         n = rep(1:135, each = 26)) %>%
  ggplot(aes(x=idx, y=Estimate)) +
    geom_point() +
    geom_line() +
    facet_wrap(~n)
  
####
id <- lapply(ssa_iso3[!ssa_iso3 %in% c("BWA", "ZAF", "GNQ", "MLI", "ETH")], function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

names(id_dhs) <- ssa_iso3
id_dhs <- id_dhs[!is.na(id_dhs)]

id_phia <- lapply(c("SWZ", "LSO", "RWA", "UGA", "TZA", "ZMB", "ZWE", "CMR", "CIV"), function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('parameter:iso3 == "', x, '"'), draft = FALSE)
})

id_phia <- id_phia %>%
  lapply(function(x) {
    len <- length(x)
    x[len-1]})

path <- paste0("archive/aaa_fit/", c(id_dhs, id_phia), "/hyper.rds")
names(path) <- c(names(id_dhs), "SWZ", "LSO", "RWA", "UGA", "TZA", "ZMB", "ZWE", "CMR", "CIV")
possibly_rds <- purrr::possibly(.f = readRDS, otherwise = NULL)
dat <- lapply(path, possibly_rds) %>%
  compact()

names(id) <- ssa_iso3
path <- paste0("archive/aaa_fit/", id[!is.na(id)], "/hyper.rds")
possibly_rds <- purrr::possibly(.f = readRDS, otherwise = NULL)
dat <- lapply(path, possibly_rds) %>%
  compact()

period_rw <- dat %>%
  lapply("[", "log_prec_rw_period") %>%
  lapply(as.data.frame.list) %>%
  bind_rows(.id="iso3") %>%
  pivot_longer(-iso3)

MASS::fitdistr(period_rw$value, "normal")

beta_tips <- dat %>%
  lapply("[", "beta_tips_dummy") %>%
  lapply(as.data.frame.list) %>%
  bind_rows(.id="iso3") %>%
  pivot_longer(-iso3)

MASS::fitdistr(beta_tips$value, "normal")
  
foo <- dat %>%
  lapply("[", "u_tips") %>%
  unlist(recursive = FALSE) %>%
  compact() %>%
  lapply(function(x) apply(x, MARGIN = 1, FUN = median))

names(foo) <- substr(names(foo), 0, 3)

foo %>%
  as.data.frame() %>%
  mutate(idx = 0:14) %>%
  pivot_longer(-idx) %>%
  ggplot(aes(x=idx, y=value)) +
    geom_line() +
    facet_wrap(~name)
  
rw_tips <- dat %>%
  lapply("[", "log_prec_rw_tips") %>%
  lapply(as.data.frame.list) %>%
  bind_rows(.id="iso3") %>%
  pivot_longer(-iso3)

MASS::fitdistr(rw_tips$value, "normal")

id <- lapply(ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "GMB", "BWA", "ZAF", "NGA", "MWI", "RWA")], function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

id_2 <- lapply(c("RWA", "MWI"), function(x){
  orderly::orderly_search(name = "aaa_mwi_rwa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

id_3 <- lapply("ETH", function(x){
  orderly::orderly_search(name = "aaa_eth_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

# id <- c(id, id_2, id_3)

hyper <- lapply(paste0("archive/aaa_fit/", id[!is.na(id)], "/hyper.rds"), possibly_rds)

names(hyper) <- ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "GMB", "BWA", "ZAF", "COD", "NGA")]
dat <- compact(dat)
names(dat)

####

sd_report <- lapply(paste0("archive/aaa_fit/", id[!is.na(id)], "/sd_report.csv"), read_csv, show_col_types = FALSE)

sd_report2 <- lapply(paste0("archive/aaa_mwi_rwa_fit/", id2, "/sd_report.csv"), read_csv, show_col_types = FALSE)

sd_report3 <- lapply(paste0("archive/aaa_eth_fit/", id3, "/sd_report.csv"), read_csv, show_col_types = FALSE)

sd_report <- c(sd_report, sd_report2)

names(sd_report) <- ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "GMB", "BWA", "ZAF", "COD", "NGA")]

View(sd_report[["NER"]])

####
                         


id <- id[!is.na(id)]
id <- id[!id %in% names(dat)]

mapping <- read_csv("global/iso_mapping_fit.csv")

dat <- lapply(paste0("archive/aaa_fit/", id, "/fr.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  # mutate(source = "No TIPS RW") %>%
  arrange(iso3) %>%
  group_by(iso3) 

default_dat <- read_csv("~/Downloads/default_fr.csv") %>%
  mutate(source = "Base") %>%
  filter(iso3 %in% unique(dat$iso3)) %>%
  arrange(iso3) %>%
  group_by(iso3)
  
default_dat <- default_dat %>%
  group_split() %>%
  setNames(unique(default_dat$iso3))

dat <- dat %>%
  group_split() %>%
  setNames(unique(dat$iso3))

fr_plot <- lapply(paste0("archive/aaa_fit/", id, "/depends/fertility_fr_plot.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  bind_rows(
    read_csv("global/phia_asfr_admin1.csv") %>%
      separate(area_id1, into=c("iso3", NA), sep = 3, remove = FALSE) %>%
      group_by(survey_id, period, area_id1) %>%
      summarise(value = 5*sum(asfr)) %>%
      ungroup %>%
      rename(area_id = area_id1) %>%
      mutate(variable = "tfr")
  ) %>%
  separate(area_id, into=c("iso3", NA), sep=3, remove= FALSE) %>%
  filter(iso3 %in% names(dat)) %>%
  arrange(iso3) %>%
  group_by(iso3) 

fr_plot <- lapply(paste0(paste0("archive/", paste0(tolower(ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "GMB", "BWA", "ZAF", "COD", "NGA")]), "_asfr")) %>%
  lapply(list.files, full.names = TRUE) %>%
  lapply(tail, 1) %>%
  unlist(),
  "/",
  paste0(tolower(ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "GMB", "BWA", "ZAF", "COD", "NGA")]),
  "_fr_plot.csv")
),
read_csv, show_col_types = FALSE)

names(fr_plot) <- ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "GMB", "BWA", "ZAF", "COD", "NGA")]

fr_plot <- fr_plot %>%
  group_split() %>%
  setNames(unique(fr_plot$iso3))

dat <- list("COD" = read.csv("archive/aaa_fit/20220312-084804-dcf691a4/fr.csv"))
fr_plot <- list("COD" = read.csv("archive/aaa_fit/20220312-084804-dcf691a4/depends/fertility_fr_plot.csv"))

plots <- Map(function(dat1, fr_plot) {
  iso3_c <- unique(dat1$iso3)
  # admin1_lvl <- filter(mapping, iso3 == iso3_c)$admin1_level
  
  fr_plot <- fr_plot %>%
    mutate(area_id = ifelse(is.na(area_id), iso3_c, area_id)) %>%
    filter(value > 0.5, area_id == "COD")
  
  tfr_plot <- dat1 %>%
    # bind_rows(dat2) %>%
    filter(area_level == 0, variable == "tfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, !survey_id %in% remove_survey), aes(y=value, color=survey_id)) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, survey_id %in% remove_survey), aes(y=value, color=survey_id), shape=22, fill=NA) +
    facet_wrap(~area_id, ncol=5) +
    labs(y="TFR", x=element_blank(), title=paste(iso3_c, "| National TFR")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )
  
}, dat, fr_plot[names(dat)])


id <- lapply(c("MOZ"), function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('parameter:iso3 == "', x, '"'), draft = FALSE)
})

id <- list.files("archive/aaa_fit")[210:232]

dat <- lapply(paste0("archive/aaa_fit/", id, "/fr.csv"), read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  mutate(source = "No RW") %>%
  arrange(iso3) %>%
  group_by(iso3) 

default_dat <- read_csv("~/Downloads/default_fr.csv") %>%
  mutate(source = "Base") %>%
  filter(iso3 %in% unique(dat$iso3)) %>%
  arrange(iso3) %>%
  group_by(iso3)

ssa_iso3[!ssa_iso3 %in% names(dat)]

#####

sd_report %>% bind_rows() %>% 
  filter(hyper %in% c(unique(sd_report[[1]]$hyper)[1:14], "beta_period", "beta_tips_dummy_10", "log_prec_smooth_iid")) %>%
  ggplot(aes(y=Estimate, x=iso)) +
    geom_pointrange(aes(ymin = (Estimate-1.96*Std..Error), ymax = (Estimate+1.96*Std..Error)), size=0.3) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + 
    facet_wrap(~hyper, scale="free")

###

sd_ner <- read_csv("draft/aaa_fit/20220310-124125-fc6d6e5b/sd_report.csv")

unique(sd_ner$hyper)[1:15]

sd_report <- list(read_csv("archive/aaa_fit/20220311-150952-d406e702/sd_report.csv"))

sd_sen %>%  
  filter(hyper  %in% unique(sd_sen$hyper)[1:14])

sen_h <- readRDS("~/Documents/GitHub/fertility_orderly/archive/aaa_fit/20220305-181437-4509eaa5/hyper.rds")
MASS::fitdistr(exp(sen_h$log_prec_rw_period[1,]), densfun = "normal")

sd_report <- read.csv("draft/aaa_fit/20220311-085733-926b3a2c/sd_report.csv")

data.frame(matrix(filter(sd_report, hyper == "eta2")$Estimate, nrow=47, byrow = FALSE)) %>%
  mutate(idx = row_number()) %>%
  left_join(areas %>% filter(area_level == 2) %>% mutate(idx = row_number()) %>% select(area_name, idx) %>% st_drop_geometry()) %>%
  select(-idx) %>%
  pivot_longer(-area_name) %>%
  mutate(name = str_remove(name, "X")) %>%
  type.convert() %>%
  ggplot(aes(x=factor(name), y=value, color=area_name, group=area_name))+
  geom_point() +
  geom_line()

data.frame(matrix(filter(sd_report, hyper == "eta1")$Estimate, nrow=7, byrow = FALSE))

matrix(filter(sd_report, hyper == "eta2")$Estimate, nrow=47, byrow = FALSE) %>%
  apply(MARGIN = 1, function(x) t(spline_mat %*% x)) %>%
  do.call(rbind, .) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(idx = row_number()) %>%
  left_join(areas %>% filter(area_level == 2) %>% mutate(idx = row_number()) %>% select(area_name, idx) %>% st_drop_geometry()) %>%
  select(-idx) %>%
  pivot_longer(-area_name) %>%
  mutate(name = str_remove(name, "V")) %>%
  type.convert() %>%
  ggplot(aes(x=name, y=value, color=area_name, group=area_name))+
    geom_point() +
    geom_line()
 

data.frame(matrix(filter(sd_report, hyper == "eta2")$Estimate, nrow=11, byrow = FALSE)) %>%
  mutate(area_sort_order = row_number() + 1) %>%
  left_join(areas %>% select(area_name, area_sort_order) %>% st_drop_geometry())  %>%
  select(-area_sort_order) %>%
  pivot_longer(-area_name) %>%
  mutate(name = str_remove(name, "X")) %>%
  group_by(name) %>%
  summarise(value = sum(value))

############


id <- lapply(ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "BWA", "MWI", "RWA", "GNB", "CAF","SSD", "COD")], function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
}) 

id2 <- lapply(c("RWA", "MWI"), function(x){
  orderly::orderly_search(name = "aaa_mwi_rwa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

id3 <- lapply("ETH", function(x){
  orderly::orderly_search(name = "aaa_eth_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

dat <- lapply(paste0("archive/aaa_fit/", id, "/fr.csv"), read_csv, show_col_types = FALSE)
dat2 <- lapply(paste0("archive/aaa_mwi_rwa_fit/", id2, "/fr.csv"), read_csv, show_col_types = FALSE)
dat3 <- lapply(paste0("archive/aaa_eth_fit/", id3, "/fr.csv"), read_csv, show_col_types = FALSE)
dat <- c(dat, dat2, dat3) %>%
  setNames(c(ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "BWA", "MWI", "RWA", "GNB", "CAF","SSD", "COD")], "RWA", "MWI", "ETH"))

fr_plot <- lapply(file.path("archive/aaa_fit", id, "depends/fr_plot.csv"), read.csv)
fr_plot2 <- lapply(file.path("archive/aaa_mwi_rwa_fit", id2, "depends/fr_plot.csv"), read.csv)
fr_plot3 <- lapply(file.path("archive/aaa_eth_fit", id3, "depends/fr_plot.csv"), read.csv)
fr_plot <- c(fr_plot, fr_plot2, fr_plot3)

name_key <- dat %>%
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
                     area_level =0,
                     variable = "tfr")
            )%>%
  bind_rows(read.csv("global/phia_asfr_admin0.csv") %>%
              rename(value = asfr) %>%
              select(-tips) %>%
              separate(survey_id, into=c("iso3", NA), sep=3, remove=FALSE) %>%
              mutate(area_id = iso3,
                     area_level =0,
                     variable = "asfr")
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
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
  filter(iso3 %in% names(fr_plot)) %>%
  arrange(iso3) %>%
  group_by(iso3)

comparison_plot <- comparison_fr %>%
  group_split() %>%
  setNames(unique(comparison_fr$iso3))

# dat <- dat[names(dat) != "AGO"]
# fr_plot <- compact(fr_plot)

get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  # xmin = gb$layout$panel_params[[1]]$x.range[1]
  # xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(ymin = ymin, ymax = ymax)
}

tfr_plots <- Map(function(dat1, fr_plot, comparison_plot) {
  iso3_c <- unique(dat1$iso3)
  cntry_name <- countrycode::countrycode(iso3_c, "iso3c", "country.name")
  
  remove_survey <- c("CIV2005AIS", "MLI2009MICS", "MLI2015MICS", "SLE2010MICS", "TGO2006MICS", "BEN1996DHS", "KEN2009MICS", "COD2017MICS", "TZA2007AIS", "TZA2012AIS")
  
  
  fr_plot <- fr_plot %>%
    mutate(area_id = ifelse(is.na(area_id), iso3_c, area_id)) %>%
    filter(value > 0.5, area_level ==0, variable == "tfr")
  
  tfr_plot <- dat1 %>%
    filter(area_level == 0, variable == "tfr", period > 1999) %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, value > 1.5, period > 1999, !survey_id %in% remove_survey), aes(y=value, color=survey_id)) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, value > 1.5, period > 1999, survey_id %in% remove_survey), aes(y=value, color=survey_id), shape=22, fill=NA) +
    # facet_wrap(~area_id, ncol=5) +
    labs(y="TFR", x=element_blank(), title=paste(iso3_c, "| National TFR")) +
    moz.utils::standard_theme()
  
  limits <- get_plot_limits(tfr_plot)
  
  tfr_plot <- tfr_plot + lims(y=c(limits$ymin, limits$ymax))
  
  comparison_tfr_plot <- dat1 %>%
    mutate(source = "dfertility") %>%
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
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=0.5, show.legend = FALSE) +
    geom_step(data = comparison_plot %>% filter(area_level == 0, variable == "tfr", source == "WPP2019"), aes(y=val, color=source), size=1) +
    labs(y="TFR", x=element_blank(), title=paste(iso3_c, "| National TFR")) +
    lims(y=c(limits$ymin, limits$ymax)) +
    moz.utils::standard_theme()
  
  ggpubr::ggarrange(tfr_plot, comparison_tfr_plot, nrow = 1)
  
}, dat, fr_plot[names(dat)], comparison_plot[names(dat)])


pdf(paste0("~/Downloads/tfr_comparison.pdf"), h = 6, w = 16)
tfr_plots
dev.off()

###

tips_fe <- sd_report %>%
  bind_rows() %>%
  filter(hyper %in% c("beta_tips_dummy_5", "beta_tips_dummy_6", "beta_tips_dummy_10")) %>%
  mutate(idx = str_remove(hyper, "beta_tips_dummy_")) %>%
  type_convert() %>%
  select(est_fe = Estimate, iso3 = iso, idx)

sd_report %>%
  bind_rows() %>%
  filter(!iso %in% c("CAF", "GNB")) %>%
  filter(hyper == "u_tips") %>%
  left_join(sd_report %>%
              bind_rows() %>%
              filter(hyper == "log_prec_rw_tips") %>%
              rename(log_prec_rw_tips = Estimate) %>%
              select(iso, log_prec_rw_tips)
  ) %>%
  mutate(sd = sqrt(1/exp(log_prec_rw_tips)),
         scaled = Estimate * sd) %>%
  rename(iso3 = iso) %>%
  group_by(iso3) %>%
  mutate(idx = row_number()-1) %>%
  left_join(tips_fe) %>%
  mutate(scaled = ifelse(!is.na(est_fe), scaled + est_fe, scaled),
         iso3 = paste0(iso3, " | ", round(sd, 2))) %>%
  ggplot(aes(x=idx, y=scaled)) +
    geom_line() +
    geom_point() +
    facet_wrap(~iso3) +
    labs(title = "Scaled TIPS RW coefficients + FE") +
  standard_theme()



sd_report %>%
  bind_rows() %>%
  filter(hyper == "log_prec_rw_tips",
         !iso %in% c("CAF", "GNB")) %>%
  View()
  rename(log_prec_rw_tips = Estimate) %>%
  select(iso, log_prec_rw_tips) %>%
  mutate(sd = sqrt(1/exp(log_prec_rw_tips)))

         
asfr_plots <- Map(function(dat1, fr_plot, comparison_plot) {
  iso3_c <- unique(dat1$iso3)
  cntry_name <- countrycode::countrycode(iso3_c, "iso3c", "country.name")
  
  remove_survey <- c("CIV2005AIS", "MLI2009MICS", "MLI2015MICS", "SLE2010MICS", "TGO2006MICS", "BEN1996DHS", "KEN2009MICS", "COD2017MICS", "TZA2007AIS", "TZA2012AIS")
  
  
  fr_plot <- fr_plot %>%
    mutate(area_id = ifelse(is.na(area_id), iso3_c, area_id)) %>%
    filter(variable == "asfr", area_level ==0) %>%
    left_join(naomi::get_age_groups() %>% select(age_group, age_group_label))
  
  asfr_plot <- dat1 %>%
    filter(area_level == 0, variable == "asfr", period > 1999) %>%
    left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = fr_plot %>% filter(variable == "asfr", value <0.5, period > 1999, !survey_id %in% remove_survey), aes(y=value, color=survey_id)) +
    geom_point(data = fr_plot %>% filter(variable == "asfr", value <0.5, period > 1999, survey_id %in% remove_survey), aes(y=value, color=survey_id), shape=22, fill=NA) +
    facet_wrap(~age_group_label, nrow = 1) +
    labs(y="ASFR", x=element_blank(), title=paste(iso3_c, "| National ASFR")) +
    moz.utils::standard_theme()
  
  limits <- get_plot_limits(asfr_plot)
  
  asfr_plot <- asfr_plot + lims(y=c(limits$ymin, limits$ymax))
  
  comparison_asfr_plot <- dat1 %>%
    mutate(source = "dfertility") %>%
    left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
    filter(area_level == 0, variable == "asfr", period > 1999) %>%
    bind_rows(
      comparison_plot %>% 
        filter(area_level == 0, 
               variable == "asfr", 
               source == "GBD2019") %>%
        rename(median = val)
    ) %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1, aes(color=source)) +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=0.5, show.legend = FALSE) +
    geom_step(data = comparison_plot %>% filter(area_level == 0, variable == "asfr", source == "WPP2019", val < 0.5), aes(y=val, color=source), size=1) +
    labs(y="ASFR", x=element_blank(), title=paste(iso3_c, "| National ASFR")) +
    facet_wrap(~age_group_label, nrow=1) +
    moz.utils::standard_theme()
  
  ggpubr::ggarrange(asfr_plot, comparison_asfr_plot, nrow = 2)
  
}, dat, fr_plot[names(dat)], comparison_plot[names(dat)])

pdf(paste0("~/Downloads/asfr_comparison.pdf"), h = 10, w = 18)
asfr_plots
dev.off()
