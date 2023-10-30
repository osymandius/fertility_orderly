library(dplyr)
library(tidyr)
library(dfertility)
library(ggplot2)
library(naomi)
library(tidyverse)
library(sf)
library(stringr)
library(TMB)
library(Matrix)
library(forcats)
# library(parallel)
library(purrr)
library(rlang)
library(mgcv)
library(moz.utils)

tmb_unload <- function(name) {
  ldll <- getLoadedDLLs() 
  idx  <- grep(name, names(ldll))
  for (i in seq_along(idx)) dyn.unload(unlist(ldll[[idx[i]]])$path)
  cat('Unload ', length(idx), "loaded versions.\n")
}


time_options <- crossing(time = c("rw", "rw2", "ar1", "arima"),
                         trend = c(0,1))

time_map_df <- crossing(iso3 = ssa_iso3()[!ssa_iso3() %in% c("SSD", "CAF", "GNB", "ERI", "BWA", "GNQ", "NGA", "COD", "CMR", "BDI")],
                        time_options)

iso3 <- time_map_df[1,]$iso3
time <- time_map_df[1,]$time
trend <- time_map_df[1,]$trend

fit <- function(iso3, time, trend, model_level = "district") {
  message(iso3)
  iso3_c <- iso3

  lvl_map <- read.csv("global/iso_mapping_fit.csv")
  
  if(str_detect(model_level, "district")) {
    
    asfr <- readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c, "_asfr.rds"))$district 
    phia_asfr <- read_csv("global/phia_asfr.csv", show_col_types = F) %>%
                    moz.utils::separate_survey_id(F) %>% 
                    filter(iso3 == iso3_c) %>%
                    mutate(survtype = "PHIA")
    fr_plot <- bind_rows(readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c,  "/", iso3_c, "_fr_plot.rds"))) %>%
      bind_rows(phia_asfr %>%
                  group_by(iso3, survey_id, year, period, area_id) %>%
                  summarise(value = 5*sum(asfr),
                            pys = sum(pys)) %>%
                  mutate(variable = "tfr"))
    naomi_level <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
    
  } else if(str_detect(model_level, "provincial")) {
    
    asfr <- readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c,  "/", iso3_c, "_asfr.rds"))$provincial
    phia_asfr <- read_csv("global/phia_asfr_admin1.csv", show_col_types = F) %>% 
                    moz.utils::separate_survey_id(F) %>% 
                    filter(iso3 == iso3_c) %>% 
                    rename(area_id = area_id1) %>%
                    mutate(survtype = "PHIA")
    fr_plot <- bind_rows(readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c,  "_fr_plot.rds"))) %>%
      bind_rows(phia_asfr %>%
                  group_by(iso3, survey_id, year, period, area_id) %>%
                  summarise(value = 5*sum(asfr),
                            pys = sum(pys)) %>%
                  mutate(variable = "tfr"))
    naomi_level <- lvl_map$admin1_level[lvl_map$iso3 == iso3]
    
  } else {
    
    asfr <- readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c,  "_asfr.rds"))$national
    phia_asfr <- read_csv("global/phia_asfr_admin0.csv", show_col_types = F) %>% 
                    moz.utils::separate_survey_id(F) %>% 
                    filter(iso3 == iso3_c) %>% 
                    mutate(area_id = iso3,
                           survtype = "PHIA")
    fr_plot <- readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c,  "_fr_plot.rds"))$national %>%
      bind_rows(phia_asfr %>%
                  group_by(iso3, survey_id, year, period, area_id) %>%
                  summarise(value = 5*sum(asfr),
                            pys = sum(pys)) %>%
                  mutate(variable = "tfr"))
    naomi_level <- 0
  }
  
  
  admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]
  
  population <- readRDS("global/pop.rds")[[iso3_c]] %>%
    # read.csv("depends/interpolated_population.csv") %>%
    rename(period = year) %>%
    mutate(iso3 = iso3) %>%
    filter(sex == "female")
  
  
  areas <- readRDS("global/areas.rds")[[iso3_c]]%>%
    mutate(iso3 = iso3_c) %>%
    st_make_valid()
  
  # asfr <- read.csv("depends/asfr.csv")
  
  # phia_asfr <- read_csv("global/phia_asfr.csv", show_col_types = F) %>%
  #   separate(area_id, into=c("iso3", NA), sep = 3, remove = FALSE) %>%
  #   filter(iso3 == iso3_c) %>%
  #   mutate(survtype = "PHIA")
  
  remove_survey <- c("CIV2005AIS",
                     # "CIV2006MICS", 
                     "GMB2005MICS",
                     # "MLI2009MICS", "MLI2015MICS", 
                     "SLE2010MICS", 
                     "TGO2006MICS", 
                     # "BEN1996DHS",
                     "KEN2009MICS", 
                     # "COD2017MICS", 
                     "NGA2007MICS",
                     "TZA2007AIS", "TZA2012AIS")
  subnational_surveys <- c("KEN2009MICS", "KEN2011MICS")
  
  asfr <- asfr %>% 
    bind_rows(phia_asfr) %>%
    filter(!survey_id %in% remove_survey,
           !(iso3 == "SWZ" & period == 2017), ## This seems dodgy
           !(period == 1995 & survey_id == "SWZ2000MICS"),
           !(period == 1999 & survey_id == "SWZ2014MICS"),
           !(period == 2004 & survey_id == "GMB2019DHS"),
           !(period == 2005 & survey_id == "GMB2010MICS"),
           !(period == 2013 & survey_id == "GMB2018MICS"),
           !(tips == 5 & survey_id == "BEN2014MICS"),
           !(tips == 6 & survey_id == "MOZ2015AIS"),
           !(tips == 5 & survey_id == "TGO2017MICS"),
           !(tips == 5 & survey_id == "SLE2017MICS"),
           !(survey_id == "AGO2006MIS" & tips > 5),
           !(survey_id == "TCD2019MICS" & tips > 4),
           !(survey_id == "SWZ2000MICS" & tips > 4),
           !(survey_id == "NGA2021MIS" & tips > 4),
           !(survey_id == "MLI2015MIS" & tips > 5),
           !(survey_id == "MLI2015MICS" & tips > 4),
           !(survey_id == "MLI2009MICS" & tips > 4),
           !(survey_id == "COD2017MICS" & tips > 10),
           
    ) %>%
    filter(period < 2021) ## THIS IS BAD - PROJECT THE POPULPATIONS BEYOND 2020
  
  # debugonce(make_model_frames_dev)
  mf <- make_model_frames_dev(iso3_c, population, asfr,  areas, naomi_level, project=2020)
  
  mf$observations$full_obs <- mf$observations$full_obs %>%
    separate_survey_id() %>%
    rename(survyear = year)
  
  end_year <- max(mf$observations$full_obs$survyear) - 5
  
  validate_model_frame(mf, areas, naomi_level)
  # # 
  # library(tidyverse)
  # library(Matrix)
  # library(dfertility)
  # library(TMB)
  
  # Z_period <- Matrix::sparse.model.matrix(~0 + period, mf$mf_model)
  # x <- 0:25
  # k <- seq(-15, 40, by = 5)
  # spline_mat <- splines::splineDesign(k, x, ord = 4)
  # spline_mat <- as(spline_mat, "sparseMatrix")
  # Z_period <- Z_period %*% spline_mat
  
  # tmb_int <- make_tmb_inputs(iso3, mf, naomi_level)
  
  mf$observations$full_obs <- mf$observations$full_obs %>% 
    ungroup() %>% 
    mutate(survtype = ifelse(survtype %in% c("AIS", "MIS"), "AIS-MIS", survtype)) %>%
    group_by(survtype) %>% 
    mutate(row_num = row_number())
  
  
  dhs_ppd_join <- mf$observations$full_obs %>%
    filter(survtype == "DHS") %>%
    ungroup() %>%
    dplyr::mutate(col_idx = dplyr::row_number()) %>%
    dplyr::select(col_idx, survyear) %>%
    type.convert(as.is = T) %>%
    dplyr::filter(survyear < end_year) %>%
    dplyr::mutate(row_idx = dplyr::row_number(),
                  x=1)
  
  X_extract_dhs_ppd <- Matrix::spMatrix(
    nrow(dhs_ppd_join),
    nrow(mf$observations$full_obs %>% filter(survtype == "DHS")),
    i = dhs_ppd_join$row_idx,
    j = dhs_ppd_join$col_idx,
    x = dhs_ppd_join$x
  )
  
  ais_ppd_join <- mf$observations$full_obs %>%
    filter(survtype == "AIS-MIS") %>%
    ungroup() %>%
    dplyr::mutate(col_idx = dplyr::row_number()) %>%
    dplyr::select(col_idx, survyear) %>%
    type.convert(as.is = T) %>%
    dplyr::filter(survyear < end_year) %>%
    dplyr::mutate(row_idx = dplyr::row_number(),
                  x=1)
  
  X_extract_ais_ppd <- Matrix::spMatrix(
    nrow(ais_ppd_join),
    nrow(mf$observations$full_obs %>% filter(survtype == "AIS-MIS")),
    i = ais_ppd_join$row_idx,
    j = ais_ppd_join$col_idx,
    x = ais_ppd_join$x
  )
  
  phia_ppd_join <- mf$observations$full_obs %>%
    filter(survtype == "PHIA") %>%
    ungroup() %>%
    dplyr::mutate(col_idx = dplyr::row_number()) %>%
    dplyr::select(col_idx, survyear) %>%
    type.convert(as.is = T) %>%
    dplyr::filter(survyear < end_year) %>%
    dplyr::mutate(row_idx = dplyr::row_number(),
                  x=1)
  
  X_extract_phia_ppd <- Matrix::spMatrix(
    nrow(phia_ppd_join),
    nrow(mf$observations$full_obs %>% filter(survtype == "PHIA")),
    i = phia_ppd_join$row_idx,
    j = phia_ppd_join$col_idx,
    x = phia_ppd_join$x
  )
  
  mics_ppd_join <- mf$observations$full_obs %>%
    filter(survtype == "MICS") %>%
    ungroup() %>%
    dplyr::mutate(col_idx = dplyr::row_number()) %>%
    dplyr::select(col_idx, survyear) %>%
    type.convert(as.is = T) %>%
    dplyr::filter(survyear < end_year) %>%
    dplyr::mutate(row_idx = dplyr::row_number(),
                  x=1)
  
  X_extract_mics_ppd <- Matrix::spMatrix(
    nrow(mics_ppd_join),
    nrow(mf$observations$full_obs %>% filter(survtype == "MICS")),
    i = mics_ppd_join$row_idx,
    j = mics_ppd_join$col_idx,
    x = mics_ppd_join$x
  )
  
  tmb_int <- list()
  
  tmb_int$data <- list(
    M_naomi_obs = mf$M_naomi_obs,
    M_full_obs = mf$M_full_obs,
    # X_tips_dummy = mf$Z$X_tips_dummy,
    # X_tips_dummy_9_11 = mf$Z$X_tips_dummy_9_11,
    # X_tips_dummy_5 = mf$Z$X_tips_dummy_5,
    X_tips_fe = mf$Z$X_tips_fe,
    X_period = mf$Z$X_period,
    X_urban_dummy = mf$Z$X_urban_dummy,
    X_extract_dhs = mf$X_extract$X_extract_dhs,
    X_extract_ais = mf$X_extract$X_extract_ais,
    X_extract_mics = mf$X_extract$X_extract_mics,
    X_extract_phia = mf$X_extract$X_extract_phia,
    Z_tips = sparse.model.matrix(~0 + tips_f, mf$observations$full_obs),
    # Z_tips_dhs = mf$Z$Z_tips_dhs,
    # Z_tips_ais = mf$Z$Z_tips_ais,
    Z_age = mf$Z$Z_age,
    # Z_period = mf$Z$Z_period,
    Z_period = mf$Z$Z_period,
    Z_spatial = mf$Z$Z_spatial,
    # Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
    # Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
    # Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
    Z_interaction1 = mgcv::tensor.prod.model.matrix(list(mf$Z$Z_age, mf$Z$Z_period, mf$Z$Z_country)),
    Z_interaction2 = mgcv::tensor.prod.model.matrix(list(mf$Z$Z_spatial, mf$Z$Z_period)),
    Z_interaction3 = mgcv::tensor.prod.model.matrix(list(mf$Z$Z_spatial, mf$Z$Z_age)),
    Z_country = mf$Z$Z_country,
    # Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
    # Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
    Z_smooth_iid = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs),
    # Z_smooth_iid_ais = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
    R_smooth_iid = mf$R$R_smooth_iid,
    R_tips = mf$R$R_tips,
    R_tips_iid = as(diag(1, ncol(mf$Z$Z_tips_dhs)), "dgTMatrix"),
    # Z_zeta1 = sparse.model.matrix(~0 + id.zeta1, mf$observations$full_obs),
    Z_zeta2 = mf$Z$Z_zeta2,
    R_zeta2 = as(diag(1, 4), "dgTMatrix"),
    R_survey = as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix"),
    R_age = mf$R$R_age,
    # R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
    R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
    # R_spline_mat = spline_mat,
    R_spatial = mf$R$R_spatial,
    R_spatial_iid = mf$R$R_spatial_iid,
    R_country = mf$R$R_country,
    rankdef_R_spatial = 1,
    
    log_offset_naomi = log(mf$observations$naomi_level_obs$pys),
    births_obs_naomi = mf$observations$naomi_level_obs$births,
    
    # log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
    # births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
    # 
    # log_offset_ais = log(filter(mf$observations$full_obs, survtype == "AIS-MIS")$pys),
    # births_obs_ais = filter(mf$observations$full_obs, survtype == "AIS-MIS")$births,
    # 
    # log_offset_phia = log(filter(mf$observations$full_obs, survtype == "PHIA")$pys),
    # births_obs_phia = filter(mf$observations$full_obs, survtype == "PHIA")$births,
    
    log_offset = log(mf$observations$full_obs$pys),
    births_obs = mf$observations$full_obs$births,
    
    X_extract_dhs_ppd = X_extract_dhs_ppd,
    X_extract_ais_ppd = X_extract_ais_ppd,
    X_extract_phia_ppd = X_extract_phia_ppd,
    X_extract_mics_ppd = X_extract_mics_ppd,
    
    # include_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear < end_year)$row_num,
    # include_ais_obs = filter(mf$observations$full_obs, survtype == "AIS-MIS", survyear < end_year)$row_num,
    # include_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear < end_year)$row_num,
    # include_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear < end_year)$row_num,
    # 
    # exclude_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear >= end_year)$row_num,
    # exclude_ais_obs = filter(mf$observations$full_obs, survtype == "AIS-MIS", survyear >= end_year)$row_num,
    # exclude_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear >= end_year)$row_num,
    # exclude_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear >= end_year)$row_num,
    
    pop = mf$mf_model$population,
    # A_asfr_out = mf$out$A_asfr_out,
    A_tfr_out = mf$out$A_tfr_out,
    
    A_full_obs = mf$observations$A_full_obs,
    
    mics_toggle = mf$mics_toggle,
    mics_only_toggle = as.integer(nrow(mf$X_extract$X_extract_mics) == nrow(mf$observations$full_obs)),
    eth_toggle = as.integer(iso3_c == "ETH"),
    zwe_toggle = as.integer(iso3_c == "ZWE"),
    mwi_rwa_toggle = as.integer(iso3_c %in% c("MWI", "RWA")),
    mwi_toggle = as.integer(iso3_c == "MWI"),
    subnational_toggle = as.integer(naomi_level > 0),
    multiple_survey_toggle = as.integer(length(unique(mf$observations$full_obs$survey_id)) > 1),
    
    rw_toggle = 0,
    ar1_toggle = 0,
    arima_toggle = 0,
    trend_toggle = 0,
    
    X_spike_2010 = mf$Z$X_spike_2010,
    # X_spike_2010 = matrix(0),
    
    X_spike_2000 = model.matrix(~0 + spike_2000, mf$observations$full_obs),
    X_spike_1999 = model.matrix(~0 + spike_1999, mf$observations$full_obs),
    X_spike_2001 = model.matrix(~0 + spike_2001, mf$observations$full_obs)
    
    # X_psi_2000 = model.matrix(~0 + spike_2000:survey_id, mf$observations$full_obs),
    # X_psi_1999 = model.matrix(~0 + spike_1999:survey_id, mf$observations$full_obs),
    # X_psi_2001 = model.matrix(~0 + spike_2001:survey_id, mf$observations$full_obs)
  )
  
  tmb_int$par <- list(
    beta_0 = 0,
    
    # beta_tips_dummy_5 = rep(0, ncol(mf$Z$X_tips_dummy_5)),
    beta_tips_fe = rep(0, ncol(mf$Z$X_tips_fe)),
    
    u_age = rep(0, ncol(mf$Z$Z_age)),
    log_prec_rw_age = 0,
    lag_logit_phi_age = 0,
    
    # zeta1 = array(0, c(length(unique(mf$observations$full_obs$survey_id)), ncol(mf$Z$Z_tips_dhs))),
    # log_prec_zeta1 = 0,
    # lag_logit_zeta1_phi_tips = 0,
    
    # u_country = rep(0, ncol(mf$Z$Z_country)),
    # log_prec_country = 0,
    
    # omega1 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_age))),
    # log_prec_omega1 = 0,
    # lag_logit_omega1_phi_age = 0,
    #
    # omega2 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_period))),
    # log_prec_omega2 = 0,
    # lag_logit_omega2_phi_period = 0,
    
    u_period = rep(0, ncol(mf$Z$Z_period)),
    log_prec_rw_period = 0,
    # logit_phi_period = 0,
    # lag_logit_phi_period = 0,
    # lag_logit_phi_arima_period = 0,
    # beta_period = 0,
    
    log_prec_smooth_iid = 0,
    u_smooth_iid = rep(0, ncol(mf$R$R_smooth_iid)),
    
    # log_overdispersion = 0,
    
    eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
    log_prec_eta1 = 0,
    logit_eta1_phi_age = 0,
    logit_eta1_phi_period = 0
  )
  
  tmb_int$random <- c("beta_0",
                      "u_age",
                      "u_period",
                      "u_smooth_iid",
                      # "beta_period",
                      "beta_tips_fe",
                      "eta1"
  )
  
  if(naomi_level != 0) {
    tmb_int$par <- c(tmb_int$par,
                     "u_spatial_str" = list(rep(0, ncol(mf$Z$Z_spatial))),
                     "log_prec_spatial" = 0,
                     
                     "eta2" = list(array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period)))),
                     "log_prec_eta2" = 0,
                     "logit_eta2_phi_period" = 0,
                     # #
                     "eta3" = list(array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age)))),
                     "log_prec_eta3" = 0,
                     "logit_eta3_phi_age" = 0)
    
    tmb_int$random <- c(tmb_int$random,
                        "u_spatial_str",
                        "eta2",
                        "eta3")
  }
  
  if(mf$mics_toggle) {
    tmb_int$data <- c(tmb_int$data,
                      "Z_tips_mics" = mf$Z$Z_tips_mics,
                      "R_tips_mics" = mf$R$R_tips_mics,
                      "log_offset_mics" = list(log(filter(mf$observations$full_obs, survtype == "MICS")$pys)),
                      "births_obs_mics" = list(filter(mf$observations$full_obs, survtype == "MICS")$births)
    )
  }
  
  if(iso3_c == "ETH") {
    tmb_int$par <- c(tmb_int$par,
                      "beta_urban_dummy" = 0
    )
    
    tmb_int$random <- c(tmb_int$random, "beta_urban_dummy")
  }
  
  if(iso3_c == "ZWE") {
    tmb_int$par <- c(tmb_int$par,
                     list("beta_spike_2010" = rep(0, ncol(mf$Z$X_spike_2010)))
    )
    
    tmb_int$random <- c(tmb_int$random, "beta_spike_2010")
  }
  
  if(length(unique(mf$observations$full_obs$survey_id)) > 1) {
    tmb_int$random <- c(tmb_int$random,
                        # "u_tips"
                        "zeta2"
    )
    
    tmb_int$par <- c(tmb_int$par,
                     # "u_tips" = list(rep(0, ncol(mf$Z$Z_tips_dhs))),
                     # "log_prec_rw_tips" = 0,
                     # "lag_logit_phi_tips" = 0
                     "zeta2" = list(array(0, c(ncol(as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix")),
                                               ncol(mf$Z$X_tips_fe)
                     )
                     )),
                     "log_prec_zeta2" = 0
    )
  }
  
  if(!iso3 %in% c("MWI", "RWA")) {
    tmb_int$par <- c(tmb_int$par,
                     "beta_spike_2000" = 0,
                     "beta_spike_1999" = 0,
                     "beta_spike_2001" = 0
                     # "psi_2000" = list(rep(0, ncol(model.matrix(~0 + spike_2000:survey_id, mf$observations$full_obs)))),
                     # "psi_1999" = list(rep(0, ncol(model.matrix(~0 + spike_1999:survey_id, mf$observations$full_obs)))),
                     # "psi_2001" = list(rep(0, ncol(model.matrix(~0 + spike_2001:survey_id, mf$observations$full_obs))))
    )
    
    tmb_int$random <- c(tmb_int$random,
                        "beta_spike_2000",
                        "beta_spike_1999",
                        "beta_spike_2001"
                        # "psi_2000",
                        # "psi_1999",
                        # "psi_2001"
    )
    
  }
  
  if(iso3 == "MWI") {
    
    mf$mf_model <- mf$mf_model %>%
      mutate(spike_famine = factor(ifelse(period %in% 2001:2002, id.period-5, 0)))
    
    mf$Z$X_spike_famine <- sparse.model.matrix(~0 + spike_famine, mf$mf_model)[,2:3]
    
    tmb_int$par <- c(tmb_int$par,
                     "beta_spike_famine" = list(c(0,0))
    )
    
    tmb_int$data <- c(tmb_int$data,
                      "X_spike_famine" = list(mf$Z$X_spike_famine)
    )
    
    tmb_int$random <- c(tmb_int$random, "beta_spike_famine")
  }
  
  tmb_int_curr <- tmb_int
  
  if(time == "ar1") {
    tmb_int_curr$par <- c(tmb_int$par, "lag_logit_phi_period" = 0)
    tmb_int_curr$data$ar1_toggle <- 1
    
  } else if(time == "arima") {
    tmb_int_curr$par <- c(tmb_int$par,"lag_logit_phi_arima_period" = 0)
    tmb_int_curr$data$arima_toggle <- 1
    
  } else if(time == "rw1") {
    tmb_int_curr$data$R_period <- make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE)
    tmb_int_curr$data$rw_toggle <- 1
  } else {
    tmb_int_curr$data$R_period <- make_rw_structure_matrix(ncol(mf$Z$Z_period), 2, adjust_diagonal = TRUE)
    tmb_int_curr$data$rw_toggle <- 1
  }
  
  if(trend == 1) {
    tmb_int_curr$par <- c(tmb_int_curr$par,
                          "beta_period" = 0
    )
    
    tmb_int_curr$random <- c(tmb_int$random, "beta_period")
    
    tmb_int_curr$data$trend_toggle <- 1
  }

  TMB::compile("src/aaa_fit/models/2023_09_15_all_levels_time.cpp", flags = "-w")               # Compile the C++ file
  dyn.load(TMB::dynlib("src/aaa_fit/models/2023_09_15_all_levels_time"))
  
  # tmb_unload <- function(name) {
  #   ldll <- getLoadedDLLs() 
  #   idx  <- grep(name, names(ldll))
  #   for (i in seq_along(idx)) dyn.unload(unlist(ldll[[idx[i]]])$path)
  #   cat('Unload ', length(idx), "loaded versions.\n")
  # }

  f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int_curr$data,
                                              parameters = tmb_int_curr$par,
                                              DLL = "2023_09_15_all_levels_time",
                                              silent=0,
                                              checkParameterOrder=FALSE)
  })
    # #
  if(is.null(parallel::mccollect(f)[[1]])) {
      stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
  }
  
  # lapply(list.files("src/aaa_fit/models/", pattern = "\\.o|\\.so", full.names = T), file.remove)
  # tmb_unload("2023_09_15_all_levels_time")
    
  obj <-  TMB::MakeADFun(data = tmb_int_curr$data,
                           parameters = tmb_int_curr$par,
                           DLL = "2023_09_15_all_levels_time",
                           random = tmb_int_curr$random,
                           hessian = FALSE)
  
  f <- stats::nlminb(obj$par, obj$fn, obj$gr)
  f$par.fixed <- f$par
  f$par.full <- obj$env$last.par
  
  fit <- c(f, obj = list(obj))

  fit$sdreport <- sdreport(fit$obj, fit$par)

  sd_report <- fit$sdreport
  sd_report <- summary(sd_report, "all")

  sd_report <- data.frame(sd_report, "hyper" = rownames(sd_report), iso = iso3)
  
  # write_csv(sd_report, file.path("outside_orderly/fit/outputs", iso3_c, model_level, "sd_report.csv"))
  
  class(fit) <- "naomi_fit"  # this is hacky...
  fit <- naomi::sample_tmb(fit, random_only=TRUE)
  tmb_results <- dfertility::tmb_outputs(fit, mf, areas)
  
  fr_plot <- fr_plot %>%
    left_join(areas %>% sf::st_drop_geometry() %>% select(area_id, area_name)) %>%
    filter(!(area_id == iso3 & survey_id %in% subnational_surveys))
  
  cntry_name <- countrycode::countrycode(iso3, "iso3c", "country.name")
  
  tfr_plot <- tmb_results %>%
    filter(area_level %in% c(0, admin1_lvl), variable == "tfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = fr_plot %>% 
                 filter(variable == "tfr", value <10, !survey_id %in% remove_survey) %>%
                 left_join(areas %>% st_drop_geometry()) %>%
                 filter(area_level %in% c(0, admin1_lvl)), aes(y=value, color=survey_id, size = pys)) +
    geom_point(data = fr_plot %>% 
                 filter(variable == "tfr", value <10, survey_id %in% remove_survey) %>%
                 left_join(areas %>% st_drop_geometry()) %>%
                 filter(area_level %in% c(0, admin1_lvl)), aes(y=value, color=survey_id, size = pys), shape=22, fill=NA) +
    facet_wrap(~fct_inorder(area_name), ncol=5) +
    # facet_wrap(~area_name) +
    labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )
  
  tfr_plot
  
  write_csv(tmb_results, file.path("outside_orderly/fit/outputs", iso3_c, model_level, "fr.csv"))
  # saveRDS(fit$sample, file.path("outside_orderly/fit/outputs/", iso3_c, model_level, "fixed.rds"))
  
  quant_pos_sum <- function(births, x) {
    if(births < x)
      0
    else
      1
  }
  
  dhs_ppd <- mf$observations$full_obs %>%
    ungroup() %>%
    filter(survtype == "DHS", 
           # survyear >= end_year
           )
  
  # dhs_ppd <- dhs_ppd %>%
  #   filter(row_num %in% tmb_int_curr$data$exclude_dhs_obs)
  #
  
  ## These two bits should go in that if statement.
  dhs_num <-  as.numeric(X_extract_dhs_ppd %*% mf$X_extract$X_extract_dhs %*% mf$observations$full_obs$row_num)
  
  dhs_ppd <- dhs_ppd %>%
    type.convert(as.is = T) %>%
    select(survey_id, area_id, period, age_group, births, tips, pys, row_num) %>%
    bind_cols(data.frame(exp(fit$sample$log_rate_pred_dhs))) %>%
    filter(!row_num %in% as.numeric(dhs_num))
  
  if(nrow(dhs_ppd)) {
    # dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
    dhsMatrix <- X_extract_dhs_ppd %*% exp(fit$sample$log_rate_pred_dhs)
    dhs_ppd <- dhs_ppd %>% 
      select(survey_id, area_id, period, age_group, births, tips, pys) %>%
      type.convert(as.is = T) %>%
      bind_cols(data.frame(as.matrix(dhsMatrix)))
      
      # filter(period >= end_year)
  } else {
    dhs_ppd <- data.frame()
  }
  
  ais_ppd <- mf$observations$full_obs %>%
    ungroup() %>%
    filter(survtype == "AIS-MIS", survyear >= end_year)
  
  
  if(nrow(ais_ppd)) {
    aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
    ais_ppd <- ais_ppd %>%
      select(survey_id, area_id, period, age_group, births, tips, pys) %>%
      bind_cols(data.frame(aisMatrix)) %>%
      type.convert() %>%
      filter(period >= end_year)
  } else {
    ais_ppd <- data.frame()
  }
  
  phia_ppd <- mf$observations$full_obs %>%
    ungroup() %>%
    filter(survtype == "PHIA", survyear >= end_year)
  
  
  if(nrow(phia_ppd)) {
    phiaMatrix <- exp(fit$sample$log_rate_exclude_phia)
    phia_ppd <- phia_ppd %>% 
      select(survey_id, area_id, period, age_group, births, tips, pys) %>%
      bind_cols(data.frame(phiaMatrix)) %>%
      type.convert() %>%
      filter(period >= end_year)
  } else {
    phia_ppd <- data.frame()
  }
  
  mics_ppd <- mf$observations$full_obs %>%
    ungroup() %>%
    filter(survtype == "MICS", survyear >= end_year)
  
  
  if(nrow(mics_ppd)) {
    micsMatrix <- exp(fit$sample$log_rate_exclude_mics)
    mics_ppd <- mics_ppd %>% 
      select(survey_id, area_id, period, age_group, births, tips, pys) %>%
      bind_cols(data.frame(micsMatrix)) %>%
      type.convert() %>%
      filter(period >= end_year)
  } else {
    mics_ppd <- data.frame()
  }
  
  dxpois <- function(x, lambda, log = TRUE) {
    val <- x * log(lambda) - lambda - lgamma(x+1)
    if (log) {
      val <- exp(val)
    }
    val
  }
  
  
  ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd) %>%
    ungroup() %>%
    mutate(idx = row_number())
  
  true_values <- ppd %>%
    select(births, pys) %>%
    mutate(observed_asfr = births/pys) %>%
    .$observed_asfr
  
  predictions <- ppd %>%
    select(X1:X1000) %>%
    as.matrix()
  
  crps = sum(scoringutils::crps_sample(true_values, predictions))
  
  est_births <- as.matrix(ppd[,paste0("X", 1:1000)]) * ppd$pys
  ll_pred <- dxpois(ppd$births, est_births, TRUE)
  elpd <- loo::elpd(t(ll_pred))
  elpd <- elpd$estimates %>%
    as.data.frame() %>%
    mutate(source = paste0(time, " ", trend))
  
  system.time(ppd %>%
                ungroup() %>%
                rowwise() %>%
                # group_by(idx) %>%
                mutate(across(starts_with("X"), ~rpois(1, pys*.x))))
  
  int <- rowMeans(ppd[,paste0("X", 1:1000)])
  ppd %>%
    select(!starts_with("X")) %>%
    mutate(mean = int)
  
  # poisson_sample_births <- function(x) {
  #   pys <- as.numeric(x["pys"])
  #   int <- as.numeric(x[paste0("X", 1:1000)])
  #   births <- sapply(int, function(x) rpois(1, pys * x))
  #   x[paste0("X", 1:1000)] <- births
  #   x
  # }
  # 
  # flib <- ppd[1,]
  # 
  # debugonce(poisson_sample_births)
  # foo <- apply(ppd, 1, poisson_sample_births)
  # 
  # system.time(apply(ppd, 1, poisson_sample_births))
  
ppd2 <- ppd %>%
    # ungroup() %>%
    # rowwise() %>%
    group_by(idx) %>%
    mutate(across(starts_with("X"), ~rpois(1, pys*.x)))
    # type.convert() %>%
    
foo <- ppd2 %>%
    # mutate(period = plyr::round_any(period, 2, round)) %>%
    ungroup() %>%
    aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                       c(paste0("X", 1:1000), "births", "pys"),
                       1,
                       areas) %>%
    mutate(survey_asfr = births/pys,
           across(starts_with("X"), ~.x/pys)) %>%
    select(survey_id:pys, survey_asfr, everything()) %>%
    group_by(survey_id, period, area_id,tips)
  
foo2 <- foo %>%
    summarise(
      survey_tfr = 5*sum(survey_asfr),
      across(starts_with("X"), ~5*sum(.x)),
      observed_births = sum(births),
      pys = sum(pys)
    ) %>%
    ungroup() %>%
    select(survey_id:survey_tfr, observed_births, pys, everything())

rowMeans(foo2[,paste0("X", 1:1000)])
  
  qtls <- apply(select(foo2, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))
  
  foo2 <- foo2 %>%
    group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
    rowwise() %>%
    summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
    ungroup %>%
    mutate(lower = qtls[1,],
           median = qtls[2,],
           upper = qtls[3,],
           source = "rw")
  
  # fit <- naomi::sample_tmb(fit, random_only=FALSE)
  # hyper <- fit$sample %>%
  #   list_modify("lambda_out" = zap(), "tfr_out" = zap())
  # saveRDS(hyper, "hyper.rds")
  
  
  
  fr_plot <- fr_plot %>%
    filter(area_id %in% c("MWI", "MWI_1_1", "MWI_1_2", "MWI_1_3")) %>%
    left_join(areas %>% sf::st_drop_geometry() %>% select(area_id, area_name)) %>%
    filter(!(area_id == iso3 & survey_id %in% subnational_surveys))
  
  cntry_name <- countrycode::countrycode(iso3, "iso3c", "country.name")
  
  tfr_plot <- tmb_results %>%
    filter(area_level %in% c(0, admin1_lvl), variable == "tfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(size=1) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, !survey_id %in% remove_survey), aes(y=value, color=survey_id, size = pys)) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, survey_id %in% remove_survey), aes(y=value, color=survey_id, size = pys), shape=22, fill=NA) +
    facet_wrap(~fct_inorder(area_name), ncol=5) +
    # facet_wrap(~area_name) +
    labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )
  
  if(str_detect(model_level, "district")) {
    
    district_tfr <- tmb_results %>%
      filter(area_level == naomi_level, variable == "tfr") %>%
      ggplot(aes(x=period, y=median)) +
      geom_line() +
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
      facet_wrap(~area_name, ncol=8) +
      theme_minimal() +
      labs(y="TFR", x=element_blank(), title=paste(iso3, "| District TFR"))
  } else {
    district_tfr <- data.frame()
  }
  
  path <- file.path("outside_orderly/fit/outputs", iso3_c, model_level, "tfr_plot.png")
  png(path, h = 800, w = 1200)
  print(tfr_plot)
  dev.off()
  # pdf("check/tfr_district.pdf", h = 12, w = 20)
  # district_tfr
  # dev.off()
  
  tfr_plot
  
}

library(dplyr)
library(tidyr)
library(dfertility)
library(ggplot2)
library(naomi)
library(tidyverse)
library(sf)
library(stringr)
library(TMB)
library(Matrix)
library(forcats)
# library(parallel)
library(purrr)
library(rlang)
library(mgcv)

debugonce(fit)
fit("MWI", "ar1", 1, "national")
fit("MWI", "provincial")
fit("MWI", "district")
fit("ZWE", "provincial")
fit("ZWE", "district")
fit("RWA")
fit("RWA", "provincial")
fit("RWA", "district")

possibly_fit <- possibly(fit, otherwise = NULL)
id <- map(moz.utils::ssa_iso3()[moz.utils::ssa_iso3() != "AGO"], ~possibly_fit(.x, model_level = "provincial"))
