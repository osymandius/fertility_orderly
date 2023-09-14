fit <- function(iso3, model_level = "national") {
  message(iso3)
  iso3_c <- iso3

  lvl_map <- read.csv("global/iso_mapping_fit.csv")
  
  if(str_detect(model_level, "district")) {
    
    asfr <- readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c, "_asfr.rds"))$district 
    phia_asfr <- read_csv("global/phia_asfr.csv", show_col_types = F) %>% moz.utils::separate_survey_id(F) %>% filter(iso3 == iso3_c)
    fr_plot <- bind_rows(readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c,  "/", iso3_c, "_fr_plot.rds"))) %>%
      bind_rows(phia_asfr %>%
                  group_by(iso3, survey_id, year, period, area_id) %>%
                  summarise(value = 5*sum(asfr),
                            pys = sum(pys)) %>%
                  mutate(variable = "tfr"))
    naomi_level <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
    
  } else if(str_detect(model_level, "provincial")) {
    
    asfr <- readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c,  "/", iso3_c, "_asfr.rds"))$provincial
    phia_asfr <- read_csv("global/phia_asfr_admin1.csv", show_col_types = F) %>% moz.utils::separate_survey_id(F) %>% filter(iso3 == iso3_c) %>% rename(area_id = area_id1)
    fr_plot <- bind_rows(readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c,  "_fr_plot.rds"))) %>%
      bind_rows(phia_asfr %>%
                  group_by(iso3, survey_id, year, period, area_id) %>%
                  summarise(value = 5*sum(asfr),
                            pys = sum(pys)) %>%
                  mutate(variable = "tfr"))
    naomi_level <- lvl_map$admin1_level[lvl_map$iso3 == iso3]
    
  } else {
    
    asfr <- readRDS(paste0("outside_orderly/asfr/outputs/", iso3_c, "/", iso3_c,  "_asfr.rds"))$national
    phia_asfr <- read_csv("global/phia_asfr_admin0.csv", show_col_types = F) %>% moz.utils::separate_survey_id(F) %>% filter(iso3 == iso3_c) %>% mutate(area_id = iso3)
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
  
  validate_model_frame(mf, areas)
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
    
    log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
    births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
    
    log_offset_ais = log(filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$pys),
    births_obs_ais = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$births,
    
    log_offset_phia = log(filter(mf$observations$full_obs, survtype == "PHIA")$pys),
    births_obs_phia = filter(mf$observations$full_obs, survtype == "PHIA")$births,
    
    pop = mf$mf_model$population,
    # A_asfr_out = mf$out$A_asfr_out,
    A_tfr_out = mf$out$A_tfr_out,
    
    A_full_obs = mf$observations$A_full_obs,
    
    mics_toggle = mf$mics_toggle,
    mics_only_toggle = as.integer(iso3 %in% c("GNB")),
    eth_toggle = as.integer(iso3_c == "ETH"),
    zwe_toggle = as.integer(iso3_c == "ZWE"),
    mwi_rwa_toggle = as.integer(iso3_c %in% c("MWI", "RWA")),
    spatial_toggle = as.integer(naomi_level > 0),
    
    # X_spike_2010 = mf$Z$X_spike_2010,
    X_spike_2010 = matrix(0),
    
    X_spike_2000 = model.matrix(~0 + spike_2000, mf$observations$full_obs),
    X_spike_1999 = model.matrix(~0 + spike_1999, mf$observations$full_obs),
    X_spike_2001 = model.matrix(~0 + spike_2001, mf$observations$full_obs),
    
    X_psi_2000 = model.matrix(~0 + spike_2000:survey_id, mf$observations$full_obs),
    X_psi_1999 = model.matrix(~0 + spike_1999:survey_id, mf$observations$full_obs),
    X_psi_2001 = model.matrix(~0 + spike_2001:survey_id, mf$observations$full_obs)
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
    lag_logit_phi_arima_period = 0,
    beta_period = 0,
    
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
                      "beta_period",
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
                     "beta_spike_2010" = 0
    )
    
    tmb_int$random <- c(tmb_int$random, "beta_spike_2010")
  }
  
  if(!iso3 %in% c("SSD", "CAF")) {
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
  
  
  # obj <- make_tmb_obj(iso3, tmb_int)
  
  if(naomi_level == 0) {
    
    TMB::compile("src/aaa_fit/models/2023_09_11_national.cpp", flags = "-w")               # Compile the C++ file
    dyn.load(TMB::dynlib("src/aaa_fit/models/2023_09_11_national"))
    
    f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                              parameters = tmb_int$par,
                                              DLL = "2023_09_11_national",
                                              silent=0,
                                              checkParameterOrder=FALSE)
    })
    # #
    if(is.null(parallel::mccollect(f)[[1]])) {
      stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
    }
    
    obj <-  TMB::MakeADFun(data = tmb_int$data,
                           parameters = tmb_int$par,
                           DLL = "2023_09_11_national",
                           random = tmb_int$random,
                           hessian = FALSE)
    
  } else {
    
    TMB::compile("src/aaa_fit/models/2023_09_11_subnational.cpp", flags = "-w")               # Compile the C++ file
    dyn.load(TMB::dynlib("src/aaa_fit/models/2023_09_11_subnational"))
    
    f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                              parameters = tmb_int$par,
                                              DLL = "2023_09_11_subnational",
                                              silent=0,
                                              checkParameterOrder=FALSE)
    })
    # #
    if(is.null(parallel::mccollect(f)[[1]])) {
      stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
    }
    
    obj <-  TMB::MakeADFun(data = tmb_int$data,
                           parameters = tmb_int$par,
                           DLL = "2023_09_11_subnational",
                           random = tmb_int$random,
                           hessian = FALSE)
    
  }
  
  f <- stats::nlminb(obj$par, obj$fn, obj$gr)
  f$par.fixed <- f$par
  f$par.full <- obj$env$last.par
  
  fit <- c(f, obj = list(obj))
  fit$sdreport <- sdreport(fit$obj, fit$par)
  
  sd_report <- fit$sdreport
  sd_report <- summary(sd_report, "all")
  
  sd_report <- data.frame(sd_report, "hyper" = rownames(sd_report), iso = iso3)
  # sd_report <- data.frame(x= "foo")
  
  write_csv(sd_report, file.path("outside_orderly/fit/outputs", iso3_c, model_level, "sd_report.csv"))
  
  class(fit) <- "naomi_fit"  # this is hacky...
  fit <- naomi::sample_tmb(fit, random_only=TRUE)
  tmb_results <- dfertility::tmb_outputs(fit, mf, areas)
  
  write_csv(tmb_results, file.path("outside_orderly/fit/outputs", iso3_c, model_level, "fr.csv"))
  saveRDS(fit$sample, file.path("outside_orderly/fit/outputs/", iso3_c, model_level, "fixed.rds"))
  
  # fit <- naomi::sample_tmb(fit, random_only=FALSE)
  # hyper <- fit$sample %>%
  #   list_modify("lambda_out" = zap(), "tfr_out" = zap())
  # saveRDS(hyper, "hyper.rds")
  
  
  
  fr_plot <- fr_plot %>%
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
fit("MWI", "district")

possibly_fit <- possibly(fit, otherwise = NULL)
id <- map(moz.utils::ssa_iso3()[moz.utils::ssa_iso3() != "AGO"], ~possibly_fit(.x, model_level = "provincial"))
