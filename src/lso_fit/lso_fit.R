iso3 <- "LSO"

# population <- read.csv(paste0("depends/", tolower(iso3), "_population.csv"))
# areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
# asfr <- read.csv(paste0("depends/", tolower(iso3), "_dhs_asfr.csv"))

population <- read.csv("archive/lso_data_population/20201125-211633-4c92792c/lso_population_census16.csv")
areas <- read_sf("archive/lso_data_areas/20201125-210836-1ea366fa/lso_areas.geojson")
asfr <- read.csv("archive/lso_asfr/20201203-170637-8a2584b9/lso_asfr.csv")

my <- max(asfr$period)
asfr <- asfr %>%
  filter(period < my-4)

mf <- make_model_frames_dev(iso3, population, asfr,  areas, naomi_level =2, project=2020)


TMB::compile("global/tmb_all_level_poisson.cpp")               # Compile the C++ file
dyn.load(TMB::dynlib("global/tmb_all_level_poisson"))

mf$mf_model <- mf$mf_model %>%
  mutate(id.period = group_indices(., period)-1)

X_period <- as(as.matrix(mf$mf_model$id.period), "dgTMatrix")

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  # X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
  Z_period = mf$Z$Z_period,
  X_period = X_period,
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1),
  R_period_iid = as(diag(nrow = ncol(mf$Z$Z_period)), "dgTMatrix"),
  R_spatial = mf$R$R_spatial,
  R_country = mf$R$R_country,
  rankdef_R_spatial = 1,
  
  log_offset_naomi = log(mf$observations$naomi_level_obs$pys),
  births_obs_naomi = mf$observations$naomi_level_obs$births,
  
  log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
  births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
  
  log_offset_ais = log(filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$pys),
  births_obs_ais = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$births,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  
  A_full_obs = mf$observations$A_full_obs,
  
  mics_toggle = mf$mics_toggle,
  
  X_spike_2000_dhs = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_1999_dhs = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_2001_dhs = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "DHS")),
  
  X_spike_2000_ais = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_1999_ais = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS")))
  
  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  
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
  beta_period = rep(0, 1),
  lag_logit_phi_arima_period = 0,
  # lag_logit_phi_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "beta_period",
                    "beta_tips_dummy",
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

if(mf$mics_toggle) {
  tmb_int$data <- c(tmb_int$data,
                    "Z_tips_mics" = mf$Z$Z_tips_mics,
                    "R_tips_mics" = mf$R$R_tips_mics,
                    "log_offset_mics" = list(log(filter(mf$observations$full_obs, survtype == "MICS")$pys)),
                    "births_obs_mics" = list(filter(mf$observations$full_obs, survtype == "MICS")$births),
                    
                    "X_spike_2000_mics" = list(model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_1999_mics" = list(model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_2001_mics" = list(model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "MICS")))
  )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "tmb_all_level_poisson",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})

parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "tmb_all_level_poisson",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

tmb_results <- read_csv("tmb_results_validation.csv")

tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "Poisson Arima Trend")
  )

######### ARIMA NO TREND #######

TMB::compile("global/validation/arima.cpp")               # Compile the C++ file
dyn.load(TMB::dynlib("global/validation/arima"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  # X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
  Z_period = mf$Z$Z_period,
  X_period = X_period,
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1),
  R_period_iid = as(diag(nrow = ncol(mf$Z$Z_period)), "dgTMatrix"),
  R_spatial = mf$R$R_spatial,
  R_country = mf$R$R_country,
  rankdef_R_spatial = 1,
  
  log_offset_naomi = log(mf$observations$naomi_level_obs$pys),
  births_obs_naomi = mf$observations$naomi_level_obs$births,
  
  log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
  births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
  
  log_offset_ais = log(filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$pys),
  births_obs_ais = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$births,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  
  A_full_obs = mf$observations$A_full_obs,
  
  mics_toggle = mf$mics_toggle,
  
  X_spike_2000_dhs = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_1999_dhs = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_2001_dhs = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "DHS")),
  
  X_spike_2000_ais = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_1999_ais = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS")))
  
  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  
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
  # beta_period = rep(0, 1),
  lag_logit_phi_arima_period = 0,
  # lag_logit_phi_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "beta_period",
                    "beta_tips_dummy",
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

if(mf$mics_toggle) {
  tmb_int$data <- c(tmb_int$data,
                    "Z_tips_mics" = mf$Z$Z_tips_mics,
                    "R_tips_mics" = mf$R$R_tips_mics,
                    "log_offset_mics" = list(log(filter(mf$observations$full_obs, survtype == "MICS")$pys)),
                    "births_obs_mics" = list(filter(mf$observations$full_obs, survtype == "MICS")$births),
                    
                    "X_spike_2000_mics" = list(model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_1999_mics" = list(model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_2001_mics" = list(model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "MICS")))
  )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "arima",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})

parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "arima",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "Poisson Arima")
  )

######### RW2 #######

TMB::compile("global/validation/rw2.cpp")               # Compile the C++ file
dyn.load(TMB::dynlib("global/validation/rw2"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  # X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
  Z_period = mf$Z$Z_period,
  X_period = X_period,
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 2),
  R_period_iid = as(diag(nrow = ncol(mf$Z$Z_period)), "dgTMatrix"),
  R_spatial = mf$R$R_spatial,
  R_country = mf$R$R_country,
  rankdef_R_spatial = 1,
  
  log_offset_naomi = log(mf$observations$naomi_level_obs$pys),
  births_obs_naomi = mf$observations$naomi_level_obs$births,
  
  log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
  births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
  
  log_offset_ais = log(filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$pys),
  births_obs_ais = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$births,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  
  A_full_obs = mf$observations$A_full_obs,
  
  mics_toggle = mf$mics_toggle,
  
  X_spike_2000_dhs = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_1999_dhs = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_2001_dhs = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "DHS")),
  
  X_spike_2000_ais = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_1999_ais = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS")))
  
  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  
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
  # beta_period = rep(0, 1),
  # lag_logit_phi_arima_period = 0,
  # lag_logit_phi_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "beta_period",
                    "beta_tips_dummy",
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

if(mf$mics_toggle) {
  tmb_int$data <- c(tmb_int$data,
                    "Z_tips_mics" = mf$Z$Z_tips_mics,
                    "R_tips_mics" = mf$R$R_tips_mics,
                    "log_offset_mics" = list(log(filter(mf$observations$full_obs, survtype == "MICS")$pys)),
                    "births_obs_mics" = list(filter(mf$observations$full_obs, survtype == "MICS")$births),
                    
                    "X_spike_2000_mics" = list(model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_1999_mics" = list(model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_2001_mics" = list(model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "MICS")))
  )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "rw2",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})

parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "rw2",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "Poisson RW2")
  )

######### AR1 #######

TMB::compile("global/validation/ar1.cpp")               # Compile the C++ file
dyn.load(TMB::dynlib("global/validation/ar1"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  # X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
  Z_period = mf$Z$Z_period,
  X_period = X_period,
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 2),
  R_period_iid = as(diag(nrow = ncol(mf$Z$Z_period)), "dgTMatrix"),
  R_spatial = mf$R$R_spatial,
  R_country = mf$R$R_country,
  rankdef_R_spatial = 1,
  
  log_offset_naomi = log(mf$observations$naomi_level_obs$pys),
  births_obs_naomi = mf$observations$naomi_level_obs$births,
  
  log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
  births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
  
  log_offset_ais = log(filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$pys),
  births_obs_ais = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$births,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  
  A_full_obs = mf$observations$A_full_obs,
  
  mics_toggle = mf$mics_toggle,
  
  X_spike_2000_dhs = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_1999_dhs = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_2001_dhs = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "DHS")),
  
  X_spike_2000_ais = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_1999_ais = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS")))
  
  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  
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
  # beta_period = rep(0, 1),
  # lag_logit_phi_arima_period = 0,
  lag_logit_phi_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "beta_period",
                    "beta_tips_dummy",
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

if(mf$mics_toggle) {
  tmb_int$data <- c(tmb_int$data,
                    "Z_tips_mics" = mf$Z$Z_tips_mics,
                    "R_tips_mics" = mf$R$R_tips_mics,
                    "log_offset_mics" = list(log(filter(mf$observations$full_obs, survtype == "MICS")$pys)),
                    "births_obs_mics" = list(filter(mf$observations$full_obs, survtype == "MICS")$births),
                    
                    "X_spike_2000_mics" = list(model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_1999_mics" = list(model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_2001_mics" = list(model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "MICS")))
  )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "ar1",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})

parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "ar1",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "Poisson AR1")
  )

######### AR1 + Trend #######

TMB::compile("global/validation/ar1_trend.cpp")               # Compile the C++ file
dyn.load(TMB::dynlib("global/validation/ar1_trend"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  # X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
  Z_period = mf$Z$Z_period,
  X_period = X_period,
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 2),
  R_period_iid = as(diag(nrow = ncol(mf$Z$Z_period)), "dgTMatrix"),
  R_spatial = mf$R$R_spatial,
  R_country = mf$R$R_country,
  rankdef_R_spatial = 1,
  
  log_offset_naomi = log(mf$observations$naomi_level_obs$pys),
  births_obs_naomi = mf$observations$naomi_level_obs$births,
  
  log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
  births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
  
  log_offset_ais = log(filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$pys),
  births_obs_ais = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$births,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  
  A_full_obs = mf$observations$A_full_obs,
  
  mics_toggle = mf$mics_toggle,
  
  X_spike_2000_dhs = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_1999_dhs = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "DHS")),
  X_spike_2001_dhs = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "DHS")),
  
  X_spike_2000_ais = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_1999_ais = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS")))
  
  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  
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
  beta_period = rep(0, 1),
  # lag_logit_phi_arima_period = 0,
  lag_logit_phi_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "beta_period",
                    "beta_tips_dummy",
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

if(mf$mics_toggle) {
  tmb_int$data <- c(tmb_int$data,
                    "Z_tips_mics" = mf$Z$Z_tips_mics,
                    "R_tips_mics" = mf$R$R_tips_mics,
                    "log_offset_mics" = list(log(filter(mf$observations$full_obs, survtype == "MICS")$pys)),
                    "births_obs_mics" = list(filter(mf$observations$full_obs, survtype == "MICS")$births),
                    
                    "X_spike_2000_mics" = list(model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_1999_mics" = list(model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    "X_spike_2001_mics" = list(model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "MICS")))
  )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          DLL = "ar1_trend",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})

parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "ar1_trend",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "Poisson AR1 trend")
  )

tmb_results %>%
  filter(area_level == 1, variable == "tfr", str_detect(area_id, "RWA")) %>%
  ggplot(aes(x=period, y=median)) +
  geom_line(aes(color=source)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=0.3) +
  # geom_point(data = fr_plot, aes(y=value)) +
  facet_wrap(~area_name, ncol=5) +
  labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size=14)
  )


fr_plot <- read.csv("archive/lso_asfr/20201203-170637-8a2584b9/lso_fr_plot.csv") %>%
  filter(variable == "tfr") %>%
  select(area_id, period, value)

# mse <- tmb_results %>%
#   filter(variable == "tfr") %>%
#   left_join(fr_plot) %>%
#   select(area_id:area_level, value) %>%
#   filter(!is.na(value), period %in% (my-4):my) %>%
#   mutate(sq_err = (value - median)^2) %>%
#   group_by(source) %>%
#   summarise(mse = mean(sq_err)) %>%
#   ungroup %>%
#   mutate(iso3 = iso3)

mse <- read_csv("mse.csv")

mse <- mse %>%
  bind_rows(
    tmb_results %>%
      filter(variable == "tfr") %>%
      left_join(fr_plot) %>%
      select(area_id:area_level, value) %>%
      filter(!is.na(value), period %in% (my-4):my) %>%
      mutate(sq_err = (value - median)^2) %>%
      group_by(source) %>%
      summarise(mse = mean(sq_err)) %>%
      ungroup %>%
      mutate(iso3 = iso3)
  )

write_csv(mse, "mse.csv")
write_csv(tmb_results, "tmb_results_validation.csv")

