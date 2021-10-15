iso3_c <- iso3

population <- read.csv("depends/interpolated_population.csv") %>%
  rename(period = year) %>%
  mutate(iso3 = iso3) %>%
  filter(sex == "female")

areas <- read_sf("depends/naomi_areas.geojson") %>%
  mutate(iso3 = iso3)

asfr <- read.csv("depends/fertility_asfr.csv") %>%
  filter(survtype != "MICS")

mics_asfr <- read.csv("resources/mics_asfr.csv") %>%
  filter(iso3 == iso3_c)

asfr <- asfr %>% bind_rows(mics_asfr)

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

mf <- make_model_frames_dev(iso3, population, asfr,  areas, naomi_level = lvl, project=2020)

spline_mat <- splines::bs(1:26, knots = seq(1, 26, 3))
class(spline_mat) <- "matrix"
spline_mat <- as(spline_mat, "sparseMatrix")

mf$Z$Z_period <- mf$Z$Z_period %*% spline_mat

validate_model_frame(mf, areas)

# TMB::compile("src/aaa_fit/tmb_all_level_poisson.cpp", flags = "-w")               # Compile the C++ file
TMB::compile("rw.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("rw"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_period = mf$Z$X_period,
  X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
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
  # Z_smooth_iid = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs),
  # Z_smooth_iid_ais = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  # R_smooth_iid = R_smooth_iid,
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  # R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
  R_period = make_rw_structure_matrix(ncol(spline_mat), 1, adjust_diagonal = TRUE),
  # R_spline_mat = spline_mat,
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
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),

  n_threads = parallel::detectCores()

  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,

  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
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

  # u_period = rep(0, ncol(mf$Z$Z_period)),
  u_period = rep(0, ncol(spline_mat)),
  log_prec_rw_period = 0,
  # logit_phi_period = 0,
  # lag_logit_phi_period = 0,
  # lag_logit_phi_arima_period = 0,
  # beta_period = 0,

  # log_prec_smooth_iid = 0,
  # u_smooth_iid = rep(0, ncol(R_smooth_iid)),

  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,

  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,

  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  logit_eta1_phi_age = 0,
  logit_eta1_phi_period = 0,

  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0,
  # #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "u_smooth_iid",
                    # "beta_period",
                    "beta_tips_dummy",
                    # "beta_urban_dummy",
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

                    # "Z_smooth_iid_mics" = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype == "MICS"))
  )
  # tmb_int$par <- c(tmb_int$par,
  #                  "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  # )
  # tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                               parameters = tmb_int$par,
                               DLL = "rw",
                               silent=0,
                               checkParameterOrder=FALSE)
})

if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "rw",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- dfertility::tmb_outputs(fit, mf, areas) %>%
  mutate(source = "rw")

################ RW2

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_period = mf$Z$X_period,
  X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
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
  # Z_smooth_iid = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs),
  # Z_smooth_iid_ais = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  # R_smooth_iid = R_smooth_iid,
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  # R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
  R_period = make_rw_structure_matrix(ncol(spline_mat), 2, adjust_diagonal = TRUE),
  # R_spline_mat = spline_mat,
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
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  
  n_threads = parallel::detectCores()
  
  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
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
                    
                    # "Z_smooth_iid_mics" = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype == "MICS"))
  )
  # tmb_int$par <- c(tmb_int$par,
  #                  "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  # )
  # tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "u_smooth_iid",
                    # "beta_period",
                    "beta_tips_dummy",
                    # "beta_urban_dummy",
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

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "rw",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "rw2")
  )

######### RW1 + trend

TMB::compile("rw_trend.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("rw_trend"))

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_period = mf$Z$X_period,
  X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
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
  # Z_smooth_iid = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs),
  # Z_smooth_iid_ais = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  # R_smooth_iid = R_smooth_iid,
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  # R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
  R_period = make_rw_structure_matrix(ncol(spline_mat), 1, adjust_diagonal = TRUE),
  # R_spline_mat = spline_mat,
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
  X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  
  n_threads = parallel::detectCores()
  
  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
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
                    
                    # "Z_smooth_iid_mics" = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype == "MICS"))
  )
  # tmb_int$par <- c(tmb_int$par,
  #                  "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  # )
  # tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
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
  
  # u_period = rep(0, ncol(mf$Z$Z_period)),
  u_period = rep(0, ncol(spline_mat)),
  log_prec_rw_period = 0,
  # logit_phi_period = 0,
  # lag_logit_phi_period = 0,
  # lag_logit_phi_arima_period = 0,
  beta_period = 0,
  
  # log_prec_smooth_iid = 0,
  # u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  logit_eta1_phi_age = 0,
  logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0,
  # #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "u_smooth_iid",
                    "beta_period",
                    "beta_tips_dummy",
                    # "beta_urban_dummy",
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

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "rw_trend",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "rw + trend")
  )

########### ARIMA (1,1,0)

TMB::compile("arima.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("arima"))

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
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
  
  # u_period = rep(0, ncol(mf$Z$Z_period)),
  u_period = rep(0, ncol(spline_mat)),
  log_prec_rw_period = 0,
  # logit_phi_period = 0,
  # lag_logit_phi_period = 0,
  lag_logit_phi_arima_period = 0,
  # beta_period = 0,
  
  # log_prec_smooth_iid = 0,
  # u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  logit_eta1_phi_age = 0,
  logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0,
  # #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "u_smooth_iid",
                    # "beta_period",
                    "beta_tips_dummy",
                    # "beta_urban_dummy",
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


obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "arima",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "ARIMA(1,1,0)")
  )

########### ARIMA with trend

TMB::compile("arima_trend.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("arima_trend"))

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
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
  
  # u_period = rep(0, ncol(mf$Z$Z_period)),
  u_period = rep(0, ncol(spline_mat)),
  log_prec_rw_period = 0,
  # logit_phi_period = 0,
  # lag_logit_phi_period = 0,
  lag_logit_phi_arima_period = 0,
  beta_period = 0,
  
  # log_prec_smooth_iid = 0,
  # u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  logit_eta1_phi_age = 0,
  logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0,
  # #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "u_smooth_iid",
                    "beta_period",
                    "beta_tips_dummy",
                    # "beta_urban_dummy",
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

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "arima_trend",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "ARIMA(1,1,0) + trend")
  )

########### AR1

TMB::compile("ar1.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("ar1"))


tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
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
  
  # u_period = rep(0, ncol(mf$Z$Z_period)),
  u_period = rep(0, ncol(spline_mat)),
  log_prec_rw_period = 0,
  # logit_phi_period = 0,
  lag_logit_phi_period = 0,
  # lag_logit_phi_arima_period = 0,
  # beta_period = 0,
  
  # log_prec_smooth_iid = 0,
  # u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  logit_eta1_phi_age = 0,
  logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0,
  # #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "u_smooth_iid",
                    # "beta_period",
                    "beta_tips_dummy",
                    # "beta_urban_dummy",
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


obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "ar1",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "AR1")
  )

########## AR1 + trend

TMB::compile("ar1_trend.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("ar1_trend"))


tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
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
  
  # u_period = rep(0, ncol(mf$Z$Z_period)),
  u_period = rep(0, ncol(spline_mat)),
  log_prec_rw_period = 0,
  # logit_phi_period = 0,
  lag_logit_phi_period = 0,
  # lag_logit_phi_arima_period = 0,
  beta_period = 0,
  
  # log_prec_smooth_iid = 0,
  # u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  logit_eta1_phi_age = 0,
  logit_eta1_phi_period = 0,
  
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0,
  # #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "u_smooth_iid",
                    "beta_period",
                    "beta_tips_dummy",
                    # "beta_urban_dummy",
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


obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "ar1_trend",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- tmb_results %>%
  bind_rows(
    dfertility::tmb_outputs(fit, mf, areas) %>%
      mutate(source = "AR1 + trend")
  )

write_csv(tmb_results, "fr.csv")