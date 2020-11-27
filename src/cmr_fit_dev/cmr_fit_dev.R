iso3 <- "CMR"

population <- read.csv(paste0("depends/", tolower(iso3), "_population.csv"))
areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
asfr <- read.csv(paste0("depends/", tolower(iso3), "_asfr.csv"))

asfr <- read_csv("archive/cmr_asfr/20201124-140350-53a0b873/cmr_asfr.csv")
population <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Analytical datasets/naomi-data/CMR/data/cmr_population_ins.csv")
areas <- read_sf("archive/cmr_data_areas/20201110-083355-e19453c7/cmr_areas.geojson")

areas_long <- areas %>%
  st_drop_geometry() %>%
  mutate(iso3 = iso3)

debugonce(make_model_frames_dev)
mf <- make_model_frames_dev(iso3, population, asfr, areas, naomi_level = 3, project=2020)

compile("global/tmb.cpp")               # Compile the C++ file
dyn.load(dynlib("global/tmb"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_age = mf$Z$Z_age,
  Z_period = mf$Z$Z_period,
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = mf$R$R_period,
  R_spatial = mf$R$R_spatial,
  R_country = mf$R$R_country,
  rankdef_R_spatial = 1,
  log_offset_dhs_naomi = log(filter(mf$observations$naomi_level_obs, ais_dummy ==0, mics_dummy == 0)$pys),
  births_obs_dhs_naomi = filter(mf$observations$naomi_level_obs, ais_dummy ==0, mics_dummy==0)$births,
  log_offset_ais_naomi = log(filter(mf$observations$naomi_level_obs, ais_dummy ==1)$pys),
  births_obs_ais_naomi = filter(mf$observations$naomi_level_obs, ais_dummy ==1)$births,
  pop = mf$mf_model$population,
  A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  mics_toggle = mf$mics_toggle,
  out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips)),
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
lag_logit_phi_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0
  
  # eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  # log_prec_eta1 = 0,
  # lag_logit_eta1_phi_age = 0,
  # lag_logit_eta1_phi_period = 0,
  # #
  # eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  # log_prec_eta2 = 0,
  # lag_logit_eta2_phi_period = 0,
  # #
  # eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  # log_prec_eta3 = 0,
  # lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0", "u_spatial_str", "u_age", "u_period", "beta_tips_dummy", "u_tips")

# if(mf$mics_toggle) {
#   tmb_int$data <- c(tmb_int$data, 
#                     "Z_tips_mics" = mf$Z$Z_tips_mics,
#                     "R_tips_mics" = mf$R$R_tips_mics,
#                     "log_offset_mics" = log(filter(mf$observations$obs, mics_dummy == 1)$pys),
#                     "births_obs_mics" = filter(mf$observations$obs, mics_dummy==1)$births,
#                     "A_mics" = mf$mics$A_mics,
                    "X_spike_2000_mics" = list(model.matrix(~0 + spike_2000, mf$mics$obs)),
                    "X_spike_1999_mics" = list(model.matrix(~0 + spike_1999, mf$mics$obs)),
                    "X_spike_2001_mics" = list(model.matrix(~0 + spike_2001, mf$mics$obs))
                    )
#   tmb_int$par <- c(tmb_int$par,
#                    "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
#   )
# }

f <- mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                parameters = tmb_int$par,
                                DLL = "tmb",
                                silent=0,
                                checkParameterOrder=FALSE)
})

mccollect(f)

obj <-  MakeADFun(data = tmb_int$data,
                  parameters = tmb_int$par,
                  DLL = "tmb",
                  random = tmb_int$random,
                  hessian = FALSE)

f <- nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- sample_tmb(fit, random_only=FALSE)

res <- tmb_outputs(fit, mf)

res %>%
  filter(variable == "tfr", area_level == 1) %>%
  ggplot(aes(x=period,y=median)) +
    geom_line() +
    facet_wrap(~area_name)
