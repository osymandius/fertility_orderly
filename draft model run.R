iso3_c <- "ZWE"

population <- read.csv(paste0("depends/", tolower(iso3_c), "_population.csv"))
areas <- read_sf(paste0("depends/", tolower(iso3_c), "_areas.geojson"))
asfr <- read.csv(paste0("depends/", tolower(iso3_c), "_asfr.csv"))
mics_asfr <- paste0("depends/", tolower(iso3_c), "_mics_asfr.csv")

population <- read.csv("archive/zwe_data_population/20201103-131932-08512a09/zwe_population_nso.csv")
areas <- read_sf("archive/zwe_data_areas/20201103-094144-94388ee2/zwe_areas.geojson")
asfr <- readRDS("~/Documents/GitHub/subnat_fertility/countries/ZWE/data/ZWE_asfr_admin2.rds")
mics_asfr <- readRDS("~/Documents/GitHub/subnat_fertility/countries/ZWE/data/ZWE_mics_admin1.rds")

population <- population %>%
  mutate(period = as.numeric(year_labels(calendar_quarter_to_quarter_id(calendar_quarter)))) %>%
  select(-calendar_quarter)

mf <- make_model_frames(iso3_c, population, asfr, mics_asfr, areas, project=2020)

compile("resources/tmb.cpp")               # Compile the C++ file
dyn.load(dynlib(here("resources/tmb.cpp")))

compile("global/tmb.cpp")               # Compile the C++ file
dyn.load(dynlib("global/tmb"))

tmb_int <- list()

tmb_int$data <- list(M_obs = mf$district$M_obs,
                     X_tips_dummy = mf$Z$X_tips_dummy,
                     X_urban_dummy = mf$Z$X_urban_dummy,
                     X_extract_dhs = mf$X_extract$X_extract_dhs,
                     X_extract_ais = mf$X_extract$X_extract_ais,
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
                     log_offset_dhs = log(filter(mf$district$obs, ais_dummy ==0)$pys),
                     births_obs_dhs = filter(mf$district$obs, ais_dummy ==0)$births,
                     log_offset_ais = log(filter(mf$district$obs, ais_dummy ==1)$pys),
                     births_obs_ais = filter(mf$district$obs, ais_dummy ==1)$births,
                     pop = mf$mf_model$population,
                     A_asfr_out = mf$out$A_asfr_out,
                     A_tfr_out = mf$out$A_tfr_out,
                     mics_toggle = mf$mics_toggle,
                     out_toggle = mf$out_toggle
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  
  u_country = rep(0, ncol(mf$Z$Z_country)),
  log_prec_country = 0,
  
  omega1 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_age))),
  log_prec_omega1 = 0,
  lag_logit_omega1_phi_age = 0,
  
  omega2 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_period))),
  log_prec_omega2 = 0,
  lag_logit_omega2_phi_period = 0,
  
  u_period = rep(0, ncol(mf$Z$Z_period)),
  log_prec_rw_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  #
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0", "u_spatial_str", "u_age", "u_period", "beta_tips_dummy", "u_tips", "eta1", "eta2", "eta3", "omega1", "omega2")

if(mf$mics_toggle) {
  tmb_int$data <- c(tmb_int$data, 
                    "M_obs_mics" = mf$mics$M_obs_mics,
                    "Z_tips_mics" = mf$Z$Z_tips_mics,
                    "R_tips_mics" = mf$R$R_tips_mics,
                    "births_obs_mics" = list(mf$mics$obs$births),
                    "log_offset_mics" = list(log(mf$mics$obs$pys)),
                    "A_mics" = mf$mics$A_mics)
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
}

if(iso3_c == "ETH") {
  tmb_int$par <- c(tmb_int$par,
                   "beta_urban_dummy" = rep(0, ncol(mf$Z$X_urban_dummy))
  )
}

f <- mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                parameters = tmb_int$par,
                                DLL = "tmb",
                                silent=0,
                                checkParameterOrder=FALSE)
})

mccollect(f)
