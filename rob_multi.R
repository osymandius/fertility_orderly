library(tidyverse)
library(naomi)
library(sf)
library(dfertility)
library(Matrix)
library(TMB)
library(orderly)
library(INLA)

iso <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COG", "GMB", "KEN", "LSO", "MLI", "MWI", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

latest_orderly_pulls <- lapply(iso, function(x){
  orderly_search(paste0('latest(parameter:iso3 == "', x, '")'), name = "aaa_inputs_orderly_pull")
})

latest_population_pulls <- lapply(iso, function(x){
  orderly_search(paste0('latest(parameter:iso3 == "', x, '")'), name = "aaa_scale_pop")
})

population <- lapply(latest_population_pulls, function(x) {
  read.csv(paste0("archive/aaa_scale_pop/", x, "/interpolated_population.csv")) %>%
    rename(period = year) %>%
    filter(sex == "female")
})

areas <- lapply(latest_orderly_pulls, function(x) {
  read_sf(paste0("archive/aaa_inputs_orderly_pull/", x, "/naomi_areas.geojson"))
})

asfr <- lapply(latest_orderly_pulls, function(x) {
  read.csv(paste0("archive/aaa_inputs_orderly_pull/", x, "/fertility_asfr.csv"))
})

names(population) <- names(areas) <- names(asfr) <- iso

population <- bind_rows(population, .id="iso3")

asfr <- bind_rows(asfr) %>%
  arrange(iso3)

asfr <- asfr %>%
  mutate(area_id = ifelse(area_id == "COG_3_12fr", "COG_2_02fq", area_id))

lvl_map <- read.csv("global/iso_mapping_fit.csv") %>%
  filter(iso3 %in% iso) %>%
  arrange(iso3)

areas <- areas[order(names(areas))]

# debugonce(make_model_frames_batch)
mf <- make_model_frames_batch(lvl_map, population, asfr, areas_list = areas, project=2020)

validate_model_frame(mf, areas %>% bind_rows())

# TMB::compile("src/aaa_fit/tmb_all_level_poisson.cpp", flags = "-w")               # Compile the C++ file
TMB::compile("multi_country.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("multi_country"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_level_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_period = mf$Z$X_period,
  # X_urban_dummy = mf$Z$X_urban_dummy,
  X_extract_dhs = mf$X_extract$X_extract_dhs,
  X_extract_ais = mf$X_extract$X_extract_ais,
  X_extract_mics = mf$X_extract$X_extract_mics,
  # Z_tips = mf$Z$Z_tips,
  Z_tips_dhs = mf$Z$Z_tips_dhs,
  Z_tips_ais = mf$Z$Z_tips_ais,
  Z_age = mf$Z$Z_age,
  Z_period = mf$Z$Z_period,
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  # Z_smooth_iid = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs),
  # Z_smooth_iid_ais = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  # R_smooth_iid = R_smooth_iid,
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
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
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,

  u_country = rep(0, ncol(mf$Z$Z_country)),
  log_prec_country = 0,

  omega1 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_age))),
  log_prec_omega1 = 0,
  logit_omega1_phi_age = 0,
  #
  omega2 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_period))),
  log_prec_omega2 = 0,
  logit_omega2_phi_period = 0,
  
  u_period = rep(0, ncol(mf$Z$Z_period)),
  log_prec_rw_period = 0,
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
  
  # eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  logit_eta1_phi_age = 0,
  logit_eta1_phi_period = 0,
  # # #
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  logit_eta2_phi_period = 0,
  # # #
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
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3",
                    "omega1",
                    "omega2"
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
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                               parameters = tmb_int$par,
                               DLL = "multi_country",
                               silent=0,
                               checkParameterOrder=FALSE)
})

if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "multi_country",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
fit$sdreport <- sdreport(fit$obj, fit$par)

sd_report <- fit$sdreport
sd_report <- summary(sd_report, "all")
sd_report <- data.frame(sd_report, "hyper" = rownames(sd_report), iso = iso3)

write.csv(sd_report, "sd_report.csv")

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

tmb_results <- dfertility::tmb_outputs(fit, mf, areas)
#
write.csv(tmb_results, "fr.csv")

fit <- naomi::sample_tmb(fit, random_only=FALSE)

hyper <- fit$sample %>%
  list_modify("lambda_out" = zap(),
              "tfr_out" = zap())
 
saveRDS(hyper, "hyper.rds")