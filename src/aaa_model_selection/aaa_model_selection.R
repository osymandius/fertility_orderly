iso3_c <- iso3

population <- read.csv("depends/interpolated_population.csv") %>%
  rename(period = year) %>%
  mutate(iso3 = iso3) %>%
  filter(sex == "female")

areas <- read_sf("depends/naomi_areas.geojson") %>%
  mutate(iso3 = iso3) %>%
  st_make_valid() %>%
  st_collection_extract("POLYGON")

asfr <- read.csv("depends/asfr.csv")

phia_asfr <- read.csv("resources/phia_asfr.csv") %>%
  separate(area_id, into=c("iso3", NA), sep = 3, remove = FALSE) %>%
  filter(iso3 == iso3_c) %>%
  mutate(survtype = "PHIA")

remove_survey <- c("CIV2005AIS", "CIV2006MICS", 
                   "GMB2005MICS",
                   # "MLI2009MICS", "MLI2015MICS", 
                   "SLE2010MICS", 
                   "TGO2006MICS", 
                   "BEN1996DHS", 
                   "KEN2009MICS", 
                   "COD2017MICS", 
                   "NGA2007MICS",
                   "TZA2007AIS", "TZA2012AIS")
subnational_surveys <- c("KEN2009MICS", "KEN2011MICS")

asfr <- asfr %>% 
  bind_rows(phia_asfr) %>%
  filter(!survey_id %in% remove_survey,
         !(iso3 == "SWZ" & period == 2017),
         !(iso3 == "SWZ" & period == 1995 & survey_id == "SWZ2000MICS"),
         !(iso3 == "SWZ" & period == 1999 & survey_id == "SWZ2014MICS"),
         !(iso3 == "GMB" & period == 2004 & survey_id == "GMB2019DHS"),
         !(iso3 == "GMB" & period == 2005 & survey_id == "GMB2010MICS"),
         !(iso3 == "GMB" & period == 2013 & survey_id == "GMB2018MICS"),
         !(survey_id == "AGO2011MIS" & tips > 5)
  )

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

mf <- make_model_frames_dev(iso3, population, asfr,  areas, naomi_level = lvl, project=2020)

# mf$observations$full_obs <- mf$observations$full_obs %>%
#   ungroup() %>%
#   mutate(id.smooth = factor(row_number()))

mf$observations$full_obs <- mf$observations$full_obs %>%
  mutate(tips_dummy_5 = as.integer(tips %in% 5),
         tips_dummy_6 = as.integer(tips %in% 6),
         tips_dummy_7 = as.integer(tips %in% 7),
         tips_dummy_8 = as.integer(tips %in% 8),
         tips_dummy_0 = as.integer(tips %in% 0),
         tips_fe = factor(case_when(
           tips == 0 ~ 1,
           tips == 6 & survtype == "DHS" ~ 2,
           tips == 10 ~ 3,
           TRUE ~ 0
         ))
  ) %>%
  ungroup() %>%
  group_by(tips_fe, survey_id) %>%
  mutate(id.zeta2 = factor(cur_group_id()),
         id.zeta2 = forcats::fct_expand(id.zeta2, as.character(1:(length(unique(mf$observations$full_obs$survey_id))*4))))

clear_col <- as.integer(unique(filter(mf$observations$full_obs, tips_fe == 0)$id.zeta2))
mf$Z$Z_zeta2 <- sparse.model.matrix(~0 + id.zeta2, mf$observations$full_obs)
mf$Z$Z_zeta2[,clear_col] <- 0

mf$Z$X_tips_fe <- sparse.model.matrix(~0 + tips_fe, mf$observations$full_obs)
mf$Z$X_tips_fe[,1] <- 0

mf$observations$full_obs <- mf$observations$full_obs %>%
  ungroup() %>%
  group_by(tips, survey_id) %>%
  mutate(id.zeta1 = factor(cur_group_id()),
         id.zeta1 = fct_expand(id.zeta1, as.character(1:(length(unique(mf$observations$full_obs$survey_id))*15)))) %>%
  ungroup()

mf$Z$X_tips_dummy_5 <- model.matrix(~0 + tips_dummy_5, mf$observations$full_obs %>% filter(survtype == "DHS"))

mf$observations$full_obs <- mf$observations$full_obs %>%
  separate(survey_id, into=c(NA, "survyear", "survtype"), sep=c(3,7), remove=FALSE, convert=TRUE) %>%
  ungroup() %>%
  mutate(id.smooth = factor(row_number())) %>%
  group_by(survtype) %>%
  mutate(idx = row_number()-1)

R_smooth_iid <- sparseMatrix(i = 1:nrow(mf$observations$full_obs), j = 1:nrow(mf$observations$full_obs), x = 1)

x <- 0:25
k <- seq(-15, 40, by = 5)
spline_mat <- splines::splineDesign(k, x, ord = 4)
spline_mat <- as(spline_mat, "sparseMatrix")

mf$Z$Z_period <- mf$Z$Z_period %*% spline_mat

validate_model_frame(mf, areas)

end_year <- max(unique(mf$observations$full_obs$survyear))-5

# end_year <- 2016

# TMB::compile("~/Documents/GitHub/dfertility/backup_src/rw.cpp", flags = "-w")               # Compile the C++ file
# TMB::compile("rw.cpp", flags = "-w")               # Compile the C++ file
# dyn.load(dynlib("~/Documents/GitHub/dfertility/backup_src/rw"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_tips_dummy_5 = mf$Z$X_tips_dummy_5,
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
  R_smooth_iid = R_smooth_iid,
  R_tips = mf$R$R_tips,
  R_tips_iid = as(diag(1, ncol(mf$Z$Z_tips_dhs)), "dgTMatrix"),
  # Z_zeta1 = sparse.model.matrix(~0 + id.zeta1, mf$observations$full_obs),
  Z_zeta2 = mf$Z$Z_zeta2,
  R_zeta2 = as(diag(1, ncol(mf$Z$X_tips_fe)), "dgTMatrix"),
  R_survey = as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix"),
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
  
  log_offset_phia = log(filter(mf$observations$full_obs, survtype == "PHIA")$pys),
  births_obs_phia = filter(mf$observations$full_obs, survtype == "PHIA")$births,
  
  include_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear < end_year)$idx,
  include_ais_obs = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"), survyear < end_year)$idx,
  include_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear < end_year)$idx,
  include_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear < end_year)$idx,
  
  exclude_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear >= end_year)$idx,
  exclude_ais_obs = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"), survyear >= end_year)$idx,
  exclude_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear >= end_year)$idx,
  exclude_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear >= end_year)$idx,

  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,

  A_full_obs = mf$observations$A_full_obs,

  mics_toggle = mf$mics_toggle,

  X_spike_2000 = model.matrix(~0 + spike_2000, mf$observations$full_obs),
  X_spike_1999 = model.matrix(~0 + spike_1999, mf$observations$full_obs),
  X_spike_2001 = model.matrix(~0 + spike_2001, mf$observations$full_obs)

)

tmb_int$par <- list(
  beta_0 = 0,

  # beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  beta_tips_dummy_5 = rep(0, ncol(mf$Z$X_tips_dummy_5)),
  beta_tips_fe = rep(0, ncol(mf$Z$X_tips_fe)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  lag_logit_phi_tips = 0,

  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  lag_logit_phi_age = 0,

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

  log_prec_smooth_iid = 0,
  u_smooth_iid = rep(0, ncol(R_smooth_iid)),

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
  logit_eta3_phi_age = 0,
  
  zeta2 = array(0, c(ncol(as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix")),
                     ncol(mf$Z$X_tips_fe)
                     )
                ),
  log_prec_zeta2 = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_smooth_iid",
                    # "beta_period",
                    # "beta_tips_dummy",
                    "beta_tips_dummy_5",
                    "beta_tips_fe",
                    # "beta_urban_dummy",
                    "u_tips",
                    "zeta2",
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

# dyn.load(dynlib("rw"))

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

quant_pos_sum <- function(births, x) {
  if(births < x)
    0
  else
    1
}

dhs_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype == "DHS", survyear >= end_year)
  

if(nrow(dhs_ppd)) {
  dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
  dhs_ppd <- dhs_ppd %>% 
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
  bind_cols(data.frame(dhsMatrix)) 
} else {
  dhs_ppd <- data.frame()
}

ais_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype %in% c("AIS", "MIS"), survyear >= end_year)
  

if(nrow(ais_ppd)) {
  aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
  ais_ppd <- ais_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(aisMatrix))
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
  bind_cols(data.frame(phiaMatrix))
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
  bind_cols(data.frame(micsMatrix))
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


rw_ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd)

true_values <- rw_ppd %>%
  select(births, pys) %>%
  mutate(observed_asfr = births/pys) %>%
  .$observed_asfr

predictions <- rw_ppd %>%
  select(X1:X1000) %>%
  as.matrix()

rw_crps = sum(scoringutils::crps(true_values, predictions))

est_births <- as.matrix(rw_ppd[,8:1007]) * rw_ppd$pys
ll_pred <- dxpois(rw_ppd$births, est_births, TRUE)
rw_elpd <- loo::elpd(t(ll_pred))
rw_elpd <- rw_elpd$estimates %>%
  as.data.frame() %>%
  mutate(source = "rw")

rw_ppd <- rw_ppd %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("X"), ~rpois(1, pys*.x))) %>%
  aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                   c(paste0("X", 1:1000), "births", "pys"),
                   1,
                   areas) %>%
  group_by(survey_id, period, area_id,tips) %>%
  summarise(
            survey_tfr = 5*sum(births/pys),
            across(starts_with("X"), ~5*sum(.x/pys)),
            observed_births = sum(births),
            pys = sum(pys)
  ) %>%
  ungroup() %>%
  select(survey_id:survey_tfr, observed_births, pys, everything())

qtls <- apply(select(rw_ppd, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))

rw_ppd <- rw_ppd %>%
  group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
  rowwise() %>%
  summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
  ungroup %>%
  mutate(lower = qtls[1,],
         median = qtls[2,],
         upper = qtls[3,],
         source = "rw")


################ RW2

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_tips_dummy_5 = mf$Z$X_tips_dummy_5,
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
  R_smooth_iid = R_smooth_iid,
  R_tips = mf$R$R_tips,
  R_tips_iid = as(diag(1, ncol(mf$Z$Z_tips_dhs)), "dgTMatrix"),
  # Z_zeta1 = sparse.model.matrix(~0 + id.zeta1, mf$observations$full_obs),
  Z_zeta2 = mf$Z$Z_zeta2,
  R_zeta2 = as(diag(1, ncol(mf$Z$X_tips_fe)), "dgTMatrix"),
  R_survey = as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix"),
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
  
  log_offset_phia = log(filter(mf$observations$full_obs, survtype == "PHIA")$pys),
  births_obs_phia = filter(mf$observations$full_obs, survtype == "PHIA")$births,
  
  include_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear < end_year)$idx,
  include_ais_obs = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"), survyear < end_year)$idx,
  include_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear < end_year)$idx,
  include_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear < end_year)$idx,
  
  exclude_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear >= end_year)$idx,
  exclude_ais_obs = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"), survyear >= end_year)$idx,
  exclude_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear >= end_year)$idx,
  exclude_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear >= end_year)$idx,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  
  A_full_obs = mf$observations$A_full_obs,
  
  mics_toggle = mf$mics_toggle,
  
  X_spike_2000 = model.matrix(~0 + spike_2000, mf$observations$full_obs),
  X_spike_1999 = model.matrix(~0 + spike_1999, mf$observations$full_obs),
  X_spike_2001 = model.matrix(~0 + spike_2001, mf$observations$full_obs)
  
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
                    "u_smooth_iid",
                    # "beta_period",
                    "beta_tips_dummy_5",
                    "beta_tips_fe",
                    # "beta_urban_dummy",
                    "u_tips",
                    "zeta2",
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

dhs_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype == "DHS", survyear >= end_year)
  

if(nrow(dhs_ppd)) {
  dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
  dhs_ppd <- dhs_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(dhsMatrix))
} else {
  dhs_ppd <- data.frame()
}

ais_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype %in% c("AIS", "MIS"), survyear >= end_year)
  

if(nrow(ais_ppd)) {
  aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
  ais_ppd <- ais_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(aisMatrix))
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
  bind_cols(data.frame(phiaMatrix))
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
  bind_cols(data.frame(micsMatrix))
} else {
  mics_ppd <- data.frame()
}

rw2_ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd)

true_values <- rw2_ppd %>%
  select(births, pys) %>%
  mutate(observed_asfr = births/pys) %>%
  .$observed_asfr

predictions <- rw2_ppd %>%
  select(X1:X1000) %>%
  as.matrix()

rw2_crps = sum(scoringutils::crps(true_values, predictions))

est_births <- as.matrix(rw2_ppd[,8:1007]) * rw2_ppd$pys
ll_pred <- dxpois(rw2_ppd$births, est_births, TRUE)
rw2_elpd <- loo::elpd(t(ll_pred))
rw2_elpd <- rw2_elpd$estimates %>%
  as.data.frame() %>%
  mutate(source = "rw2")

rw2_ppd <- rw2_ppd %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("X"), ~rpois(1, pys*.x))) %>%
  aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                   c(paste0("X", 1:1000), "births", "pys"),
                   1,
                   areas) %>%
  group_by(survey_id, period, area_id,tips) %>%
  summarise(
    survey_tfr = 5*sum(births/pys),
    across(starts_with("X"), ~5*sum(.x/pys)),
    observed_births = sum(births),
    pys = sum(pys)
  ) %>%
  ungroup() %>%
  select(survey_id:survey_tfr, observed_births, pys, everything())

qtls <- apply(select(rw2_ppd, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))

rw2_ppd <- rw2_ppd %>%
  group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
  rowwise() %>%
  summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
  ungroup %>%
  mutate(lower = qtls[1,],
         median = qtls[2,],
         upper = qtls[3,],
         source = "rw2")

######### RW1 + trend

# TMB::compile("rw1_trend.cpp", flags = "-w")               # Compile the C++ file
# dyn.load(dynlib("rw1_trend"))

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_tips_dummy_5 = mf$Z$X_tips_dummy_5,
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
  R_smooth_iid = R_smooth_iid,
  R_tips = mf$R$R_tips,
  R_tips_iid = as(diag(1, ncol(mf$Z$Z_tips_dhs)), "dgTMatrix"),
  # Z_zeta1 = sparse.model.matrix(~0 + id.zeta1, mf$observations$full_obs),
  Z_zeta2 = mf$Z$Z_zeta2,
  R_zeta2 = as(diag(1, ncol(mf$Z$X_tips_fe)), "dgTMatrix"),
  R_survey = as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix"),
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
  
  log_offset_phia = log(filter(mf$observations$full_obs, survtype == "PHIA")$pys),
  births_obs_phia = filter(mf$observations$full_obs, survtype == "PHIA")$births,
  
  include_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear < end_year)$idx,
  include_ais_obs = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"), survyear < end_year)$idx,
  include_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear < end_year)$idx,
  include_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear < end_year)$idx,
  
  exclude_dhs_obs = filter(mf$observations$full_obs, survtype == "DHS", survyear >= end_year)$idx,
  exclude_ais_obs = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"), survyear >= end_year)$idx,
  exclude_phia_obs = filter(mf$observations$full_obs, survtype == "PHIA", survyear >= end_year)$idx,
  exclude_mics_obs = filter(mf$observations$full_obs, survtype == "MICS", survyear >= end_year)$idx,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  A_tfr_out = mf$out$A_tfr_out,
  
  A_full_obs = mf$observations$A_full_obs,
  
  mics_toggle = mf$mics_toggle,
  
  X_spike_2000 = model.matrix(~0 + spike_2000, mf$observations$full_obs),
  X_spike_1999 = model.matrix(~0 + spike_1999, mf$observations$full_obs),
  X_spike_2001 = model.matrix(~0 + spike_2001, mf$observations$full_obs)

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
  
  # beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  beta_tips_dummy_5 = rep(0, ncol(mf$Z$X_tips_dummy_5)),
  beta_tips_fe = rep(0, ncol(mf$Z$X_tips_fe)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  lag_logit_phi_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  lag_logit_phi_age = 0,
  
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
  
  log_prec_smooth_iid = 0,
  u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
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
  logit_eta3_phi_age = 0,
  
  zeta2 = array(0, c(ncol(as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix")),
                     ncol(mf$Z$X_tips_fe)
                     )
                ),
  log_prec_zeta2 = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_smooth_iid",
                    "beta_period",
                    "beta_tips_dummy_5",
                    "beta_tips_fe",
                    # "beta_urban_dummy",
                    "u_tips",
                    "zeta2",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

# dyn.load(dynlib("rw1_trend"))

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "rw1_trend",
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

dhs_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype == "DHS", survyear >= end_year)
  

if(nrow(dhs_ppd)) {
  dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
  dhs_ppd <- dhs_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(dhsMatrix))
} else {
  dhs_ppd <- data.frame()
}

ais_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype %in% c("AIS", "MIS"), survyear >= end_year)
  

if(nrow(ais_ppd)) {
  aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
  ais_ppd <- ais_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(aisMatrix))
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
  bind_cols(data.frame(phiaMatrix))
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
  bind_cols(data.frame(micsMatrix))
} else {
  mics_ppd <- data.frame()
}


rw_trend_ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd)

true_values <- rw_trend_ppd %>%
  select(births, pys) %>%
  mutate(observed_asfr = births/pys) %>%
  .$observed_asfr

predictions <- rw_trend_ppd %>%
  select(X1:X1000) %>%
  as.matrix()

rw_trend_crps = sum(scoringutils::crps(true_values, predictions))

est_births <- as.matrix(rw_trend_ppd[,8:1007]) * rw_trend_ppd$pys
ll_pred <- dxpois(rw_trend_ppd$births, est_births, TRUE)
rw_trend_elpd <- loo::elpd(t(ll_pred))
rw_trend_elpd <- rw_trend_elpd$estimates %>%
  as.data.frame() %>%
  mutate(source = "rw_trend")

rw_trend_ppd <- rw_trend_ppd %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("X"), ~rpois(1, pys*.x))) %>%
  aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                   c(paste0("X", 1:1000), "births", "pys"),
                   1,
                   areas) %>%
  group_by(survey_id, period, area_id,tips) %>%
  summarise(
    survey_tfr = 5*sum(births/pys),
    across(starts_with("X"), ~5*sum(.x/pys)),
    observed_births = sum(births),
    pys = sum(pys)
  ) %>%
  ungroup() %>%
  select(survey_id:survey_tfr, observed_births, pys, everything())

qtls <- apply(select(rw_trend_ppd, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))

rw_trend_ppd <- rw_trend_ppd %>%
  group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
  rowwise() %>%
  summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
  ungroup %>%
  mutate(lower = qtls[1,],
         median = qtls[2,],
         upper = qtls[3,],
         source = "rw + trend")

########### ARIMA (1,1,0)

# TMB::compile("arima.cpp", flags = "-w")               # Compile the C++ file
# dyn.load(dynlib("arima"))

tmb_int$par <- list(
  beta_0 = 0,
  
  # beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  beta_tips_dummy_5 = rep(0, ncol(mf$Z$X_tips_dummy_5)),
  beta_tips_fe = rep(0, ncol(mf$Z$X_tips_fe)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  lag_logit_phi_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  lag_logit_phi_age = 0,
  
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
  
  log_prec_smooth_iid = 0,
  u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
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
  logit_eta3_phi_age = 0,
  
  zeta2 = array(0, c(ncol(as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix")),
                     ncol(mf$Z$X_tips_fe)
                     )
                ),
  log_prec_zeta2 = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_smooth_iid",
                    # "beta_period",
                    "beta_tips_dummy_5",
                    "beta_tips_fe",
                    # "beta_urban_dummy",
                    "u_tips",
                    "zeta2",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

# dyn.load(dynlib("arima"))

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

dhs_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype == "DHS", survyear >= end_year)
  

if(nrow(dhs_ppd)) {
  dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
  dhs_ppd <- dhs_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(dhsMatrix))
} else {
  dhs_ppd <- data.frame()
}

ais_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype %in% c("AIS", "MIS"), survyear >= end_year)
  

if(nrow(ais_ppd)) {
  aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
  ais_ppd <- ais_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(aisMatrix))
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
  bind_cols(data.frame(phiaMatrix))
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
  bind_cols(data.frame(micsMatrix))
} else {
  mics_ppd <- data.frame()
}


arima_ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd)

true_values <- arima_ppd %>%
  select(births, pys) %>%
  mutate(observed_asfr = births/pys) %>%
  .$observed_asfr

predictions <- arima_ppd %>%
  select(X1:X1000) %>%
  as.matrix()

arima_crps = sum(scoringutils::crps(true_values, predictions))

est_births <- as.matrix(arima_ppd[,8:1007]) * arima_ppd$pys
ll_pred <- dxpois(arima_ppd$births, est_births, TRUE)
arima_elpd <- loo::elpd(t(ll_pred))
arima_elpd <- arima_elpd$estimates %>%
  as.data.frame() %>%
  mutate(source = "arima")

arima_ppd <- arima_ppd %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("X"), ~rpois(1, pys*.x))) %>%
  aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                   c(paste0("X", 1:1000), "births", "pys"),
                   1,
                   areas) %>%
  group_by(survey_id, period, area_id,tips) %>%
  summarise(
    survey_tfr = 5*sum(births/pys),
    across(starts_with("X"), ~5*sum(.x/pys)),
    observed_births = sum(births),
    pys = sum(pys)
  ) %>%
  ungroup() %>%
  select(survey_id:survey_tfr, observed_births, pys, everything())

qtls <- apply(select(arima_ppd, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))

arima_ppd <- arima_ppd %>%
  group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
  rowwise() %>%
  summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
  ungroup %>%
  mutate(lower = qtls[1,],
         median = qtls[2,],
         upper = qtls[3,],
         source = "arima")

########### ARIMA with trend

# TMB::compile("arima_trend.cpp", flags = "-w")               # Compile the C++ file
# dyn.load(dynlib("arima_trend"))

tmb_int$par <- list(
  beta_0 = 0,
  
  # beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  beta_tips_dummy_5 = rep(0, ncol(mf$Z$X_tips_dummy_5)),
  beta_tips_fe = rep(0, ncol(mf$Z$X_tips_fe)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  lag_logit_phi_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  lag_logit_phi_age = 0,
  
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
  
  log_prec_smooth_iid = 0,
  u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
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
  logit_eta3_phi_age = 0,
  
  zeta2 = array(0, c(ncol(as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix")),
                     ncol(mf$Z$X_tips_fe)
                     )
                ),
  log_prec_zeta2 = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_smooth_iid",
                    "beta_period",
                    # "beta_tips_dummy",
                    "beta_tips_dummy_5",
                    "beta_tips_fe",
                    # "beta_urban_dummy",
                    "u_tips",
                    "zeta2",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

# dyn.load(dynlib("arima_trend"))

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

dhs_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype == "DHS", survyear >= end_year)
  

if(nrow(dhs_ppd)) {
  dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
  dhs_ppd <- dhs_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(dhsMatrix))
} else {
  dhs_ppd <- data.frame()
}

ais_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype %in% c("AIS", "MIS"), survyear >= end_year)
  

if(nrow(ais_ppd)) {
  aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
  ais_ppd <- ais_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(aisMatrix))
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
  bind_cols(data.frame(phiaMatrix))
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
  bind_cols(data.frame(micsMatrix))
} else {
  mics_ppd <- data.frame()
}


arima_trend_ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd)

true_values <- arima_trend_ppd %>%
  select(births, pys) %>%
  mutate(observed_asfr = births/pys) %>%
  .$observed_asfr

predictions <- arima_trend_ppd %>%
  select(X1:X1000) %>%
  as.matrix()

arima_trend_crps = sum(scoringutils::crps(true_values, predictions))

est_births <- as.matrix(arima_trend_ppd[,8:1007]) * arima_trend_ppd$pys
ll_pred <- dxpois(arima_trend_ppd$births, est_births, TRUE)
arima_trend_elpd <- loo::elpd(t(ll_pred))
arima_trend_elpd <- arima_trend_elpd$estimates %>%
  as.data.frame() %>%
  mutate(source = "arima_trend")

arima_trend_ppd <- arima_trend_ppd %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("X"), ~rpois(1, pys*.x))) %>%
  aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                   c(paste0("X", 1:1000), "births", "pys"),
                   1,
                   areas) %>%
  group_by(survey_id, period, area_id,tips) %>%
  summarise(
    survey_tfr = 5*sum(births/pys),
    across(starts_with("X"), ~5*sum(.x/pys)),
    observed_births = sum(births),
    pys = sum(pys)
  ) %>%
  ungroup() %>%
  select(survey_id:survey_tfr, observed_births, pys, everything())

qtls <- apply(select(arima_trend_ppd, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))

arima_trend_ppd <- arima_trend_ppd %>%
  group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
  rowwise() %>%
  summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
  ungroup %>%
  mutate(lower = qtls[1,],
         median = qtls[2,],
         upper = qtls[3,],
         source = "arima_trend")
########### AR1

# TMB::compile("ar1.cpp", flags = "-w")               # Compile the C++ file
# dyn.load(dynlib("ar1"))


tmb_int$par <- list(
  beta_0 = 0,
  
  # beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  beta_tips_dummy_5 = rep(0, ncol(mf$Z$X_tips_dummy_5)),
  beta_tips_fe = rep(0, ncol(mf$Z$X_tips_fe)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  lag_logit_phi_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  lag_logit_phi_age = 0,
  
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
  
  log_prec_smooth_iid = 0,
  u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
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
  logit_eta3_phi_age = 0,
  
  zeta2 = array(0, c(ncol(as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix")),
                     ncol(mf$Z$X_tips_fe)
  )
  ),
  log_prec_zeta2 = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_smooth_iid",
                    # "beta_period",
                    # "beta_tips_dummy",
                    "beta_tips_dummy_5",
                    "beta_tips_fe",
                    # "beta_urban_dummy",
                    "u_tips",
                    "zeta2",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

# dyn.load(dynlib("ar1"))

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

dhs_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype == "DHS", survyear >= end_year)
  

if(nrow(dhs_ppd)) {
  dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
  dhs_ppd <- dhs_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(dhsMatrix))
} else {
  dhs_ppd <- data.frame()
}

ais_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype %in% c("AIS", "MIS"), survyear >= end_year)
  

if(nrow(ais_ppd)) {
  aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
  ais_ppd <- ais_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(aisMatrix))
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
  bind_cols(data.frame(phiaMatrix))
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
  bind_cols(data.frame(micsMatrix))
} else {
  mics_ppd <- data.frame()
}


ar1_ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd)

true_values <- ar1_ppd %>%
  select(births, pys) %>%
  mutate(observed_asfr = births/pys) %>%
  .$observed_asfr

predictions <- ar1_ppd %>%
  select(X1:X1000) %>%
  as.matrix()

ar1_crps = sum(scoringutils::crps(true_values, predictions))

est_births <- as.matrix(ar1_ppd[,8:1007]) * ar1_ppd$pys
ll_pred <- dxpois(ar1_ppd$births, est_births, TRUE)
ar1_elpd <- loo::elpd(t(ll_pred))
ar1_elpd <- ar1_elpd$estimates %>%
  as.data.frame() %>%
  mutate(source = "ar1")

ar1_ppd <- ar1_ppd %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("X"), ~rpois(1, pys*.x))) %>%
  aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                   c(paste0("X", 1:1000), "births", "pys"),
                   1,
                   areas) %>%
  group_by(survey_id, period, area_id,tips) %>%
  summarise(
    survey_tfr = 5*sum(births/pys),
    across(starts_with("X"), ~5*sum(.x/pys)),
    observed_births = sum(births),
    pys = sum(pys)
  ) %>%
  ungroup() %>%
  select(survey_id:survey_tfr, observed_births, pys, everything())

qtls <- apply(select(ar1_ppd, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))

ar1_ppd <- ar1_ppd %>%
  group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
  rowwise() %>%
  summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
  ungroup %>%
  mutate(lower = qtls[1,],
         median = qtls[2,],
         upper = qtls[3,],
         source = "ar1")

########## AR1 + trend

# TMB::compile("ar1_trend.cpp", flags = "-w")               # Compile the C++ file
# dyn.load(dynlib("ar1_trend"))


tmb_int$par <- list(
  beta_0 = 0,
  
  # beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  beta_tips_dummy_5 = rep(0, ncol(mf$Z$X_tips_dummy_5)),
  beta_tips_fe = rep(0, ncol(mf$Z$X_tips_fe)),
  # beta_urban_dummy = rep(0, ncol(mf$Z$X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  lag_logit_phi_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  lag_logit_phi_age = 0,
  
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
  
  log_prec_smooth_iid = 0,
  u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  
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
  logit_eta3_phi_age = 0,
  
  zeta2 = array(0, c(ncol(as(diag(1, length(unique(mf$observations$full_obs$survey_id))), "dgTMatrix")),
                     ncol(mf$Z$X_tips_fe)
                     )
                ),
  log_prec_zeta2 = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_smooth_iid",
                    "beta_period",
                    # "beta_tips_dummy",
                    "beta_tips_dummy_5",
                    "beta_tips_fe",
                    # "beta_urban_dummy",
                    "u_tips",
                    "zeta2",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3"
                    # "omega1",
                    # "omega2"
)

# dyn.load(dynlib("ar1_trend"))

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

dhs_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype == "DHS", survyear >= end_year)
  

if(nrow(dhs_ppd)) {
  dhsMatrix <- exp(fit$sample$log_rate_exclude_dhs)
  dhs_ppd <- dhs_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(dhsMatrix))
} else {
  dhs_ppd <- data.frame()
}

ais_ppd <- mf$observations$full_obs %>%
  ungroup() %>%
  filter(survtype %in% c("AIS", "MIS"), survyear >= end_year)
  

if(nrow(ais_ppd)) {
  aisMatrix <- exp(fit$sample$log_rate_exclude_ais)
  ais_ppd <- ais_ppd %>%
  select(survey_id, area_id, period, age_group, births, tips, pys) %>%
   bind_cols(data.frame(aisMatrix))
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
  bind_cols(data.frame(phiaMatrix))
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
  bind_cols(data.frame(micsMatrix))
} else {
  mics_ppd <- data.frame()
}


ar1_trend_ppd <- bind_rows(dhs_ppd, ais_ppd, phia_ppd, mics_ppd)

true_values <- ar1_trend_ppd %>%
  select(births, pys) %>%
  mutate(observed_asfr = births/pys) %>%
  .$observed_asfr

predictions <- ar1_trend_ppd %>%
  select(X1:X1000) %>%
  as.matrix()

ar1_trend_crps = sum(scoringutils::crps(true_values, predictions))

est_births <- as.matrix(ar1_trend_ppd[,8:1007]) * ar1_trend_ppd$pys
ll_pred <- dxpois(ar1_trend_ppd$births, est_births, TRUE)
ar1_trend_elpd <- loo::elpd(t(ll_pred))
ar1_trend_elpd <- ar1_trend_elpd$estimates %>%
  as.data.frame() %>%
  mutate(source = "ar1_trend")

ar1_trend_ppd <- ar1_trend_ppd %>%
  ungroup() %>%
  rowwise() %>%
  mutate(across(starts_with("X"), ~rpois(1, pys*.x))) %>%
  aggregate_to_admin(c("survey_id", "period", "age_group", "tips"),
                   c(paste0("X", 1:1000), "births", "pys"),
                   1,
                   areas) %>%
  group_by(survey_id, period, area_id,tips) %>%
  summarise(
    survey_tfr = 5*sum(births/pys),
    across(starts_with("X"), ~5*sum(.x/pys)),
    observed_births = sum(births),
    pys = sum(pys)
  ) %>%
  ungroup() %>%
  select(survey_id:survey_tfr, observed_births, pys, everything())

qtls <- apply(select(ar1_trend_ppd, starts_with("X")), 1, quantile, c(0.025, 0.5, 0.975))

ar1_trend_ppd <- ar1_trend_ppd %>%
  group_by(survey_id, area_id, period, tips, observed_births, pys, survey_tfr) %>%
  rowwise() %>%
  summarise(quant_pos = sum(across(starts_with("X"), ~quant_pos_sum(survey_tfr, .x)))) %>%
  ungroup %>%
  mutate(lower = qtls[1,],
         median = qtls[2,],
         upper = qtls[3,],
         source = "ar1_trend")

pred <- bind_rows(rw_ppd, rw2_ppd, arima_ppd, arima_trend_ppd, ar1_ppd, ar1_trend_ppd)
elpd <- bind_rows(rw_elpd, rw2_elpd, arima_elpd, arima_trend_elpd, ar1_elpd, ar1_trend_elpd)
crps <- data.frame("rw" = rw_crps,
                   "rw2" = rw2_crps,
                   "arima" = arima_crps,
                   "arima_trend" = arima_trend_crps,
                   "ar1" = ar1_crps,
                   "ar1_trend" = ar1_trend_crps)

write_csv(tmb_results, "fr.csv")
write_csv(pred, "pred.csv")
write_csv(elpd, "elpd.csv")
write_csv(crps, "elpd.csv")