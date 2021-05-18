devtools::load_all("~/Documents/GitHub/dfertility/")

library(tidyverse)
library(naomi)
library(sf)
library(dfertility)
library(Matrix)
library(TMB)

lvl <- read_csv("~/Documents/GitHub/subnat_fertility/input_data/lvl_df.csv") %>%
  # filter(!iso3 %in% c("ETH", "COD")) %>%
  filter(iso3 %in% c("NAM", "ZMB", "ZWE", "SWZ", "MWI")) %>%
  # filter(iso3 %in% c("ZWE")) %>%
  arrange(iso3)

interaction_level <- filter(lvl, area_level_name == "province")$area_level_id

iso3_list <- unique(lvl$iso3)

area_names <- sort(paste0("archive/", tolower(iso3_list), "_data_areas"))
asfr_names <- paste0("archive/", tolower(iso3_list), "_asfr")
asfr_names <- grep("nga", asfr_names, value=TRUE, invert=TRUE)
# asfr_names <- sort(c(asfr_names, "archive/nga_merge_asfr"))
population_names <- sort(paste0("archive/", tolower(iso3_list), "_data_population"))

areas_list <- lapply(area_names, list.files, full.name=TRUE) %>%
  lapply(tail, 1) %>%
  lapply(list.files, pattern="_areas.geojson", full.name=TRUE) %>%
  lapply(read_sf) %>%
  lapply(select, c(area_id:parent_area_id, area_sort_order, area_level_label, display, spectrum_region_code, geometry)) %>%
  bind_rows() %>%
  separate(area_id, into=c("iso3", "area_level", NA), sep="_", remove = FALSE, convert = TRUE) %>%
  mutate(area_level = ifelse(is.na(area_level), 0, area_level)) %>%
  group_split(iso3)

asfr <- lapply(asfr_names, list.files, full.name=TRUE) %>%
  lapply(tail, 1) %>%
  lapply(list.files, pattern="_asfr.csv", full.name=TRUE) %>%
  lapply(function(x) {
    if(length(x) > 1) {
      grep("_dhs|_mics", x, value=TRUE, invert=TRUE)
    } else {
      x
    }
  }) %>%
  lapply(read.csv)

aaa_populations <- list.files("archive/aaa_data_population_worldpop", full.names = TRUE) %>%
  lapply(list.files, full.names=TRUE, pattern = ".csv") %>%
  lapply(read.csv) %>%
  bind_rows()

population <- lapply(c("archive/bdi_data_population/20201127-134436-054a80f6/bdi_population_gpw.csv",
"archive/bfa_data_population/20201127-135259-2caa0a3f/bfa_population_gpw.csv",
"archive/civ_data_population/20201201-105405-7f809ce7/civ_population_gpw.csv",
"archive/cmr_data_population/20201125-202009-b20905bc/cmr_population_ins.csv",
# "archive/cod_data_population/20201113-112238-0cdb556d/cod_population_gpw.csv",
"archive/eth_data_population/20201125-222059-23ceccb4/eth_population_gpw.csv",
"archive/gmb_data_population/20210114-160040-35e2f724/gmb_population_moh-projections.csv",
"archive/ken_data_population/20201130-134607-5df2f0e6/ken_population_knbs-census19.csv",
"archive/lso_data_population/20201125-211633-4c92792c/lso_population_census16.csv",
"archive/moz_data_population/20201130-094019-f924623c/moz_population_nso.csv",
"archive/mwi_data_population/20201125-095927-2281c97e/mwi_population_census18.csv",
"archive/nam_data_population/20201126-122848-78e12eb6/nam_population_gpw.csv",
"archive/nga_data_population/20201130-141351-a7827dc7/nga_population_grid3.csv",
"archive/rwa_data_population/20201130-145722-296bcdf9/rwa_population.csv",
"archive/swz_data_population/20201130-150332-bd7e6821/swz_world_pop_for_naomi.csv",
"archive/tgo_data_population/20201130-154944-e28658ad/tgo_population_nso.csv",
"archive/tza_data_population/20201130-151425-3472d606/tza_population_tnbs.csv",
"archive/uga_data_population/20201130-170229-8c69d8e4/uga_population_ubos.csv",
"archive/zmb_data_population/20201130-163948-1fa3fdee/zmb_population_nso.csv",
"archive/zwe_data_population/20201130-162514-ea8350d2/zwe_population_nso.csv"), read.csv) %>%
  bind_rows() %>%
  bind_rows(aaa_populations) %>%
  separate(area_id, into=c("iso3", "area_level", NA), sep="_", remove = FALSE, convert = TRUE) %>%
  mutate(area_level = ifelse(is.na(area_level), 0, area_level)) %>%
  select(area_id:population) %>%
  filter(iso3 %in% iso3_list, !is.na(iso3), age_group %in% filter(get_age_groups(), age_group_sort_order %in% 16:22)$age_group) %>%
  group_split(iso3)

names(asfr) <- names(population) <- iso3_list

asfr[["KEN"]] <- asfr[["KEN"]] %>%
  filter(survey_id != "KEN2014DHS")

# debugonce(make_model_frames_batch)
mf <- make_model_frames_batch(population, asfr,  areas_list, lvl, project=2020)

mf$observations$full_obs  <- mf$observations$full_obs %>%
  mutate(
    obs_idx = row_number())

mf$observations$full_obs <- mf$observations$full_obs %>%
  left_join(
    mf$observations$full_obs %>%
      filter(survtype == "DHS") %>%
      mutate(id.interaction4 = factor(group_indices(., survey_id, tips_f), levels = 1:(length(unique(filter(mf$observations$full_obs, survtype == "DHS")$survey_id))*15))) %>%
      select(obs_idx, id.interaction4)
  )

# mf$mf_model <- mf$mf_model %>%
#   mutate(
#     id.interaction2 = factor(group_indices(., period, area_id)),
#     id.interaction3 = factor(group_indices(., age_group, area_id)),
#   )

TMB::compile("global/tmb_admin1_interaction_multi.cpp")               # Compile the C++ file
dyn.load(dynlib("global/tmb_admin1_interaction_multi"))

X_tips_dummy_interaction <- model.matrix(~ -1 + tips_dummy * as.factor(iso3) - as.factor(iso3),
                             data = mf$observations$full_obs %>% filter(survtype == "DHS"))

X_period_interaction <- model.matrix(~ -1 + id.period * as.factor(iso3) - as.factor(iso3) - id.period,
                             data = mf$mf_model)

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_model_level_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  # X_tips_dummy_interaction = X_tips_dummy_interaction,
  X_period = as(mf$Z$X_period, "matrix"),
  X_period_interaction = X_period_interaction,
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
  # Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  # Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_interaction4 = sparse.model.matrix(~0 + id.interaction4, mf$observations$full_obs %>% filter(survtype == "DHS")),
  Z_country = sparse.model.matrix(~0 + iso3, mf$mf_model),
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = mf$R$R_period,
  R_spatial = mf$R$R_spatial,
  R_spatial_interaction = mf$R$R_spatial_interaction,
  # R_spatial_interaction = mf$R$R_spatial,
  R_survey = as(diag(1, length(unique(filter(mf$observations$full_obs, survtype == "DHS")$survey_id))), "dgTMatrix"),
  # R_spatial_admin1 = as(matrix(1), "dgTMatrix"),
  R_country = mf$R$R_country,
  rankdef_R_spatial = 1,

  log_offset_naomi = log(mf$observations$model_level_obs$pys),
  births_obs_naomi = mf$observations$model_level_obs$births,

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
  # beta_tips_dummy_interaction = rep(0, ncol(X_tips_dummy_interaction)),
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  log_prec_rw_tips = 0,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 0,
  
  u_country = rep(0, length(areas_list)),
  log_prec_country = 0,

  # omega1 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_age))),
  omega1 = array(0, c(length(areas_list), ncol(mf$Z$Z_age))),
  log_prec_omega1 = 0,
  lag_logit_omega1_phi_age = 0,

  omega2 = array(0, c(length(areas_list), ncol(mf$Z$Z_period))),
  # omega2 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_period))),
  log_prec_omega2 = 0,
  lag_logit_omega2_phi_period = 0,
  
  u_period = rep(0, ncol(mf$Z$Z_period)),
  log_prec_rw_period = 0,
  # lag_logit_phi_period = 0,
  lag_logit_phi_arima_period = 0,
  beta_period = 0,
  beta_period_interaction = rep(0, ncol(X_period_interaction)),
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(length(areas_list), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  # eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  #
  eta2 = array(0, c(ncol(mf$R$R_spatial_interaction), ncol(mf$Z$Z_period))),
  # eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(ncol(mf$R$R_spatial_interaction), ncol(mf$Z$Z_age))),
  # eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
  
  # eta4 = array(0, c(length(unique(filter(mf$observations$full_obs, survtype == "DHS")$survey_id)), ncol(mf$R$R_tips))),
  # log_prec_eta4 = 0,
  # lag_logit_eta4_phi_tips = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_country",
                    "beta_period",
                    "beta_period_interaction",
                    "beta_tips_dummy",
                    # "beta_tips_dummy_interaction",
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "eta1",
                    "eta2",
                    "eta3",
                    # "eta4",
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
  )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}

# 
f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                          parameters = tmb_int$par,
                                          random = tmb_int$random,
                                          DLL = "tmb_admin1_interaction_multi",
                                          silent=0,
                                          checkParameterOrder=FALSE)
})

parallel::mccollect(f)

saveRDS(tmb_int, "tmb_int_8.rds")
tmb_int <- readRDS("tmb_int_8.rds")

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "tmb_admin1_interaction_multi",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=FALSE)

tmb_results <- dfertility::tmb_outputs(fit, mf, areas_list %>% bind_rows)

tmb_results_l <- tmb_results %>%
  separate(area_id, sep="_", remove=FALSE, into=c("iso3", NA)) %>%
  group_split(iso3)

fr_plot <- lapply(asfr_names, list.files, full.name=TRUE) %>%
  lapply(tail, 1) %>%
  lapply(list.files, pattern="plot", full.name=TRUE) %>%
  lapply(read.csv) %>%
  Map(function(x, areas) {
    x %>%
      left_join(areas %>% st_drop_geometry() %>% select(area_id, area_name))
  }, ., areas_list)

fit_names <- paste0("archive/", tolower(iso3_list), "_fit")

separate_res <- lapply(fit_names, list.files, full.name=TRUE) %>%
  lapply(tail, 1) %>%
  lapply(list.files, pattern="fr.csv", full.name=TRUE) %>%
  lapply(read.csv) %>%
  lapply(function(x) {
    x %>%
      mutate(source = "Separate")
  })

joint_no_int <- readRDS("joint_tmb_results.rds") %>%
  filter(source == "model2") %>%
  group_split(iso3)

plots <- Map(function(tmb_results, separate_res, joint_no_int, fr_plot) {
  tmb_results %>%
    mutate(source = "Joint Interaction") %>%
    bind_rows(joint_no_int %>% mutate(source = "Joint no interaction")) %>%
    bind_rows(separate_res) %>%
    filter(area_level == 1, variable == "tfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(aes(color=source)) +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=0.5) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10), aes(y=value)) +
    facet_wrap(~area_name, ncol=5) +
    # labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )
}, tmb_results_l, separate_res, joint_no_int, fr_plot)

hyper_names <- grep(names(fit$sample), pattern = "out", invert = TRUE, value=TRUE)

# hyper_fitted <- lapply(hyper_names, function(x) {
#   hyper <- fit$sample[[x]][1,]
#   distr <- MASS::fitdistr(hyper, "normal")
#   distr$estimate
# })

names(hyper_fitted) <- hyper_names

# saveRDS(hyper_fitted, "hyper_fitted.rds")

joint_hyper_distributions <- lapply(hyper_names, function(x) {
  hyper <- fit$sample[[x]][1,]
  data.frame(x=hyper, hyper = x)
  # distr <- MASS::fitdistr(hyper, "normal")
  # distr$estimate
}) %>%
  bind_rows()

# saveRDS(joint_hyper_distributions, "joint_hyper_distributions.rds")
# saveRDS(tmb_results, "joint_tmb_results.rds")

# tmb_results_joint <- readRDS("joint_tmb_results.rds")
# joint_hyper_distributions <- readRDS("joint_hyper_distributions.rds")
# 
# joint_hyper_distributions <- joint_hyper_distributions %>%
#   mutate(model = "joint")
# 
# zwe_hyper_distributions <- lapply(hyper_names, function(x) {
#   hyper <- fit$sample[[x]][1,]
#   data.frame(x=hyper, hyper = x)
#   # distr <- MASS::fitdistr(hyper, "normal")
#   # distr$estimate
# }) %>%
#   bind_rows() %>%
#   mutate(model = "zwe")

joint_hyper_distributions %>%
  # bind_rows(zwe_hyper_distributions) %>%
  ggplot() +
    geom_density(aes(x=x)) +
    facet_wrap(~hyper, scales="free")

prev_res <- readRDS("joint_tmb_results.rds")
prev_hyper <- readRDS("joint_hyper_distributions.rds")

saveRDS(tmb_results %>% mutate(source = "model3") %>% bind_rows(prev_res), "joint_tmb_results.rds")
saveRDS(joint_hyper_distributions %>% mutate(source = "model3") %>% bind_rows(prev_hyper), "joint_hyper_distributions.rds")

tmb_results %>%
  mutate(source = "admin2") %>%
  bind_rows(tmb_res_admin1, separate_res[[1]] %>% mutate(source = "separate")) %>%
  filter(area_level == 1, variable == "tfr") %>%
  ggplot(aes(x=period, y=median)) +
  geom_line(aes(color=source)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=0.5) +
  # geom_point(data = fr_plot %>% filter(variable == "tfr", value <10), aes(y=value)) +
  facet_wrap(~area_name, ncol=5) +
  # labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size=14)
  )
