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

remove_survey <- c("CIV2005AIS", "COG2014MICS", "MLI2009MICS", "MLI2015MICS", "SLE2010MICS", "TGO2006MICS", "BEN1996DHS", "KEN2009MICS", "COD2017MICS")
subnational_surveys <- c("KEN2009MICS", "KEN2011MICS")

asfr <- asfr %>% 
  bind_rows(phia_asfr) %>%
  filter(!survey_id %in% remove_survey)

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

mf <- make_model_frames_dev(iso3, population, asfr,  areas, naomi_level = lvl, project=2020)

mf$observations$full_obs <- mf$observations$full_obs %>%
  ungroup() %>%
  mutate(id.smooth = factor(row_number()))

# R_smooth_iid <- as(diag(nrow = nrow(mf$observations$full_obs)), "sparseMatrix")
R_smooth_iid <- sparseMatrix(nrow(mf$observations$full_obs), nrow(mf$observations$full_obs), rep(1, nrow(mf$observations$full_obs)))

x <- 0:25
k <- seq(-15, 40, by = 5)
spline_mat <- splines::splineDesign(k, x, ord = 4)
spline_mat <- as(spline_mat, "sparseMatrix")

mf$Z$Z_period <- mf$Z$Z_period %*% spline_mat

validate_model_frame(mf, areas)

# TMB::compile("src/aaa_fit/no_tips.cpp", flags = "-w")               # Compile the C++ file
# TMB::compile("no_tips.cpp", flags = "-w")               # Compile the C++ file
# dyn.load(dynlib("no_tips"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
  M_full_obs = mf$M_full_obs,
  X_tips_dummy = mf$Z$X_tips_dummy,
  X_tips_dummy_10 = mf$Z$X_tips_dummy_10,
  X_tips_dummy_9_11 = mf$Z$X_tips_dummy_9_11,
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
  R_age = mf$R$R_age,
  # R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
  R_period = make_rw_structure_matrix(ncol(spline_mat), 1, adjust_diagonal = TRUE),
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
  
  X_spike_2000 = model.matrix(~0 + spike_2000, mf$observations$full_obs),
  X_spike_1999 = model.matrix(~0 + spike_1999, mf$observations$full_obs),
  X_spike_2001 = model.matrix(~0 + spike_2001, mf$observations$full_obs)

  # X_spike_2000_dhs = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "DHS")),
  # X_spike_1999_dhs = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "DHS")),
  # X_spike_2001_dhs = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "DHS")),
  # 
  # X_spike_2000_ais = model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  # X_spike_1999_ais = model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),
  # X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype %in% c("AIS", "MIS"))),

  # n_threads = parallel::detectCores()

  # out_toggle = mf$out_toggle
  # A_obs = mf$observations$A_obs,
)

tmb_int$par <- list(
  beta_0 = 0,

  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  beta_tips_dummy_10 = rep(0, ncol(mf$Z$X_tips_dummy_10)),
  # beta_tips_dummy_9_11 = rep(0, ncol(mf$Z$X_tips_dummy_9_11)),
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
  logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "u_smooth_iid",
                    "beta_period",
                    "beta_tips_dummy",
                    "beta_tips_dummy_10",
                    # "beta_tips_dummy_9_11",
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
                    "births_obs_mics" = list(filter(mf$observations$full_obs, survtype == "MICS")$births)

                    # "X_spike_2000_mics" = list(model.matrix(~0 + spike_2000, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    # "X_spike_1999_mics" = list(model.matrix(~0 + spike_1999, mf$observations$full_obs %>% filter(survtype == "MICS"))),
                    # "X_spike_2001_mics" = list(model.matrix(~0 + spike_2001, mf$observations$full_obs %>% filter(survtype == "MICS")))

                    # "Z_smooth_iid_mics" = sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype == "MICS"))
  )
  # tmb_int$par <- c(tmb_int$par,
  #                  "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  # )
  # tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                               parameters = tmb_int$par,
                               DLL = "no_tips",
                               silent=0,
                               checkParameterOrder=FALSE)
})

if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "no_tips",
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
# sd_report <- data.frame(x= "foo")

write_csv(sd_report, "sd_report.csv")

class(fit) <- "naomi_fit"  # this is hacky...

fit <- naomi::sample_tmb(fit, random_only=TRUE)
tmb_results <- dfertility::tmb_outputs(fit, mf, areas)
write_csv(tmb_results, "fr.csv")
 
# fit <- naomi::sample_tmb(fit, random_only=FALSE)
# hyper <- fit$sample %>%
#   list_modify("lambda_out" = zap(), "tfr_out" = zap())
# saveRDS(hyper, "hyper.rds")

fr_plot <- read.csv("depends/fr_plot.csv")

fr_plot <- fr_plot %>%
  left_join(areas %>% st_drop_geometry() %>% select(area_id, area_name))

fr_plot <- fr_plot %>%
  bind_rows(
    read_csv("resources/phia_asfr_admin1.csv") %>%
      separate(area_id1, into=c("iso3", NA), sep = 3, remove = FALSE) %>%
      filter(iso3 == iso3_c) %>%
      group_by(survey_id, period, area_id1) %>%
      summarise(value = 5*sum(asfr)) %>%
      ungroup %>%
      rename(area_id = area_id1) %>%
      left_join(areas %>% st_drop_geometry() %>% select(area_id, area_name)) %>%
      mutate(variable = "tfr")
  ) %>%
  filter(!(area_id == iso3 & survey_id %in% subnational_surveys))

cntry_name <- countrycode::countrycode(iso3, "iso3c", "country.name")

plot_prep <- tmb_results %>%
  filter(area_level %in% c(0, admin1_lvl), variable == "tfr")

tfr_plot <- plot_prep %>%
  ggplot(aes(x=period, y=median)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
  geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, !survey_id %in% remove_survey), aes(y=value, color=survey_id)) +
  geom_point(data = fr_plot %>% filter(variable == "tfr", value <10, survey_id %in% remove_survey), aes(y=value, color=survey_id), shape=22, fill=NA) +
  facet_wrap(~fct_relevel(area_name, levels=c(cntry_name, unique(plot_prep$area_name)[!unique(plot_prep$area_name) == cntry_name])), ncol=5) +
  labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    text = element_text(size=14)
  )

district_tfr <- tmb_results %>%
  filter(area_level == lvl, variable == "tfr") %>%
  ggplot(aes(x=period, y=median)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
  facet_wrap(~area_name, ncol=8) +
  theme_minimal() +
  labs(y="TFR", x=element_blank(), title=paste(iso3, "| District TFR"))

dir.create("check")
pdf("check/tfr_admin1.pdf", h = 12, w = 20)
tfr_plot
dev.off()
pdf("check/tfr_district.pdf", h = 12, w = 20)
district_tfr
dev.off()