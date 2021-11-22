population <- read.csv("depends/interpolated_population.csv") %>%
  rename(period = year) %>%
  mutate(iso3 = iso3) %>%
  filter(sex == "female")

areas <- read_sf("depends/naomi_areas.geojson") %>%
  mutate(iso3 = iso3)

asfr <- read.csv("depends/fertility_asfr.csv") %>%
  filter(survtype == "DHS")

lvl_map <- read.csv("resources/iso_mapping_fit.csv")
lvl <- lvl_map$fertility_fit_level[lvl_map$iso3 == iso3]
admin1_lvl <- lvl_map$admin1_level[lvl_map$iso3 == iso3]

# population <- read.csv("archive/aaa_data_population_worldpop/20210106-203832-d9202b45/population_worldpop_naomi.csv")
# areas <- read_sf("archive/ago_data_areas/20210105-150243-778fa342/ago_areas.geojson")
# asfr <- read.csv("archive/ago_asfr/20210122-093323-ccda8444/ago_dhs_asfr.csv")

mf <- make_model_frames_dev(iso3, population, asfr,  areas, naomi_level = lvl, project=2020)

validate_model_frame(mf, areas)

TMB::compile("tmb_all_level_poisson.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("tmb_all_level_poisson"))

tmb_int <- list()

tmb_int$data <- list(
  M_naomi_obs = mf$M_naomi_obs,
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
  # Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  # Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
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
  
  # beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  # u_tips = rep(0, ncol(mf$Z$Z_tips_dhs)),
  # log_prec_rw_tips = 0,
  
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
  # lag_logit_phi_period = 0,
  # lag_logit_phi_arima_period = 0,
  # beta_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0
  # log_overdispersion = 0,
  
  # eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  # log_prec_eta1 = 0,
  # lag_logit_eta1_phi_age = 0,
  # lag_logit_eta1_phi_period = 0,
  #
  # eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  # log_prec_eta2 = 0
  # lag_logit_eta2_phi_period = 0
  #
  # eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  # log_prec_eta3 = 0,
  # lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    # "beta_period",
                    # "beta_tips_dummy",
                    # "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001"
                    # "eta1",
                    # "eta2"
                    # "eta3"
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

if(is.null(parallel::mccollect(f)[[1]])) {
  stop("TMB model is invalid. This is most likely an indexing error e.g. iterating over dimensions in an array that do not exist. Check mf model object")
}

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
# 
# sd_report <- fit$sdreport
# sd_report <- summary(sd_report, "all") %>%
#   .[rownames(.) %in% c("log_prec_rw_tips", "log_prec_spatial", "log_prec_eta1", "lag_logit_eta1_phi_age", "lag_logit_eta1_phi_period", "log_prec_eta2", "lag_logit_eta2_phi_period", "log_prec_eta3", "lag_logit_eta3_phi_age", "log_prec_rw_age", "log_prec_rw_period", "lag_logit_phi_arima_period", "beta_tips_dummy"), ]
# 
# hyper_sd <- data.frame(sd_report, "hyper" = rownames(sd_report), iso = iso3) %>%
#   mutate(source = "RW1 both")

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

# tmb_results <- dfertility::tmb_outputs(fit, mf, areas)
# 
# fr_plot <- read.csv("depends/fertility_fr_plot.csv")
# 
# fr_plot <- fr_plot %>%
#   left_join(areas %>% st_drop_geometry() %>% select(area_id, area_name))
# 
# tmb_results %>%
#   filter(area_level == admin1_lvl, variable == "tfr") %>%
#   ggplot(aes(x=period, y=median)) +
#   geom_line() +
#   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
#   geom_point(data = fr_plot %>% filter(variable == "tfr", value <10), aes(y=value, color=survey_id)) +
#   facet_wrap(~area_name, ncol=5) +
#   labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom",
#     text = element_text(size=14)
#   )
# 
# eta2_v <- apply(fit$sample$eta2, 1, median)
# dim(eta2_v) <- c(135,26)
# data.frame(eta2_v) %>%
#   mutate(idx = row_number()) %>%
#   pivot_longer(-idx) %>%
#   mutate(name = rep(1:26, times = 135)) %>%
#   ggplot(aes(x=name, y=value)) +
#     geom_point() +
#     geom_line() +
#     facet_wrap(~idx)

full_obs_pp <- mf$observations$full_obs %>%
  ungroup %>%
  mutate(idx = row_number())

births_pred_dhs <- rpois(length(fit$sample$mu_obs_pred_dhs),
                      exp(fit$sample$mu_obs_pred_dhs)
)

dim(births_pred_dhs) <- dim(fit$sample$mu_obs_pred_dhs)

foo <- data.frame(births_pred_dhs) %>%
  # cbind("idx" = as.numeric(mf$X_extract$X_extract_dhs %*% full_obs_pp$idx)) %>%
  type.convert()

t1 <- foo %>%
  cbind(mf$observations$full_obs %>% select(survey_id, area_id, period, age_group, tips, births, pys, idx) %>% type.convert()) %>%
  filter(str_detect(area_id, "_2_"),
         period %in% 2000:2015) %>%
  # left_join(spread_areas(areas) %>% select(area_id1, area_id) %>% st_drop_geometry()) %>%
  select(survey_id, area_id, period, age_group, tips, births, pys, everything(), -idx) %>%
  group_by(survey_id, area_id, age_group, period, tips) %>%
  summarise(births = sum(births), 
            pys = sum(pys),
            across(X1:X1000, sum)
  )

t1_line <- t1 %>%
  ungroup %>%
  rowwise() %>%
  mutate(lower = quantile(c_across(X1:X1000), 0.025),
         median = quantile(c_across(X1:X1000), 0.5),
         upper = quantile(c_across(X1:X1000), 0.975),
         p = mean(births >= c_across(X1:X1000))
         ) %>%
  select(survey_id:pys, lower:upper, p)

t1_line %>%
  ungroup %>%
  mutate(width_est = upper - lower) %>%
  left_join(mf$observations$full_obs %>%
              filter(str_detect(area_id, "_2_"),
                    period %in% 2000:2015) %>%
              mutate(lower_data = qpois(0.025, births),
                     median_data = qpois(0.5, births),
                     upper_data = qpois(0.975, births),
                     width_data = upper_data - lower_data
              ) %>%
              select(survey_id:pys, width_data) %>%
              type.convert()) %>%
  arrange(width_data) %>%
  ggplot(aes(x=width_data, y=width_est)) +
    geom_point()

t1 <- t1 %>%
  pivot_longer(-c(survey_id:pys))

t1_lab <- t1 %>%
  select(survey_id:pys) %>%
  distinct()

t1 %>%
  filter(area_id1 == "RWA_1_1") %>%
  ggplot() +
    geom_density(aes(x=value)) +
    geom_vline(data = t1_lab %>% filter(area_id1 == "RWA_1_1"), aes(xintercept = births)) +
    facet_grid(tips ~ period)



t1_line %>%
  ungroup %>%
  ggplot(aes(sample = median)) + 
  stat_qq(distribution = stats::qpois,
          dparams = list(lambda = median(t1_line$births)))
