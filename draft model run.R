iso3 <- "CMR"

population <- read.csv(paste0("depends/", tolower(iso3), "_population_gpw.csv"))
areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))
asfr <- read.csv(paste0("depends/", tolower(iso3), "_dhs_asfr.csv"))

# debugonce(make_model_frames)
mf <- dfertility::make_model_frames(iso3, population, asfr, mics_asfr=NULL, areas, model_level =3, project=2020)

mf$district$obs <- mf$district$obs %>%
  mutate(
    spike_2000 = ifelse(period == 2000, 1, 0),
    spike_1999 = ifelse(period == 1999, 1, 0),
    spike_2001 = ifelse(period == 2001, 1, 0),
         )

# TMB::compile("global/tmb_nb_spike.cpp")               # Compile the C++ file
TMB::compile("~/Documents/GitHub/dfertility/src/tmb_nb_spike.cpp")               # Compile the C++ file
dyn.load(dynlib("~/Documents/GitHub/dfertility/src/tmb_nb_spike"))

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
                     out_toggle = mf$out_toggle,
                     eth_toggle = 0,
                     
                     X_spike_2000_dhs = model.matrix(~0 + spike_2000, mf$district$obs %>% filter(ais_dummy==0)),
                     X_spike_1999_dhs = model.matrix(~0 + spike_1999, mf$district$obs %>% filter(ais_dummy==0)),
                     X_spike_2001_dhs = model.matrix(~0 + spike_2001, mf$district$obs %>% filter(ais_dummy==0)),
                     
                     X_spike_2000_ais = model.matrix(~0 + spike_2000, mf$district$obs %>% filter(ais_dummy == 1)),
                     X_spike_1999_ais = model.matrix(~0 + spike_1999, mf$district$obs %>% filter(ais_dummy == 1)),
                     X_spike_2001_ais = model.matrix(~0 + spike_2001, mf$district$obs %>% filter(ais_dummy == 1))
)

tmb_int$par <- list(
  beta_0 = 0,
  
  beta_tips_dummy = rep(0, ncol(mf$Z$X_tips_dummy)),
  # beta_urban_dummy = rep(0, ncol(X_urban_dummy)),
  u_tips = rep(0, ncol(mf$Z$Z_tips)),
  log_prec_rw_tips = 5.952751057,
  
  u_age = rep(0, ncol(mf$Z$Z_age)),
  log_prec_rw_age = 1.200289582,
  
  # u_country = rep(0, ncol(mf$Z$Z_country)),
  # log_prec_country = 0,
  
  omega1 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_age))),
  log_prec_omega1 = 9.921401819,
  lag_logit_omega1_phi_age = 0.125913846,

  omega2 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_period))),
  log_prec_omega2 = 7.406561742,
  lag_logit_omega2_phi_period = -0.983030653,
  
  u_period = rep(0, ncol(mf$Z$Z_period)),
  # log_prec_rw_period = 5.950742385,
  log_prec_rw_period = 0,
  lag_logit_phi_period = 0,
  # lag_logit_ar2_phi_period = c(0,0),
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 9.904655446,
  
  log_overdispersion = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 2.655949618,
  lag_logit_eta1_phi_age = 2.531602079,
  lag_logit_eta1_phi_period = 4.714228565,
  #
  eta2 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 6.932968652,
  lag_logit_eta2_phi_period = -1.850375768,
  #
  eta3 = array(0, c(ncol(mf$Z$Z_spatial), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 2.475220973,
  lag_logit_eta3_phi_age = 3.658756756
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "beta_tips_dummy",
                    "u_tips",
                    "beta_spike_2000",
                    "beta_spike_1999",
                    "beta_spike_2001",
                    "omega1",
                    "omega2",
                    "eta1",
                    "eta2",
                    "eta3")

if(mf$mics_toggle) {
  tmb_int$data <- c(tmb_int$data,
                    "M_obs_mics" = mf$mics$M_obs_mics,
                    "Z_tips_mics" = mf$Z$Z_tips_mics,
                    "R_tips_mics" = mf$R$R_tips_mics,
                    "births_obs_mics" = list(mf$mics$obs$births),
                    "log_offset_mics" = list(log(mf$mics$obs$pys)),
                    "A_mics" = mf$mics$A_mics,
                    "X_spike_2000_mics" = model.matrix(~0 + spike_2000, mf$mics$obs),
                    "X_spike_1999_mics" = model.matrix(~0 + spike_1999, mf$mics$obs),
                    "X_spike_2001_mics" = model.matrix(~0 + spike_2001, mf$mics$obs)
                    )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
}

# if(iso3 == "ETH") {
#   tmb_int$par <- c(tmb_int$par,
#                    "beta_urban_dummy" = rep(0, ncol(mf$Z$X_urban_dummy))
#   )
# }

f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                                parameters = tmb_int$par,
                                DLL = "tmb_nb_spike",
                                silent=0,
                                # map = list(
                                #   log_prec_rw_tips = factor(NA),
                                #   log_prec_omega1 = factor(NA),
                                #   lag_logit_omega1_phi_age = factor(NA),
                                #   log_prec_omega2 = factor(NA),
                                #   lag_logit_omega2_phi_period = factor(NA),
                                #   log_prec_spatial = factor(NA),
                                #   log_prec_eta1 = factor(NA),
                                #   lag_logit_eta1_phi_age = factor(NA),
                                #   lag_logit_eta1_phi_period = factor(NA),
                                #   log_prec_eta2 = factor(NA),
                                #   lag_logit_eta2_phi_period = factor(NA),
                                #   log_prec_eta3 = factor(NA),
                                #   lag_logit_eta3_phi_age = factor(NA),
                                #   log_prec_rw_age = factor(NA),
                                #   log_prec_rw_period = factor(NA)
                                # ),
                                checkParameterOrder=FALSE)
})

parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                  parameters = tmb_int$par,
                  DLL = "tmb_nb_spike",
                  random = tmb_int$random,
                  # map = list(
                  #   log_prec_rw_tips = factor(NA),
                  #   log_prec_omega1 = factor(NA),
                  #   lag_logit_omega1_phi_age = factor(NA),
                  #   log_prec_omega2 = factor(NA),
                  #   lag_logit_omega2_phi_period = factor(NA),
                  #   # log_prec_spatial = factor(NA),
                  #   # log_prec_eta1 = factor(NA),
                  #   # lag_logit_eta1_phi_age = factor(NA),
                  #   # lag_logit_eta1_phi_period = factor(NA),
                  #   # log_prec_eta2 = factor(NA),
                  #   # lag_logit_eta2_phi_period = factor(NA),
                  #   # log_prec_eta3 = factor(NA),
                  #   # lag_logit_eta3_phi_age = factor(NA),
                  #   log_prec_rw_age = factor(NA),
                  #   log_prec_rw_period = factor(NA)
                  # ),
                  hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=FALSE)

fit$sdreport <- sdreport(fit$obj)

tmb_results <- dfertility::tmb_outputs(fit, mf, areas) 

write_csv(tmb_results, paste0(tolower(iso3), "_fr.csv"))

fr_plot <- read.csv(paste0("depends/", tolower(iso3), "_fr_plot.csv"))

fr_plot <- fr_plot %>%
  left_join(areas %>% st_drop_geometry() %>% select(area_id, area_name))

tfr_plot <- tmb_results %>%
  filter(area_level == 1, variable == "tfr") %>%
  ggplot(aes(x=period, y=median)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", value <10), aes(y=value, color=survey_id)) +
    facet_wrap(~area_name, ncol=5) +
    labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_tfr_admin1.pdf"), h = 12, w = 20)
tfr_plot
dev.off()
