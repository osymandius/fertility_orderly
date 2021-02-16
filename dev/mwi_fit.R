library(Matrix)
library(tidyverse)
library(sf)
library(rdhs)
library(dfertility)
library(naomi)
devtools::load_all("~/Documents/GitHub/dfertility/")

iso3 <- "UGA"

population <- read.csv("archive/mwi_data_population/20201125-095927-2281c97e/mwi_population_census18.csv")
areas <- read_sf("archive/uga_data_areas/20201026-075718-2c4dfbae/uga_areas.geojson")
# asfr <- read.csv(paste0("depends/", tolower(iso3), "_asfr.csv"))

asfr <- read.csv("archive/uga_asfr/20201203-174557-c3b8ee3a/uga_dhs_asfr.csv")

asfr <- readRDS("~/Documents/GitHub/subnat_fertility/countries/MWI/data/MWI_asfr_admin1.rds")

# population <- population %>%
#   rename(age_group_label = age_group) %>%
#   left_join(get_age_groups() %>% select(age_group, age_group_label)) %>%
#   select(-age_group_label)

asfr <- asfr %>%
  left_join(get_age_groups() %>% select(age_group_label, age_group), by=c("age_group" = "age_group_label")) %>%
  select(-age_group) %>%
  rename(age_group = age_group.y) %>%
  filter(survtype == "DHS")

population <- read.csv("archive/uga_data_population/20201130-170229-8c69d8e4/uga_population_ubos.csv")

mf <- make_model_frames_dev(iso3, population, asfr,  areas, naomi_level =3, project=2020)

mf$observations$full_obs <- mf$observations$full_obs %>%
  mutate(id.smooth = factor(group_indices(., age_group, period, area_id, survey_id)))


iid <- diag(length(unique(mf$observations$full_obs$id.smooth)))
R_smooth_iid <- as(iid, "dgTMatrix")

Z_smooth_iid_dhs <-  sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype == "DHS"))
Z_smooth_iid_ais <-  sparse.model.matrix(~0 + id.smooth, mf$observations$full_obs %>% filter(survtype != "DHS"))




TMB::compile("global/tmb_all_level_iid.cpp")               # Compile the C++ file
dyn.load(dynlib("global/tmb_all_level_iid"))

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
  Z_spatial = mf$Z$Z_spatial,
  Z_interaction1 = sparse.model.matrix(~0 + id.interaction1, mf$mf_model),
  Z_interaction2 = sparse.model.matrix(~0 + id.interaction2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  Z_smooth_iid_dhs = Z_smooth_iid_dhs,
  Z_smooth_iid_ais = Z_smooth_iid_ais,
  # Z_smooth_iid_ais = Z_smooth_ais,
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = mf$R$R_period,
  R_spatial = mf$R$R_spatial,
  R_country = mf$R$R_country,
  R_smooth_iid = R_smooth_iid,
  rankdef_R_spatial = 1,
  
  log_offset_naomi = log(mf$observations$naomi_level_obs$pys),
  births_obs_naomi = mf$observations$naomi_level_obs$births,
  
  log_offset_dhs = log(filter(mf$observations$full_obs, survtype == "DHS")$pys),
  births_obs_dhs = filter(mf$observations$full_obs, survtype == "DHS")$births,
  
  log_offset_ais = log(filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$pys),
  births_obs_ais = filter(mf$observations$full_obs, survtype %in% c("AIS", "MIS"))$births,
  
  pop = mf$mf_model$population,
  # A_asfr_out = mf$out$A_asfr_out,
  # A_tfr_out = mf$out$A_tfr_out,
  
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
  
  u_smooth_iid = rep(0, ncol(R_smooth_iid)),
  log_prec_smooth_iid = 0,

  omega1 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_age))),
  log_prec_omega1 = 0,
  lag_logit_omega1_phi_age = 0,

  omega2 = array(0, c(ncol(mf$R$R_country), ncol(mf$Z$Z_period))),
  log_prec_omega2 = 0,
  lag_logit_omega2_phi_period = 0,
  
  u_period = rep(0, ncol(mf$Z$Z_period)),
  log_prec_rw_period = 0,
lag_logit_phi_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,

  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  log_overdispersion = 0,

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

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "beta_tips_dummy",
                    "u_tips",
                    "u_smooth_iid",
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
)
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
                               parameters = tmb_int$par,
                               DLL = "tmb_all_level_iid",
                               silent=0,
                               checkParameterOrder=FALSE)
})

parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                  parameters = tmb_int$par,
                  DLL = "tmb_all_level_iid",
                  random = tmb_int$random,
                  hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

qtls1 <- apply(fit$sample$lambda, 1, quantile, c(0.025, 0.5, 0.975))

u_iid <- apply(fit$sample$u_smooth_iid, 1, quantile, 0.5)


# fr_plot <- fr_plot %>%
#   left_join(areas %>% st_drop_geometry() %>% select(area_id, area_name)) %>%
#   filter(str_detect(area_id, "MWI_1_"))
# 
# tfr_plot <- read_csv("archive/uga_asfr/20201203-174557-c3b8ee3a/uga_fr_plot.csv")
# fr <- read.csv("archive/uga_fit/20201204-000940-0c4e8738/uga_fr.csv")

mf$mf_model %>%
  mutate(lower = qtls1[1,],
         median = qtls1[2,],
         upper = qtls1[3,]
  ) %>%
  group_by(area_id, period) %>%
  summarise(median = 5*sum(median)) %>%
  mutate(source = "iid") %>%  
  type.convert() %>%
  bind_rows(fr %>%
              filter(variable == "tfr", area_level == 1) %>%
              mutate(source = "No iid") %>%
              type.convert()) %>%
  ungroup() %>%
  ggplot(aes(x=period,y=median)) +
  geom_line(aes(group=source, color=source))+
  geom_point(data=tfr_plot %>% filter(variable == "tfr", value<10), aes(y=value)) +
  facet_wrap(~area_id)




# tmb_results <- dfertility::tmb_outputs(fit, mf, areas) 

# write_csv(tmb_results, paste0(tolower(iso3), "_fr.csv"))



tmb_results %>%
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

###############


df <- data.frame(x=1:100) %>%
  mutate(y=jitter(-2*x+4, factor = 100))

plot(df)
