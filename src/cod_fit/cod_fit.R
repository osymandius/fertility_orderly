iso3 <- "COD"

# convert_age_groups <- function(df) {
#   
#   df %>%
#     left_join(get_age_groups() %>%
#                 select(age_group, age_group_label) %>%
#                 rename(age_group_hold = age_group), 
#               by=c("age_group" = "age_group_label")) %>%
#     select(-age_group) %>%
#     rename(age_group = age_group_hold)
#   
# }

population <- read.csv(paste0("depends/", tolower(iso3), "_population_local.csv"))
areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson")) %>%
  mutate(iso3 = iso3)
asfr <- read.csv(paste0("depends/", tolower(iso3), "_asfr.csv"))

# population <- read.csv("archive/cod_data_population-local/20210526-082659-4cc83e7f/cod_population_local.csv")
# areas <- read_sf("archive/cod_data_areas/20201112-171234-2710c17f/cod_areas.geojson") %>%
#   mutate(iso3 = iso3)
# asfr <- read.csv("archive/cod_asfr/20201203-165704-0ceacdae/cod_asfr.csv")

# population <- convert_age_groups(population)
# areas_wide <- spread_areas(areas)
# 
# # fil_area_id1 <- paste0("COD_1_", c(1:20, 22:26)) # 
# 
# ## Some COD areas have 0 population. Remove. 
# pop_check <- population %>%
#   left_join(areas_wide %>% st_drop_geometry()) %>%
#   filter(area_id1 == "COD_1_21", sex == "female") %>%
#   group_by(area_id, calendar_quarter) %>%
#   summarise(population = sum(population)) %>%
#   ungroup %>%
#   filter(population <1) %>%
#   .$area_id %>%
#   unique
# 
# # fil_area_id2 <- paste0("COD_2_", c(125:127)) # No
# # fil_area_id2 <- paste0("COD_2_", c(128:130)) # 
# 
# area_id2 <- areas_wide %>%
#   filter(area_id1 %in% fil_area_id1) %>%
#   # filter(area_id1 == "COD_1_20") %>%
#   .$area_id2 %>%
#   unique()
# 
# area_id3 <- areas_wide %>%
#   filter(area_id1 %in% fil_area_id1) %>%
#   .$area_id3 %>%
#   unique()
# 
# areas <- areas %>%
#   # filter(!area_id %in% c(fil_area_id2, area_id3))
#   filter(area_id %in% c("COD", fil_area_id1, area_id2, area_id3))
# 
# # areas <- areas %>%
# #   filter(area_id != "COD_1_20",
# #          !area_id %in% area_id2,
# #          !area_id %in% area_id3
# #   )
# 
# asfr <- asfr %>%
#   # filter(area_id %in% unique(areas$area_id))
#   filter(area_id %in% areas$area_id)
# 
# # fr_plot <- read.csv("archive/cod_asfr/20201203-165704-0ceacdae/cod_fr_plot.csv")
# # 
# # fr_plot %>%
# #   filter(variable == "tfr", value <10) %>%
# #   ggplot(aes(x=period, y=value,color=survey_id)) +
# #     geom_point() +
# #     facet_wrap(~area_id)

# debugonce(make_model_frames_dev)
mf <- make_model_frames_dev(iso3, population, asfr,  areas, naomi_level =3, project=2020)

# mf$mf_model <- mf$mf_model %>%
#   left_join(areas_wide %>% select(area_id, area_id1, area_id2) %>% st_drop_geometry()) %>%
#   mutate(id.interaction2_admin1 = factor(group_indices(., period, area_id1)),
#          id.interaction3_admin1 = factor(group_indices(., age_group, area_id1)),
#          id.interaction2_admin2 = factor(group_indices(., period, area_id2)),
#          id.interaction3_admin2 = factor(group_indices(., age_group, area_id2))
#          )

TMB::compile("resources/parallel.cpp", flags = "-w")               # Compile the C++ file
dyn.load(dynlib("resources/parallel"))


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
  # Z_interaction2 = sparse.model.matrix(~0 + id.interaction2_admin2, mf$mf_model),
  Z_interaction3 = sparse.model.matrix(~0 + id.interaction3, mf$mf_model),
  # Z_interaction3 = sparse.model.matrix(~0 + id.interaction3_admin2, mf$mf_model),
  Z_country = mf$Z$Z_country,
  Z_omega1 = sparse.model.matrix(~0 + id.omega1, mf$mf_model),
  Z_omega2 = sparse.model.matrix(~0 + id.omega2, mf$mf_model),
  R_tips = mf$R$R_tips,
  R_age = mf$R$R_age,
  R_period = make_rw_structure_matrix(ncol(mf$Z$Z_period), 1, adjust_diagonal = TRUE),
  R_spatial = mf$R$R_spatial,
  # R_spatial_admin1 = make_adjacency_matrix(areas, model_level = 2),
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
  
  n_threads = 8
  
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
  lag_logit_phi_arima_period = 0,
  beta_period = 0,
  
  u_spatial_str = rep(0, ncol(mf$Z$Z_spatial)),
  log_prec_spatial = 0,
  
  beta_spike_2000 = 0,
  beta_spike_1999 = 0,
  beta_spike_2001 = 0,
  # log_overdispersion = 0,
  
  eta1 = array(0, c(ncol(mf$Z$Z_country), ncol(mf$Z$Z_period), ncol(mf$Z$Z_age))),
  log_prec_eta1 = 0,
  lag_logit_eta1_phi_age = 0,
  lag_logit_eta1_phi_period = 0,
  #
  eta2 = array(0, c(length(filter(areas, area_level == 3)$area_id), ncol(mf$Z$Z_period))),
  log_prec_eta2 = 0,
  lag_logit_eta2_phi_period = 0,
  #
  eta3 = array(0, c(length(filter(areas, area_level == 3)$area_id), ncol(mf$Z$Z_age))),
  log_prec_eta3 = 0,
  lag_logit_eta3_phi_age = 0
)

tmb_int$random <- c("beta_0",
                    "u_spatial_str",
                    "u_age",
                    "u_period",
                    "beta_period",
                    "beta_tips_dummy",
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
  )
  tmb_int$par <- c(tmb_int$par,
                   "u_tips_mics" = list(rep(0, ncol(mf$Z$Z_tips_mics)))
  )
  tmb_int$random <- c(tmb_int$random, "u_tips_mics")
}


# f <- parallel::mcparallel({TMB::MakeADFun(data = tmb_int$data,
#                                           parameters = tmb_int$par,
#                                           random = tmb_int$random,
#                                           DLL = "parallel")
# })
# 
# parallel::mccollect(f)

obj <-  TMB::MakeADFun(data = tmb_int$data,
                       parameters = tmb_int$par,
                       DLL = "parallel",
                       random = tmb_int$random,
                       hessian = FALSE)

f <- stats::nlminb(obj$par, obj$fn, obj$gr)
f$par.fixed <- f$par
f$par.full <- obj$env$last.par

fit <- c(f, obj = list(obj))
# fit$sdreport <- sdreport(fit$obj, fit$par)

class(fit) <- "naomi_fit"  # this is hacky...
fit <- naomi::sample_tmb(fit, random_only=TRUE)

tmb_results <- dfertility::tmb_outputs(fit, mf, areas)

# asfr_qtls <- apply(fit$sample$lambda_out, 1, quantile, c(0.025, 
#                                                          0.5, 0.975), na.rm=TRUE)
# 
# tfr_qtls <- apply(fit$sample$tfr_out, 1, quantile, c(0.025, 
#                                                      0.5, 0.975), na.rm=TRUE)
# 
# tmb_results <- mf$out$mf_out %>% 
#   dplyr::mutate(lower = c(asfr_qtls[1,], tfr_qtls[1, ]), 
#                 median = c(asfr_qtls[2, ], tfr_qtls[2, ]), 
#                 upper = c(asfr_qtls[3, ], tfr_qtls[3, ]), 
#                 source = "tmb") %>% 
#   utils::type.convert() %>% 
#   dplyr::left_join(areas %>% st_drop_geometry())

write_csv(tmb_results, paste0(tolower(iso3), "_fr.csv"))

fr_plot <- read.csv(paste0("depends/", tolower(iso3), "_fr_plot.csv"))
# fr_plot <- read.csv("archive/cod_asfr/20201203-165704-0ceacdae/cod_fr_plot.csv")

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

district_tfr <- tmb_results %>%
  filter(area_level == 3, variable == "tfr") %>%
  ggplot(aes(x=period, y=median)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
  facet_wrap(~area_name, ncol=8) +
  theme_minimal() +
  labs(y="TFR", x=element_blank(), title=paste(iso3, "| District TFR"))

dir.create("check")
pdf(paste0("check/", tolower(iso3), "_tfr_admin1.pdf"), h = 12, w = 20)
tfr_plot
dev.off()
pdf(paste0("check/", tolower(iso3), "_tfr_district.pdf"), h = 12, w = 20)
district_tfr
dev.off()