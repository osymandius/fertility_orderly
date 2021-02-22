tmb_results <- read_csv("tmb_results_validation.csv")

tmb_results <- tmb_results %>%
  separate(area_id, into=c("iso3", NA), sep=3, remove=FALSE)

fr <- bind_rows(read.csv("archive/mwi_asfr/20201203-173050-ef175986/mwi_fr_plot.csv"), 
          read.csv("archive/rwa_asfr/20201203-173655-d59fe41c/rwa_fr_plot.csv"),
          read.csv("archive/lso_asfr/20201203-170637-8a2584b9/lso_fr_plot.csv"),
          read.csv("archive/moz_asfr/20201204-141436-7f9b7a9d/moz_fr_plot.csv"),
          read.csv("archive/ken_asfr/20201203-170212-d40cd696/ken_fr_plot.csv"),
          read.csv("archive/zwe_asfr/20201203-175137-77e29f2b/zwe_fr_plot.csv"),
          read.csv("archive/nam_asfr/20201203-173514-66f3cc83/nam_fr_plot.csv"),
          read.csv("archive/swz_asfr/20201203-174002-8fef11ee/swz_fr_plot.csv"),
          read.csv("archive/uga_asfr/20201203-174557-c3b8ee3a/uga_fr_plot.csv")
          
)

fr_unc <- fr %>%
  filter(variable == "tfr") %>%
  select(area_id, period, value)

res_unc <- tmb_results %>%
  filter(variable == "tfr", str_detect(area_id, "_1_")) %>%
  left_join(fr_unc) %>%
  select(area_id:area_level, value, -c(idx_row, variable, age_group)) %>%
  filter(!is.na(value)) %>%
  group_by(iso3) %>%
  filter(period %in% (max(period) - 5))

mse <- res_unc %>%
  mutate(sq_err = (value-median)^2) %>%
  group_by(iso3, area_id, source) %>%
  summarise(mse = mean(sq_err))

p1 <- mse %>%
  ggplot(aes(x=mse, y=iso3)) +
    geom_boxplot() +
    facet_wrap(~source, ncol = 1)
  

fr_l <- group_split(fr, iso3)          

tmb_l <- group_split(tmb_results, iso3)

pl <- Map(function(tmb_results, fr_plot) {
  
  iso3 <- unique(tmb_results$iso3)
  
  fr_plot <- fr_plot %>% left_join(select(tmb_results, area_id, area_name) %>% distinct)
  
  intercept <- max(fr_plot$period) - 5
  
  tmb_results %>%
    filter(area_level == 1, variable == "tfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(aes(color=source)) +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=0.3) +
    geom_point(data = fr_plot %>% filter(variable == "tfr", str_detect(area_id, "_1_"), value<12), aes(y=value)) +
    geom_vline(aes(xintercept = intercept), linetype = 3) +
    facet_wrap(~area_name, ncol=5) +
    labs(y="TFR", x=element_blank(), color="Survey ID", title=paste(iso3, "| Provincial TFR")) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )
  
  
}, tmb_l, fr_l)

pl[[1]]


res_intv_score <- res_unc %>%
  mutate(
    log_diff_intv = log(upper) - log(lower),
    greater_than_upper = as.integer(value > upper),
    less_than_lower = as.integer(value < lower),
    coeff_lower = (2/0.025)*(log(lower) - log(value))*less_than_lower,
    coeff_upper = (2/0.025)*(log(value) - log(upper))*greater_than_upper,
    score = log_diff_intv + coeff_lower + coeff_upper
  ) %>%
  group_by(iso3, area_id, source) %>%
  summarise(mean_score = mean(score))
    
p2 <- res_intv_score %>%
  ggplot(aes(x=mean_score, y=iso3)) +
      geom_boxplot() +
      labs(y=element_blank(), x="Mean interval score", title = "Interval Score") +
      facet_wrap(~source, ncol=1)

res_intv_score_nat <- res_unc %>%
  mutate(
    log_diff_intv = log(upper) - log(lower),
    greater_than_upper = as.integer(value > upper),
    less_than_lower = as.integer(value < lower),
    coeff_lower = (2/0.025)*(log(lower) - log(value))*less_than_lower,
    coeff_upper = (2/0.025)*(log(value) - log(upper))*greater_than_upper,
    score = log_diff_intv + coeff_lower + coeff_upper
  ) %>%
  group_by(iso3, source) %>%
  summarise(mean_score = mean(score))

res_intv_score_nat %>%
  ggplot(aes(x=mean_score, y=source)) +
    geom_boxplot()+
    labs(y=element_blank(), x="Mean interval score", title = "Interval Score")
