library(tidyverse)

iso3_easy <- c("COG", "BDI", "BEN", "BFA", "CIV", "CMR", "KEN", "LSO", "MLI", "MWI",  "SLE", "SWZ", "TCD",
               "TGO", "ZWE", "AGO",  "GAB", "GHA", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA")

lapply(iso3_easy, function(x){
  orderly::orderly_pull_archive("aaa_model_selection", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "fertility", recursive = FALSE)
})

id <- lapply(iso3_easy, function(x){
  orderly::orderly_search(name = "aaa_model_selection", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

selection <- paste0("archive/aaa_model_selection/", id, "/fr.csv") %>%
  lapply(read.csv)

id <- lapply(iso3_easy, function(x){
  orderly::orderly_search(name = "aaa_inputs_orderly_pull", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

plot <- paste0("archive/aaa_inputs_orderly_pull/", id, "/fertility_fr_plot.csv") %>%
  lapply(read.csv) %>%
  bind_rows()

selection_df <- selection %>%
  bind_rows() %>%
  filter(variable == "asfr") %>%
  left_join(read.csv("global/iso_mapping_fit.csv") %>%
              select(iso3, admin1_level)) %>%
  filter(area_level == admin1_level) %>%
  select(iso3, area_id, period, age_group, source, lower, median, upper)

selection_l <- selection_df %>%
  group_by(iso3) %>%
  group_split()

plot_l <- plot %>%
  filter(variable == "asfr") %>%
  select(iso3, area_id, period, age_group, value) %>%
  group_by(iso3) %>%
  group_split()

score_df <- Map(function(selection, plot) {
  max_year <- max(plot$period)
  selection %>%
    filter(period %in% c((max_year - 5):max_year)) %>%
    left_join(plot) %>%
    mutate(below_lower = value < lower,
           above_upper = value > upper,
           int_diff = log(upper) - log(lower),
           penalty_lower = 2/0.05 * (log(lower) - value) * as.integer(below_lower),
           penalty_upper = 2/0.05 * (value - log(upper)) * as.integer(above_upper),
           total = int_diff + penalty_lower + penalty_upper
    )
}, selection_l, plot_l)

score_df %>%
  bind_rows() %>%
  filter(!is.na(total)) %>%
  select(iso3:value, total) %>%
  group_by(source) %>%
  summarise(total = sum(total))

selection_tfr <- selection %>%
  bind_rows() %>%
  filter(variable == "tfr") %>%
  left_join(read.csv("global/iso_mapping_fit.csv") %>%
              select(iso3, admin1_level)) %>%
  filter(area_level == admin1_level) %>%
  select(iso3, area_id, period, age_group, source, lower, median, upper)

p_ar <- Map(function(dat, plot) {
  
  dat %>%
    filter(!str_detect(source, "rw")) %>%
    ggplot(aes(x=period, y= median)) +
    geom_line(aes(color=source), size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
    geom_point(data=plot %>% filter(variable == "tfr", value < 10), aes(y=value)) +
    facet_wrap(~area_id)
  
}, selection_tfr %>% group_by(iso3) %>% group_split(), plot %>% group_by(iso3) %>% group_split())

##############

plot <- plot %>%
  filter(variable == "tfr")

selection <- selection %>%
  bind_rows() %>%
  filter(variable == "tfr") %>%
  left_join(read.csv("global/iso_mapping_fit.csv") %>%
              select(iso3, admin1_level)) %>%
  filter(area_level == admin1_level)

selection <- selection %>%
  group_by(iso3) %>%
  group_split

plot <- plot %>%
  group_by(iso3) %>%
  group_split

p_rw <- Map(function(dat, plot) {
  
  dat %>%
    filter(str_detect(source, "rw")) %>%
    ggplot(aes(x=period, y= median)) +
      geom_line(aes(color=source), size=1) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
      geom_point(data=plot %>% filter(variable == "tfr", value < 10), aes(y=value)) +
      facet_wrap(~area_id)
  
}, selection, plot)

p_ar <- Map(function(dat, plot) {
  
  dat %>%
    filter(!str_detect(source, "rw")) %>%
    ggplot(aes(x=period, y= median)) +
    geom_line(aes(color=source), size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
    geom_point(data=plot %>% filter(variable == "tfr", value < 10), aes(y=value)) +
    facet_wrap(~area_id)
  
}, selection, plot)

p_comp <- Map(function(dat, plot) {
  
  dat %>%
    filter(source %in% c("AR1 + trend", "ARIMA(1,1,0) + trend")) %>%
    ggplot(aes(x=period, y= median)) +
    geom_point(data=plot %>% filter(variable == "tfr", value < 10), aes(y=value)) +
    geom_line(aes(color=source), size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
    
    facet_wrap(~area_id)
  
}, selection, plot)

p_comp
