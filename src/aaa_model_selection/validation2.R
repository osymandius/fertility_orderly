library(tidyverse)

get_latest_selection <- function(iso3) {
  id <- lapply(iso3, function(x){
    orderly::orderly_search(name = "aaa_model_selection", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
  })
  
  out <- list()
  
  paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/pred.csv")
  out$dat <- lapply(paths, read.csv) %>%
    bind_rows()
  
  paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/crps.csv")
  out$crps <- lapply(paths, read.csv) %>%
    bind_rows()
  
  paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/elpd.csv")
  out$elpd <- lapply(paths, read.csv) %>%
    bind_rows()
  
  paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/fr.csv")
  out$fr <- lapply(paths, read.csv) %>%
    bind_rows()
  
  out

}

dat <- get_latest_selection(c("TGO", "SLE", "SEN", "NER", "MLI", "LBR", "GIN", "GHA", "CIV", "TCD", "CMR", "BDI", "BFA", "BEN", "ZWE", "ZMB", "TZA", "UGA", "ZAF", "NAM", "MOZ", "LSO", "KEN", "SWZ", "AGO"))

dat$dat %>%
  mutate(within_95 = as.integer(quant_pos %in% 25:975)) %>%
  group_by(source) %>%
  summarise(coverage = sum(within_95)/n())

dat$dat %>%
  filter(lower != 0,
         upper != 0,
         survey_tfr != 0) %>%
  mutate(diff = log(upper) - log(lower),
         pen_lower = ifelse(survey_tfr < lower, 40*(log(lower) - log(survey_tfr)), 0),
         pen_upper = ifelse(survey_tfr > upper, 40*(log(survey_tfr) - log(upper)), 0),
         pen_10 = ifelse(upper > 10, log(upper), 0),
         score = diff + pen_lower + pen_upper + pen_10
  ) %>%
  group_by(source) %>%
  summarise(score = sum(score))

dat$crps %>%
  mutate(idx = row_number()) %>%
  pivot_longer(-idx) %>%
  group_by(name) %>%
  summarise(value = sum(value))

dat$elpd %>%
  mutate(idx = rep(c(1,2), 150)) %>%
  filter(idx == 1) %>%
  group_by(source) %>%
  summarise(elpd = sum(Estimate))

fr %>%
  filter(area_level == 0,
         variable == "tfr",
         iso3 == "ZWE") %>%
  ggplot() +
    geom_line(aes(x=period, y=median, color=source)) +
    facet_wrap(~source)

dat %>%
  filter(lower == 0)
  distinct(area_id, period) 
