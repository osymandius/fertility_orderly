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

orderly_pull_oli("aaa_model_selection", c("TGO", "SLE", "SEN", "NER", "MLI", "LBR", "GIN", "GHA", "CIV", "TCD", "CMR", "BDI", "BFA", "BEN", "ZWE", "ZMB", "TZA", "UGA", "ZAF", "NAM", "MOZ", "LSO", "KEN", "SWZ", "AGO"))
dat <- get_latest_selection(c("TGO", "SLE", "SEN", "NER", "MLI", "LBR", "GIN", "GHA", "CIV", "TCD", "CMR", "BDI", "BFA", "BEN", "ZWE", "ZMB", "TZA", "UGA", "ZAF", "NAM", "MOZ", "LSO", "KEN", "SWZ", "AGO"))

selec <- dat$dat %>%
  mutate(within_95 = as.integer(quant_pos %in% 25:975)) %>%
  group_by(source) %>%
  summarise(coverage = sum(within_95)/n()) %>%
  left_join(
    dat$dat %>%
      filter(lower != 0,
             # upper != 0,
             survey_tfr != 0) %>%
      mutate(diff = log(upper) - log(lower),
             pen_lower = ifelse(survey_tfr < lower, 40*(log(lower) - log(survey_tfr)), 0),
             pen_upper = ifelse(survey_tfr > upper, 40*(log(survey_tfr) - log(upper)), 0),
             pen_10 = ifelse(upper > 10, log(upper), 0),
             score = diff + pen_lower + pen_upper + pen_10
      ) %>%
      group_by(source) %>%
      summarise(score = sum(score))
  ) %>%
  left_join(
    dat$crps %>%
      mutate(idx = row_number()) %>%
      pivot_longer(-idx, names_to = "source") %>%
      group_by(source) %>%
      summarise(crps = sum(value))
  ) %>%
  left_join(
    dat$elpd %>%
      mutate(idx = rep(c(1,2), 150)) %>%
      filter(idx == 1) %>%
      group_by(source) %>%
      summarise(elpd = sum(Estimate))
  ) %>%
  write_csv("~/Downloads/selection.csv")

dat$fr %>%
  filter(source %in% c("AR1", "rw2"),
         area_level ==0,
         variable == "tfr") %>%
  ggplot(aes(x=period, y=median)) +
    geom_line(aes(colour=source)) +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=source), alpha=.3) +
    facet_wrap(~area_name, scales="free")
