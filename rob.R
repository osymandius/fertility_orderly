library(tidyverse)
library(purrr)
library(orderly)
library(spud)

iso <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COG", "GMB", "KEN", "LSO", "MLI", "MWI", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")
iso <- c("GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")
iso <- c("KEN", "MLI", "NER", "RWA", "ZMB")

lapply(iso, function(x){
  orderly_search(paste0('latest(parameter:iso3 == "', x, '")'), name = "aaa_inputs_orderly_pull")
})

orderly_search("parameter:iso3 == 'KEN'", "aaa_orderly_inputs_pull")

lapply(iso, function(x){
  orderly_pull_archive("aaa_scale_pop", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "fertility")
  orderly_pull_archive("aaa_inputs_orderly_pull", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "fertility")
})

safe_run <- safely(.f = orderly_run, otherwise = NULL, quiet = FALSE)
safe_read <- safely(.f = read_csv, otherwise = NULL, quiet = FALSE)

id <- map(iso, ~safe_run(name = "aaa_fit", parameters = data.frame(iso3 = .x)))

orderly_run("aaa_fit", parameters = data.frame(iso3 = "UGA"))

id <- map(id, "result") %>% unlist()

sd_report <- lapply(id, function(x) {
      safe_read(paste0("draft/aaa_fit/", x, "/sd_report.csv"))
    }) %>%
      map("result") %>%
      bind_rows()
  )

utils::write.csv(hyper_sd_report, "hyper_sd_report.csv")

# read_csv("draft/aaa_fit/20210804-113716-a9b86051/fr.csv") %>%
tmb_results %>%
  filter(area_level == 1, variable == "tfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
    geom_point(data = read_csv("~/Documents/GitHub/fertility_orderly/draft/aaa_fit/20210804-105108-7ed9299c/depends/fertility_fr_plot.csv") %>% filter(variable == "tfr", value <10), aes(y=value, color=survey_id)) +
    facet_wrap(~area_id, ncol=5) +
    labs(y="TFR", x=element_blank(), color="Survey ID") +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      text = element_text(size=14)
    )

read_csv("draft/aaa_fit/20210803-155251-0b960e67/sd_report.csv") %>%
  filter(hyper == "u_tips") %>%
  mutate(idx = row_number()) %>%
  ggplot(aes(x=idx, y=Estimate)) +
    geom_line()

# 20210804-092005-9b5c5746 0.85 both ARIMA and Eta2
# 20210803-160543-fbba5ef6 0.85 ARIMA only
# 20210803-155251-0b960e67 0.85 eta2 only
# 20210804-095722-ac925725 0.99 eta2 only
# 20210804-100949-d735f6bd 0.85 eta2, no tips

orderly_develop_start("aaa_fit", parameters = data.frame(iso3 = "UGA"))
setwd("src/aaa_fit")
