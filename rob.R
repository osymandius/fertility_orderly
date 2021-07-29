install.packages(c("purrr","tidyverse"))

devtools::install_github("vimc/orderly")
devtools::install_github("vimc/orderly.sharepoint")
devtools::install_github("reside-ic/spud")
devtools::install_github("osymandius/dfertility")
devtools::install_github("mrc-ide/naomi")

library(tidyverse)
library(purrr)
library(orderly)
library(orderly.sharepoint)
library(spud)

iso <- c("BDI", "BEN", "BFA", "CIV", "CMR", "COD", "COG", "GMB", "KEN", "LSO", "MLI", "MOZ", "MWI", "NGA", "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO", "ETH", "GAB", "GHA", "GIN", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

lapply(iso, function(x){
  orderly_pull_archive("aaa_scale_pop", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "fertility")
  orderly_pull_archive("aaa_orderly_inputs_pull", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "fertility")
})

safe_run <- safely(.f = orderly_run, otherwise = "Error")

id <- map(iso, ~safe_run(name = "aaa_fit", parameters = data.frame(iso3 = .x)))

id <- map(id, "result")

hyper_sd_report <- lapply(id, function(x) {
  utils::read.csv(paste0("draft/aaa_fit/", x, "/sd_report.csv"))
}) %>%
  bind_rows()

utils::write.csv(hyper_sd_report, "hyper_sd_report.csv")