library(tidyverse)

iso3_easy <- c("COG", "BDI", "BEN", "BFA", "CIV", "CMR", "KEN", "LSO", "MLI", "MWI",  "SLE", "SWZ", "TCD", "TGO", "ZWE", "AGO",  "GAB", "GHA", "LBR", "NAM", "NER", "RWA", "SEN", "TZA", "UGA", "ZMB")

selection <- list.files("archive/aaa_model_selection", full.names = TRUE) %>%
  list.files(pattern = "fr", full.names = TRUE) %>%
  lapply(read.csv)

id <- lapply(iso3_easy, function(x){
  orderly::orderly_search(name = "aaa_inputs_orderly_pull", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

unlist(id)

plot <- paste0("archive/aaa_inputs_orderly_pull/", unlist(id), "/fertility_fr_plot.csv") %>%
  lapply(read.csv) %>%
  bind_rows()

plot <- plot %>%
  filter(variable == "asfr")

selection <- selection %>%
  bind_rows() %>%
  filter(variable == "asfr") %>%
  select(iso3, area_id, period, age_group, lower, median, upper)

plot <- plot %>%
  select(iso3, area_id, period, age_group, value)
