library(tidyverse)
library(orderly)
library(countrycode)
library(purrr)
library(moz.utils)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode::countrycode(ssa_names, "country.name", "iso3c")

possibly_run <- purrr::possibly(.f = orderly_run, otherwise = NULL)
possibly_pull <- purrr::possibly(.f = orderly_pull_archive, otherwise = NA)

remaining_iso3 <- ssa_iso3[!ssa_iso3 %in% names(dat) & !ssa_iso3 %in% c("BWA", "ETH", "COD", "ZAF", "CAF")]

res <- orderly_batch("aaa_fit", data.frame(iso3 = ssa_iso3), continue_on_error = )

id <- map(c("UGA", "CMR", "CIV", "ZWE", "RWA", "LSO", "SWZ", "MWI", "TZA", "ZMB"), ~possibly_run("aaa_fit", parameters = data.frame(iso3 = .x)))
id <- map(ssa_iso3[!ssa_iso3 %in% c("BWA", "ETH", "COD", "ZAF", "CAF")], ~possibly_run("aaa_fit", parameters = data.frame(iso3 = .x)))
lapply(id %>% compact(), orderly_commit)

orderly_dev_start_oli("aaa_assign_populations", data.frame(iso3 = "SLE"))

id <-  orderly_batch("aaa_fit", data.frame(iso3 = ssa_iso3[!ssa_iso3 %in% names(dat) & !ssa_iso3 %in% c("BWA", "ETH", "COD", "ZAF", "CAF")]))

tsks <- list.files("src", pattern = "asfr") %>%
  lapply(grep, pattern  = "nga", invert = TRUE, value = TRUE) %>%
  unlist()

id <- map(tsks, ~possibly_run(.x))
names(id) <- tsks

lapply(id %>% compact(), orderly_commit)

fail_id <- id %>%
  keep(is.null)

fail### SEARCH
orderly::orderly_search(name = 'aaa_inputs_orderly_pull', query = paste0('latest(parameter:iso3 == "', "GNB", '")'), draft = FALSE)
orderly::orderly_search(name = 'aaa_inputs_orderly_pull', query = paste0('latest(parameter:iso3 == "', "GNB", '")'), draft = FALSE)

id <- lapply(c(ssa_iso3), function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

map(ssa_iso3, ~possibly_pull("aaa_model_selection", id = paste0('latest(parameter:iso3 == "', .x, '")'), recursive = FALSE, remote = "real"))

 map(ssa_iso3[!ssa_iso3 %in% c("BWA", "ETH", "MOZ", "ZAF", "SSD", "UGA", "CAF", "COD", "GNQ", "GMB", "GIN", "GNB")], ~possibly_pull("aaa_inputs_orderly_pull", id = paste0('latest(parameter:iso3 == "', .x, '")'), recursive = FALSE, remote = "real"))
map(c("MOZ", "UGA", "CAF", "COD", "GNQ", "GMB", "GIN", "GNB"), ~possibly_pull("aaa_inputs_orderly_pull", id = paste0('latest(parameter:iso3 == "', .x, '")'), recursive = FALSE, remote = "real"))

dat <- lapply(paste0("archive/aaa_fit/", id[!is.na(id)], "/fr.csv"), read_csv, show_col_types = FALSE)
dat <- dat %>% bind_rows()

write_csv(dat, "~/Downloads/default_fr.csv")

possibly_remote <- purrr::possibly(.f = orderly_run_remote, otherwise = NULL)
map(ssa_iso3, ~possibly_remote("aaa_data_population_worldpop", parameters = list(iso3 = .x)))
orderly_run_remote("aaa_data_population_worldpop", parameters = list(iso3 = "AGO"))


map(grep("phia", list.files("src", "survey"), invert = TRUE, value = TRUE), ~possibly_remote(.x))
