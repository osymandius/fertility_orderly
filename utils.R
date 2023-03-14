library(tidyverse)
library(orderly)
library(countrycode)
library(purrr)
library(moz.utils)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode::countrycode(ssa_names, "country.name", "iso3c")

possibly_run <- purrr::possibly(.f = orderly_run, otherwise = NULL)
possibly_pull <- purrr::possibly(.f = orderly_pull_archive, otherwise = NA)
possibly_push <- purrr::possibly(.f = orderly_push_archive, otherwise = NA)

remaining_iso3 <- ssa_iso3[!ssa_iso3 %in% names(dat) & !ssa_iso3 %in% c("BWA", "ETH", "COD", "ZAF", "CAF")]

res <- orderly_batch("aaa_fit", data.frame(iso3 = ssa_iso3), continue_on_error = )

id <- map(c("UGA", "CMR", "CIV", "ZWE", "RWA", "LSO", "SWZ", "MWI", "TZA", "ZMB"), ~possibly_run("aaa_fit", parameters = data.frame(iso3 = .x)))
id <- map(ssa_iso3[!ssa_iso3 %in% c("BWA", "ETH", "COD", "ZAF", "CAF", "NGA", "MWI", "RWA")], ~possibly_run("aaa_fit", parameters = data.frame(iso3 = .x)))
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
orderly::orderly_search(name = 'aaa_scale_pop', query = paste0('latest(parameter:iso3 == "', "MLI", '")'), draft = FALSE)
orderly::orderly_search(name = 'aaa_inputs_orderly_pull', query = paste0('latest(parameter:iso3 == "', "GNB", '")'), draft = FALSE)

id <- lapply("COD", function(x){
  orderly::orderly_search(name = "aaa_scale_pop", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

id <- map(c("COD", "TZA", "COG"), ~possibly_pull("aaa_fit", id = paste0('latest(parameter:iso3 == "', .x, '")'), recursive = FALSE))

orderly_pull_archive("aaa_scale_pop", id = paste0('latest(parameter:iso3 == "', "ETH", '")'), recursive = FALSE, remote = "real")

map(c("TZA"), ~possibly_pull("aaa_asfr", id = paste0('latest(parameter:iso3 == "', .x, '")'), recursive = FALSE))

map(c("RWA", "MWI"), ~possibly_pull("aaa_mwi_rwa_fit", id = paste0('latest(parameter:iso3 == "', .x, '")'), recursive = FALSE))

dat <- lapply(paste0("archive/aaa_fit/", id[!is.na(id)], "/fr.csv"), read_csv, show_col_types = FALSE)
dat <- dat %>% bind_rows()

write_csv(dat, "~/Downloads/default_fr.csv")

possibly_remote <- purrr::possibly(.f = orderly_run_remote, otherwise = NULL)
map(ssa_iso3, ~possibly_remote("aaa_data_population_worldpop", parameters = list(iso3 = .x)))
orderly_run_remote("aaa_data_population_worldpop", parameters = list(iso3 = "AGO"))


id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_data_population_worldpop", query = paste0('latest(parameter:iso3 == "', x, '" && parameter:version == 2021)'), draft = FALSE)
})

names(id) <- ssa_iso3

names(id %>% keep(~is.na(.x)))

lapply(id$id[id$success == TRUE], function(x) orderly_push_archive("aaa_data_population_worldpop", id = x, remote = "real"))

id <- map(paste0(tolower(ssa_iso3), "_survey"), ~possibly_run(.x))

map(ssa_iso3, ~possibly_push("aaa_data_population_worldpop", remote = "real"))

id <- map(ssa_iso3, ~possibly_pull("aaa_areas_pull", id = paste0('latest(parameter:iso3 == "', .x, '" && parameter:version == 2021)'), recursive = FALSE))