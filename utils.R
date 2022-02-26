library(tidyverse)
library(orderly)
library(countrycode)
library(purrr)
library(moz.utils)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode(ssa_names, "country.name", "iso3c")

possibly_run <- purrr::possibly(.f = orderly_run, otherwise = NULL)
possibly_pull <- purrr::possibly(.f = orderly_pull_archive, otherwise = NA)


id <- map(ssa_iso3, ~possibly_run("aaa_assign_populations", parameters = data.frame(iso3 = .x)))
lapply(id %>% compact(), orderly_commit)

orderly_dev_start_oli("aaa_assign_populations", data.frame(iso3 = "SLE"))

### SEARCH
orderly::orderly_search(name = "aaa_assign_province", query = paste0('latest(parameter:iso3 == "', "ZAF", '")'), draft = FALSE)

