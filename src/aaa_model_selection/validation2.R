get_latest_selection <- function(iso3, return_list = FALSE) {
  id <- lapply(iso3, function(x){
    orderly::orderly_search(name = "aaa_model_selection", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
  })
  
  paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/pred.csv")
  dat <- lapply(paths, read.csv) %>%
    setNames(iso3)
  
  if(return_list)
    dat
  else
    dat %>%
    bind_rows()
}

get_latest_selection_fr <- function(iso3, return_list = FALSE) {
  id <- lapply(iso3, function(x){
    orderly::orderly_search(name = "aaa_model_selection", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
  })
  
  paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/fr.csv")
  dat <- lapply(paths, read.csv) %>%
    setNames(iso3)
  
  if(return_list)
    dat
  else
    dat %>%
    bind_rows()
}

dat <- get_latest_selection(c("TGO", "SLE", "SEN", "NER", "MLI", "LBR", "GIN", "GHA", "CIV", "TCD", "CMR", "BDI", "BFA", "BEN", "ZWE", "ZMB", "TZA", "UGA", "ZAF", "NAM", "MOZ", "LSO", "KEN", "SWZ", "AGO"))
fr <- get_latest_selection_fr(c("TGO", "SLE", "SEN", "NER", "MLI", "LBR", "GIN", "GHA", "CIV", "TCD", "CMR", "BDI", "BFA", "BEN", "ZWE", "ZMB", "TZA", "UGA", "ZAF", "NAM", "MOZ", "LSO", "KEN", "SWZ", "AGO"))

dat %>%
  mutate(within_95 = as.integer(quant_pos %in% 25:975)) %>%
  group_by(source) %>%
  summarise(coverage = sum(within_95)/n())

dat %>%
  filter(lower != 0,
         upper != 0,
         survey_tfr != 0) %>%
  mutate(diff = log(upper) - log(lower),
         pen_lower = ifelse(survey_tfr < lower, 40*(log(lower) - log(survey_tfr)), 0),
         pen_upper = ifelse(survey_tfr > upper, 40*(log(survey_tfr) - log(upper)), 0),
         score = diff + pen_lower + pen_upper
  ) %>%
  group_by(source) %>%
  summarise(score = sum(score))

fr %>%
  filter(area_level == 0,
         variable == "tfr",
         iso3 == "ZWE") %>%
  ggplot() +
    geom_line(aes(x=period, y=median, color=source)) +
    facet_wrap(~source)

dat %>%
  filter(survey_tfr == 0) %>%
  distinct(area_id, period) %>% View(

  )
