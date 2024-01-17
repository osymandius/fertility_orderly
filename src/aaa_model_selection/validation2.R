library(tidyverse)

# get_latest_selection <- function(iso3) {
#   id <- lapply(iso3, function(x){
#     orderly::orderly_search(name = "aaa_model_selection", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
#   })
#   
#   out <- list()
#   
#   paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/pred.csv")
#   out$dat <- lapply(paths, read.csv) %>%
#     bind_rows()
#   
#   paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/crps.csv")
#   out$crps <- lapply(paths, read.csv) %>%
#     bind_rows()
#   
#   paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/elpd.csv")
#   out$elpd <- lapply(paths, read.csv) %>%
#     bind_rows()
#   
#   paths <- paste0(rprojroot::find_rstudio_root_file(), "/archive/aaa_model_selection/", id, "/fr.csv")
#   out$fr <- lapply(paths, read.csv) %>%
#     bind_rows()
#   
#   out
# 
# }
# 
# orderly_pull_oli("aaa_model_selection", c("TGO", "SLE", "SEN", "NER", "MLI", "LBR", "GIN", "GHA", "CIV", "TCD", "CMR", "BDI", "BFA", "BEN", "ZWE", "ZMB", "TZA", "UGA", "ZAF", "NAM", "MOZ", "LSO", "KEN", "SWZ", "AGO"))
# dat <- get_latest_selection(c("TGO", "SLE", "SEN", "NER", "MLI", "LBR", "GIN", "GHA", "CIV", "TCD", "CMR", "BDI", "BFA", "BEN", "ZWE", "ZMB", "TZA", "UGA", "ZAF", "NAM", "MOZ", "LSO", "KEN", "SWZ", "AGO"))

sel_dat <- list.files("outside_orderly/fit/outputs/selection", full.names = T, recursive = T, pattern = "ppd.csv") %>%
  lapply(read_csv, show_col_types = F)

elpd <- list.files("outside_orderly/fit/outputs/selection", full.names = T, recursive = T, pattern = "elpd") %>%
  lapply(read_csv, show_col_types = F)

names(elpd) <- str_extract(list.files("outside_orderly/fit/outputs/selection", full.names = T, recursive = T, pattern = "elpd"), "[A-Z]{3}")

elpd <- elpd %>%
  bind_rows(.id = "iso3")

elpd_crs <- elpd %>%
  select(-SE) %>%
  filter(indicator != "ic") %>%
  pivot_wider(names_from  = indicator, values_from = Estimate)

selection <- sel_dat %>%
  bind_rows() %>%
  mutate(within_95 = as.integer(quant_pos %in% 25:975)) %>%
  separate_survey_id(F) %>%
  group_by(iso3, source) %>%
  summarise(coverage = sum(within_95)/n()) %>%
  left_join(
    bind_rows(sel_dat) %>%
      separate_survey_id(F) %>%
      filter(lower != 0,
             # upper != 0,
             survey_tfr != 0) %>%
      mutate(diff = log(upper) - log(lower),
             pen_lower = ifelse(survey_tfr < lower, 40*(log(lower) - log(survey_tfr)), 0),
             pen_upper = ifelse(survey_tfr > upper, 40*(log(survey_tfr) - log(upper)), 0),
             pen_10 = ifelse(upper > 10, log(upper), 0),
             score = diff + pen_lower + pen_upper + pen_10
      ) %>%
      group_by(iso3, source) %>%
      summarise(score = sum(score))
  ) %>%
  left_join(
    elpd_crs %>% mutate(source = str_replace(source, " ", "-"))
  )

df <- selection %>%
  filter(!iso3 %in% c("GHA", "AGO")) %>%
  group_by(source) %>%
  summarise(
    coverage = median(coverage),
    med_score = median(score),
    score = sum(score),
    med_elpd = median(elpd),
    elpd = sum(elpd),
    med_crps = median(crps),
    crps = sum(crps))

write_csv(df, "outside_orderly/fit/scripts/selection.csv")

ken_dat <- lapply(list.files("outside_orderly/fit/outputs/selection", recursive = T, full.names = T) %>%
  lapply(grep, pattern = "KEN", value = T) %>%
  unlist() %>%
  lapply(grep, pattern = "fr.csv", value = T) %>%
  unlist(),
  read_csv)

rwa_dat <- lapply(list.files("outside_orderly/fit/outputs/selection", recursive = T, full.names = T) %>%
                    lapply(grep, pattern = "RWA", value = T) %>%
                    unlist() %>%
                    lapply(grep, pattern = "fr.csv", value = T) %>%
                    unlist(),
                  read_csv)

names(ken_dat) <- c("AR1", "AR1 + trend", "ARIMA(1,1,0)", "ARIMA(1,1,0) + trend", "RW1", "RW1 + trend", "RW2", "RW2 + trend")
names(rwa_dat) <- names(ken_dat)

ken_asfr_plot <- readRDS("outside_orderly/asfr/outputs/KEN/KEN_fr_plot.rds")$national

sfig <- ken_dat %>%
  bind_rows(.id = "source") %>%
  filter(variable == "tfr", area_level == 0, period > 1999) %>%
  ggplot(aes(x=period, y=median)) +
    geom_line(aes(color = source), show.legend = F) +
    geom_point(data = ken_asfr_plot %>% filter(variable == "tfr", period > 1999, !survey_id %in% c("KEN2022DHS", "KEN2020MIS")), aes(y=value, size = pys)) +
    geom_point(data = ken_asfr_plot %>% filter(variable == "tfr", period > 1999, survey_id %in% c("KEN2022DHS", "KEN2020MIS")), aes(y=value, size = pys), color = "grey", show.legend = F) +
    ggrepel::geom_label_repel(data = . %>% filter(period == max(period)), aes(color =source, label = source), 
                              xlim = c(-Inf, Inf), nudge_x = 5, size=4, segment.linetype = 2, show.legend = FALSE, direction="y") +
    scale_x_continuous(breaks = c(2000, 2010, 2020)) +
    standard_theme() +
    coord_cartesian(clip="off", xlim=c(2000, 2020)) +
    theme(plot.margin = unit(c(1,9,1,0), "lines")) +
    labs(y="TFR", x=element_blank(), size = "Person-years", title = "Kenya")

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/selection_fig.png", width = 500, height = 500)
annotate_figure(sfig, bottom = "Grey points withheld from model calibration")
dev.off()
