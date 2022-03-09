library(tidyverse)
library(orderly)
library(countrycode)
library(purrr)
library(moz.utils)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode(ssa_names, "country.name", "iso3c")

mapping <- read_csv("global/iso_mapping_fit.csv")

lapply(ssa_iso3, function(x){
  orderly::orderly_pull_archive("aaa_model_selection", id = paste0('latest(parameter:iso3 == "', x, '")'), remote = "fertility", recursive = FALSE)
})

selection_id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_model_selection", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})
names(selection_id) <- ssa_iso3
selection_id <- selection_id[!is.na(selection_id)]

selection <- lapply(paste0("archive/aaa_model_selection/", selection_id[!str_detect(selection_id, "2021")], "/fr.csv"), read_csv, show_col_types = FALSE) %>%
  setNames(names(selection_id[!str_detect(selection_id, "2021")]))
  

id <- lapply(ssa_iso3, function(x){
  orderly::orderly_search(name = "aaa_inputs_orderly_pull", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})
names(id) <- ssa_iso3

plot <- paste0("archive/aaa_inputs_orderly_pull/", id[!is.na(id)], "/fertility_fr_plot.csv") %>%
  lapply(read.csv)
names(plot) <- names(id[!is.na(id)])

# selection_df <- selection %>%
#   bind_rows() %>%
#   filter(variable == "asfr") %>%
#   left_join(read.csv("global/iso_mapping_fit.csv") %>%
#               select(iso3, admin1_level)) %>%
#   filter(area_level == admin1_level) %>%
#   select(iso3, area_id, period, age_group, source, lower, median, upper)

# selection_l <- selection_df %>%
#   group_by(iso3) %>%
#   group_split()
# 
# plot_l <- plot %>%
#   filter(variable == "asfr") %>%
#   select(iso3, area_id, period, age_group, value) %>%
#   group_by(iso3) %>%
#   group_split()

score_df <- Map(function(selection, plot, mapping) {
  
  iso3_c <- unique(plot$iso3)
  admin1_lvl <- filter(mapping, iso3 == iso3_c)$admin1_level
  
  selection %>%
    filter(period %in% 2016:max(period),
           area_level == admin1_lvl,
           variable == "asfr") %>%
    left_join(plot) %>%
    # mutate(below_lower = value < lower,
    #        above_upper = value > upper,
    #        int_diff = log(upper) - log(lower),
    #        penalty_lower = 2/0.05 * (log(lower) - value) * as.integer(below_lower),
    #        penalty_upper = 2/0.05 * (value - log(upper)) * as.integer(above_upper),
    #        total = int_diff + penalty_lower + penalty_upper
    # )
    mutate(below_lower = value < lower,
           above_upper = value > upper,
           upper_above_10 = upper >= 10,
           int_diff = lower-upper,
           penalty_lower = (log(lower) - value) * as.integer(below_lower),
           penalty_upper = (value - log(upper)) * as.integer(above_upper),
           penalty_10 = ifelse(upper_above_10 == TRUE, log(upper), 0),
           total = int_diff + penalty_lower + penalty_upper - penalty_10
    )
}, selection, plot[names(selection)], list(mapping))

score_df %>%
  bind_rows() %>%
  filter(!is.na(total)) %>%
  select(iso3, area_id, area_name, period, age_group, source, lower:upper, total) %>%
  group_by(source) %>%
  summarise(total = sum(total))

selection_tfr <- selection %>%
  bind_rows() %>%
  filter(variable == "tfr") %>%
  left_join(read.csv("global/iso_mapping_fit.csv") %>%
              select(iso3, admin1_level)) %>%
  filter(area_level == admin1_level) %>%
  select(iso3, area_id, period, age_group, source, lower, median, upper)

p_ar <- Map(function(dat, plot) {
  
  dat %>%
    filter(!str_detect(source, "rw")) %>%
    ggplot(aes(x=period, y= median)) +
    geom_line(aes(color=source), size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
    geom_point(data=plot %>% filter(variable == "tfr", value < 10), aes(y=value)) +
    facet_wrap(~area_id)
  
}, selection_tfr %>% group_by(iso3) %>% group_split(), plot %>% group_by(iso3) %>% group_split())

##############

plot <- plot %>%
  filter(variable == "tfr")

selection <- selection %>%
  bind_rows() %>%
  filter(variable == "tfr") %>%
  left_join(read.csv("global/iso_mapping_fit.csv") %>%
              select(iso3, admin1_level)) %>%
  filter(area_level == admin1_level)

selection <- selection %>%
  group_by(iso3) %>%
  group_split

plot <- plot %>%
  group_by(iso3) %>%
  group_split

p_rw <- Map(function(dat, plot) {
  
  iso3_c <- unique(plot$iso3)
  admin1_lvl <- filter(mapping, iso3 == iso3_c)$admin1_level
  
  plot <- plot %>%
    separate(area_id, into=c(NA, "area_level", NA), sep=c(4,5), convert=TRUE, remove=FALSE) %>%
    filter(variable == "tfr",
           value < 10,
           area_level == admin1_lvl)
  
  dat %>%
    filter(str_detect(source, "rw"),
           variable == "tfr",
           area_level == admin1_lvl,) %>%
    ggplot(aes(x=period, y= median)) +
      geom_line(aes(color=source), size=1) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
      geom_point(data=plot, aes(y=value)) +
      facet_wrap(~area_id)
  
}, selection, plot[names(selection)])

p_ar <- Map(function(dat, plot) {
  
  iso3_c <- unique(plot$iso3)
  admin1_lvl <- filter(mapping, iso3 == iso3_c)$admin1_level
  
  plot <- plot %>%
    separate(area_id, into=c(NA, "area_level", NA), sep=c(4,5), convert=TRUE, remove=FALSE) %>%
    filter(variable == "tfr",
           value < 10,
           area_level == admin1_lvl)
  
  dat %>%
    filter(!str_detect(source, "rw"),
           variable == "tfr",
           area_level == admin1_lvl,) %>%
    ggplot(aes(x=period, y= median)) +
    geom_line(aes(color=source), size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
    geom_point(data=plot, aes(y=value)) +
    facet_wrap(~area_id)
  
}, selection, plot[names(selection)])

p_comp <- Map(function(dat, plot) {
  
  iso3_c <- unique(plot$iso3)
  admin1_lvl <- filter(mapping, iso3 == iso3_c)$admin1_level
  
  plot <- plot %>%
    separate(area_id, into=c(NA, "area_level", NA), sep=c(4,5), convert=TRUE, remove=FALSE) %>%
    filter(variable == "tfr",
           value < 10,
           area_level == admin1_lvl)
  
  dat %>%
    filter(source %in% c("AR1 + trend", "ARIMA(1,1,0) + trend"),
           variable == "tfr",
           area_level == admin1_lvl,) %>%
    ggplot(aes(x=period, y= median)) +
    geom_point(data=plot, aes(y=value)) +
    geom_line(aes(color=source), size=1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=source), alpha=0.2) +
    
    facet_wrap(~area_id)
  
}, selection, plot[names(selection)])

p_comp
