library(tidyverse)
library(sf)
library(moz.utils)
library(ggpubr)

ssa_iso3 <- moz.utils::ssa_iso3()
mapping <- read.csv("global/iso_mapping_fit.csv")
areas <- readRDS("global/areas.rds")
grey <- read_sf(moz.utils::grey_areas()) %>%
  bind_rows(areas$BWA %>% filter(area_level == 0),
            areas$SSD %>% filter(area_level == 0) %>% select(-spectrum_region_code),
            areas$GNQ %>% filter(area_level == 0)
  )

dat <- list.files("outside_orderly/fit/outputs", full.names = T, all.files = T, recursive = T, pattern = "fr.csv") %>%
  lapply(grep, pattern = "district", value = T) %>%
  unlist() %>%
  lapply(read_csv, show_col_types = F)

fr_plot <- list.files("outside_orderly/asfr/outputs", full.names = T, all.files = T, recursive = T, pattern = "fr_plot.rds") %>%
  lapply(readRDS)


pop <- readRDS("global/pop.rds")

pop <- pop %>% 
  bind_rows() %>%
  filter(year == 2018,
         sex == "female") %>%
  moz.utils::five_year_to_15to49("population")

# fr <- c(fr, "COD" = list(read.csv("archive/cod_fit_new/20210922-172127-8f9e6cd3/fr.csv")))

region <- moz.utils::region()

fr <- dat %>% 
  bind_rows() %>%
  left_join(region) %>%
  arrange(iso3)

fr_list <- fr %>%
  group_split(iso3) %>%
  setNames(unique(fr$iso3))
  # setNames(sort(c(ssa_iso3, "COD")))

lvls <- fr %>%
  group_by(iso3) %>%
  filter(area_level == max(area_level)) %>%
  select(iso3, area_level) %>%
  distinct %>%
  arrange(iso3) %>%
  .$area_level

nat_geom <- read_sf(moz.utils::national_areas())

dist_fr <- fr_list %>%
  Map(function(fr, areas, level) {
  
  fr %>%
    filter(area_level == level,
           variable == "tfr"
    ) %>%
    select(iso3, area_id, area_name, period, median) %>%
    left_join(areas %>% select(area_id, geometry), by = "area_id")
  
  
}, fr = ., areas[names(.)], lvls)

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

plot_order <- c("SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "ETH", "GAB", "COG", "COD", "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "ZWE", "NAM", "SWZ", "LSO", "ZAF")

dist_18 <- dist_fr %>%
  bind_rows() %>%
  filter(period == 2018,
         iso3 != "SSD")

nat_18 <- fr %>%
  filter(area_level ==0, period == 2018, iso3 != 'SSD') %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot()))

# 
# fr %>%
#   left_join(mapping) %>%
#   filter(area_level == 0, period == 2016, variable == "asfr") %>%
#   left_join(naomi::get_age_groups()) %>%
#   mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
#   group_by(iso3, area_id) %>%
#   summarise(af = sum(median * age_mid_point),
#             f = sum(median),
#             macb = af/f) %>%
#   left_join(region) %>%
#   left_join(fr %>% filter(area_level == 0, period == 2016, variable == "tfr")) %>%
#   ggplot(aes(x=macb, y=median)) +
#     geom_point() +
#     geom_smooth(method = "lm", se=FALSE) +
#     standard_theme() +
#     labs(y="TFR", x="Mean age of child bearing")
#   
# fr %>%
#   left_join(mapping) %>%
#   filter(area_level == 0, variable == "asfr") %>%
#   left_join(naomi::get_age_groups()) %>%
#   mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
#   group_by(iso3, period,region, area_id) %>%
#   summarise(af = sum(median * age_mid_point),
#             f = sum(median),
#             macb = af/f) %>%
#   ggplot(aes(x=period, y=macb, group =period)) +
#     geom_boxplot() +
#     standard_theme() +
#     labs(x=element_blank(), y="MACB") +
#     facet_wrap(~region)

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

dist_18 %>%
  select(-geometry) %>%
  group_by(iso3) %>%
  summarise(
    u = max(median),
    l = min(median),
    rng = max(median) - min(median)) %>%
  ungroup() %>%
  arrange(desc(rng))

dist_18 %>%
  group_by(iso3) %>%
  summarise(median = median(median)) %>%
  left_join(nat_18 %>% filter(variable == "tfr") %>% select(iso3, nat_median = median)) %>%
  filter(nat_median < median)

asfr <- lapply(list.files("outside_orderly/asfr/outputs/", recursive = T, pattern = "asfr.rds", full.names = T), readRDS) %>%
  lapply("[[", "national")

bind_rows(asfr) %>%
  pull(survey_id) %>%
  unique %>%
  length

bind_rows(asfr) %>% pull(pys) %>% sum
bind_rows(asfr) %>% pull(births) %>% sum
bind_rows(asfr) %>% distinct(iso3, survey_id) %>% count(iso3) %>% reframe(calculate_quantile(n, percentage = F))


f1a <- dist_fr %>%
  bind_rows() %>%
  filter(period == 2018, iso3 != "SSD") %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill=median), size=0, color = NA) +
  geom_sf(data = grey, aes(geometry = geometry), fill="grey", size=0.3) +
  geom_sf(data = nat_geom, aes(geometry = geometry), fill=NA, size=0.3) +
  labs(fill = "Total fertility rate", tag = "A")+
  scale_fill_gradientn(colours = pal, breaks = seq(2,10,2), labels = as.character(seq(2,10,2))) +
  theme_minimal() +
  theme(legend.position = c(.2, .4),
        legend.direction = "horizontal",
        legend.key.width = unit(1.5, "cm"),
        legend.text = element_text(size=16),
        legend.title = element_text(size=16),
        plot.tag = element_text(size = 23, face = "bold")) +
  coord_sf(datum = NA) +
  guides(fill = guide_colorbar(title.position = "top",
                               title.hjust = 0.5))

f1b <- dist_18 %>%
  filter(iso3 != "SSD") %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot())) %>%
  left_join(region) %>%
  left_join(pop %>% ungroup() %>% select(area_id, population)) %>%
  group_by(iso3) %>%
  mutate(population = population/median(population)) %>%
  name_region(F) %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, plot_order)), y=median)) +
    geom_jitter(aes(color=region, size = population), shape=20, width =0.1, alpha = 0.5) +
    geom_point(data=nat_18 %>% filter(variable == "tfr"), shape = 21, size = 3, fill = "white", col = "black", alpha = 0.9) +
    standard_theme() +
    scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name", custom_match = cc_plot())) +
    scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
    scale_size_continuous(breaks = c(0.5, 1, 10, 20, 100), labels=paste0(c(0.5, 1, 10, 20, 100), "x")) +
    expand_limits(y=0) +
    labs(y="Total fertility rate",
         x=element_blank(),
         size = "District population relative\nto median district size",
         color = element_blank(),
         tag = "B") +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    theme(legend.title.align = 0.5,
          legend.text = element_text(size=14),
          legend.title = element_text(size=14),
          axis.text = element_text(size = 16),
          axis.title = element_text(size=16),
          plot.tag = element_text(size = 23, face = "bold")
          ) +
    guides(color = guide_legend(nrow = 2,
                                override.aes = list(size = 4,
                                                    alpha = 1)
                                ),
           size = guide_legend(title.position = "left", 
                               nrow = 2)) +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    coord_flip()

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Figs/Fig 2.png", width = 1400, height = 800)
ggpubr::ggarrange(f1a, f1b, nrow = 1)
dev.off()

###

# nat_decline <- fr %>%
#   filter(area_level ==0,
#          variable == "tfr",
#          period > 1999) %>%
#   group_by(iso3, region) %>%
#   summarise(decline = median[period == 2020]/median[period == 2000])
# 
# 1-quantile(nat_decline$decline, c(0.25, 0.5, 0.75))
# 1-quantile(filter(nat_decline, region == "ESA")$decline, c(0.25, 0.5, 0.75))
# 1-quantile(filter(nat_decline, region == "WCA")$decline, c(0.25, 0.5, 0.75))
# 
# ###
# 
# ntl_change <- fr %>%
#   filter(area_level ==0,
#          variable == "tfr") %>%
#   group_by(iso3) %>%
#   mutate(ntl_change = median/median[period == 2000]) %>%
#   select(iso3, period, ntl_change)
# 
# dist_fr %>%
#   bind_rows() %>%
#   select(-geometry) %>%
#   filter(period > 1999) %>%
#   group_by(area_id) %>%
#   mutate(dist_change = median/median[period == 2000]) %>%
#   left_join(ntl_change) %>%
#   mutate(dist_ntl_change_ratio = dist_change/ntl_change) %>%
#   ggplot(aes(x=period,y=dist_ntl_change_ratio-1, group=area_id)) +
#     geom_line(alpha = 0.1) +
#     scale_y_continuous(labels = scales::label_percent()) +
#     standard_theme() +
#     labs(x=element_blank(), y="District change relative to national change")

#####

# p <- fr %>%
#   left_join(mapping %>% select(iso3, fertility_fit_level)) %>%
#   filter(area_level %in% c(0, fertility_fit_level)) %>%
#   group_by(iso3) %>%
#   group_split() %>%
#   lapply(function(x) {
#     
#     ntl_change <- x %>%
#       filter(area_level ==0,
#              variable == "tfr",
#              period > 1999) %>%
#       group_by(iso3) %>%
#       mutate(ntl_change = median/median[period == 2000]) %>%
#       select(iso3, period, ntl_change)
#     
#     x %>%
#       filter(period > 1999,
#              variable == "tfr") %>%
#       select(iso3, area_id, period, median) %>%
#       group_by(area_id) %>%
#       mutate(dist_change = median/median[period == 2000]) %>%
#       left_join(ntl_change, by= c("iso3", "period")) %>%
#       mutate(dist_ntl_change_ratio = dist_change/ntl_change) %>%
#       ggplot(aes(x=period,y=dist_ntl_change_ratio-1, group=area_id)) +
#       geom_line(alpha = 0.1) +
#       scale_y_continuous(labels = scales::label_percent()) +
#       standard_theme() +
#       labs(x=element_blank(), y="District change relative to national change", title = unique(ntl_change$iso3))
#     
#   })

###
# Rwanda, Tanzania, Mozambique, Zambia, Angola, Zimbabwe, and Lesotho

# ntl_asfr <- fr %>%
#   filter(area_level == 0, 
#          variable == "asfr",
#          iso3 %in% c("RWA", "TZA", "MOZ", "ZMB", "ZWE", "LSO", "MWI")
#          )
# 
# ntl_asfr %>%
#   left_join(naomi::get_age_groups()) %>%
#   ggplot(aes(x=period, y=median)) +
#     geom_line(aes(color=age_group_label)) +
#     geom_ribbon(aes(ymin = lower, ymax = upper, fill=age_group_label), alpha = 0.3) +
#     facet_wrap(~area_name) +
#     standard_theme() +
#     labs(x=element_blank(), y="ASFR", color=element_blank(), fill=element_blank())
# 
# dist_fr %>%
#   bind_rows() %>%
#   # filter(variable == "tfr") %>%
#   group_by(iso3) %>%
#   filter(period == 2018) %>%
#   summarise(median = median(median)) %>%
#   left_join(nat_18 %>% filter(variable == "tfr") %>% rename(nat_median = median)) %>%
#   mutate(nat_lower = as.integer(nat_median < median)) %>%
#   count(nat_lower)
# 
# 
# pdf("~/Downloads/dist_tfr.pdf", width = 15, height = 10)
# lapply(dist_fr, function(x) {
#   t <- unique(x$iso3)
#   x %>%
#     st_drop_geometry() %>%
#     select(-geometry) %>%
#     ggplot(aes(x=period, y=median, group = area_id)) +
#     geom_line() +
#     labs(title = t)
# })
# dev.off()
# 
# pdf("~/Downloads/dist_tfr_norm.pdf", width = 15, height = 10)
# lapply(dist_fr, function(x) {
#   t <- unique(x$iso3)
#   x %>%
#     st_drop_geometry() %>%
#     select(-geometry) %>%
#     filter(period > 1999) %>%
#     group_by(area_id) %>%
#     mutate(median = median/median[period == 2000]) %>%
#     ggplot(aes(x=period, y=median, group = area_id)) +
#     geom_line() +
#     labs(title = t)
# })
# dev.off()

fr %>%
  filter(area_level ==0,
         variable == "tfr") %>%
  group_by(iso3) %>%
  mutate(ntl_change = median/median[period == 2000]) %>%
  select(iso3, period, ntl_change) %>%
  filter(period == 2020) %>%
  left_join(region()) %>%
  ungroup() %>%
  reframe(calculate_quantile(ntl_change), .by = "region") %>%
  mutate(across(-region, ~.x-100))

dist_18 %>%
  filter(median < 2.1)

# ## SLE, MLI, TZA, UGA?, KEN, ETH
f3a <- fr %>%
  left_join(areas$TZA %>% select(area_id, area_sort_order) %>% st_drop_geometry()) %>%
  filter(iso3 == "TZA",
         variable == "tfr",
         area_level == 4,
         # str_detect(area_id, "TZA_4_12|4_14"),
         !area_sort_order %in% c(96, 149, 159, 217, 213)
  ) %>%
  
  #
  ggplot(aes(x=period, y=median, group = area_id)) +
  geom_line(alpha = 0.1) +
  geom_line(data = fr %>%
              filter(iso3 == "TZA",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(iso3 == "TZA") %>%
              left_join(areas$TZA %>% select(area_id, area_sort_order) %>% st_drop_geometry()) %>%
              filter(variable == "tfr",
                     area_sort_order %in% c(96, 149, 159, 217, 213)),
            aes(color=area_id),
            size=1.3) +
  scale_manual("color", 5) +
  # scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1,4,5)], wesanderson::wes_palette("Rushmore1")[3])) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  standard_theme() +
  expand_limits(y=c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15)) +
  labs(y="Total Fertility Rate", x=element_blank(), title="Tanzania")

f3b <- fr %>%
  left_join(areas$ETH %>% select(area_id, area_sort_order) %>% st_drop_geometry()) %>%
  filter(iso3 == "ETH",
         variable == "tfr",
         area_level == 2,
         !area_sort_order %in% c(112, 125, 14, 74)
  ) %>%
  # filter(area_sort_order %in% 72:77) %>%
  ggplot(aes(x=period, y=median, group = area_id)) +
  geom_line(alpha = .1) +
  geom_line(data = fr %>%
              filter(iso3 == "ETH",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(iso3 == "ETH") %>%
              left_join(areas$ETH %>% select(area_id, area_sort_order) %>% st_drop_geometry()) %>%
              filter(variable == "tfr",
                     area_sort_order %in% c(112, 125, 14, 74)),
            aes(color=area_id),
            size=1.3) +
  scale_manual("color", 4) +
  # scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1,4,5)], wesanderson::wes_palette("Rushmore1")[3])) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  standard_theme() +
  expand_limits(y=c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15)) +
  labs(y="Total Fertility Rate", x=element_blank(), title="Ethiopia")
# 
f3c <- fr %>%
  left_join(areas$KEN %>% select(area_id, area_sort_order) %>% st_drop_geometry()) %>%
  filter(iso3 == "KEN",
         variable == "tfr",
         area_level == 2,
         !area_sort_order %in% c(19, 31, 32, 35)
  ) %>%
  # filter(area_sort_order %in% c(16:22)) %>%
  ggplot(aes(x=period, y=median, group = area_id)) +
  geom_line(alpha = .1) +
  geom_line(data = fr %>%
              filter(iso3 == "KEN",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(iso3 == "KEN") %>%
              left_join(areas$KEN %>% select(area_id, area_sort_order) %>% st_drop_geometry()) %>%
              filter(variable == "tfr",
                     area_sort_order %in% c(19, 31, 32, 35)),
            aes(color=area_id),
            size=1.3) +
  scale_manual("color", 4) +
  # scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1,4,5)], wesanderson::wes_palette("Rushmore1")[3])) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  standard_theme() +
  expand_limits(y=c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 15)) +
  labs(y="Total Fertility Rate", x=element_blank(), title="Kenya")
# # 
# 
# f3_dat <- dist_fr %>%
#   bind_rows() %>%
#   select(-geometry) %>%
#   filter(period > 1999,
#          iso3 %in% c("KEN", "RWA", "SLE",
#                      "UGA", "LSO", "ZWE"))
# 
# f3a <- f3_dat %>%
#   group_by(iso3, area_id) %>%
#   mutate(median = (median/median[period == 2000])-1) %>%
#   ungroup() %>%
#   reframe(quantile_df(median, c(0, 0.2, 0.4, 0.5, 0.6, 0.8, 1)), .by = c(iso3, period)) %>%
#   mutate(quant = paste0("q", quant)) %>%
#   pivot_wider(names_from = quant, values_from = val) %>%
#   mutate(iso3 = factor(iso3, 
#                        levels = c("LSO", "UGA", "ZWE", "KEN", "SLE", "RWA"), 
#                        labels = c("A: Lesotho", "B: Uganda", "C: Zimbabwe", "D: Kenya", "E: Sierra Leone", "F: Rwanda"))) %>%
#   ggplot(aes(x=period)) +
#     # geom_line(aes(y=q0.5)) +
#     geom_ribbon(aes(ymin = q0.4, ymax = q0.6), fill = "blue", alpha = 0.6) +
#     geom_ribbon(aes(ymin = q0.2, ymax = q0.8), fill = "blue", alpha = 0.4) +
#     geom_ribbon(aes(ymin = q0, ymax = q1), fill = "blue", alpha = 0.1) +
#     scale_percent() +
#     geom_hline(aes(yintercept = 0), linetype = 2) +
#     scale_x_continuous(breaks = c(2005, 2015, 2025)) +
#     facet_wrap(~iso3, ncol = 1) +
#     labs(x=element_blank(), y="TFR change relative to 2000", title = "Pooled trajectories") +
#     standard_theme() +
#     theme(plot.title = element_text(hjust = 0.5, face = "bold"),
#           strip.text = element_text(hjust = 0),
#           panel.grid = element_blank(),
#           legend.text = element_text(size=14),
#           legend.title = element_text(size=14),
#           axis.text = element_text(size = 16),
#           axis.title = element_text(size=18))
# 
# f3b <- f3_dat %>%
#   group_by(iso3, area_id) %>%
#   mutate(median = (median/median[period == 2000])-1,
#          iso3 = factor(iso3, levels = c("LSO", "UGA", "ZWE", "KEN", "SLE", "RWA"))) %>%
#   ggplot(aes(x=period, y=median, group = area_id)) +
#   geom_line(alpha = 0.15) +
#   scale_percent() +
#   geom_hline(aes(yintercept = 0), linetype = 2) +
#     scale_x_continuous(breaks = c(2005, 2015, 2025)) +
#   facet_wrap(~iso3, ncol = 1) +
#   labs(x=element_blank(), y=element_blank(), title = "District-specific trajectories") +
#   standard_theme() +
#   theme(plot.title = element_text(hjust = 0.5, face = "bold"),
#         strip.text = element_text(color = "white"),
#         panel.grid = element_blank(),
#         legend.text = element_text(size=14),
#         axis.text.y =element_blank(),
#         legend.title = element_text(size=14),
#         axis.text = element_text(size = 16),
#         axis.title = element_text(size=16))

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Figs/Fig 3.png", width = 500, height = 1000)
ggpubr::ggarrange(f3a, f3b, f3c, ncol = 1)
dev.off()


quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
  tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}

name_countries <- function(x, custom_match = T) {
  if(custom_match)
    x %>%
      mutate(iso3 = countrycode::countrycode(iso3, 'iso3c', 'country.name', custom_match = moz.utils::cc_plot()))
  else
    x %>%
      mutate(iso3 = countrycode::countrycode(iso3, 'iso3c', 'country.name'))
}

fr %>%
  left_join(mapping) %>% 
  filter(area_level == fertility_fit_level,
         variable == "asfr",
         period == 2018,
         iso3 == "NGA") %>%
  group_by(area_id) %>%
  mutate(median = median/median[age_group == "Y020_024"]) %>%
  filter(age_group == "Y025_029") %>% 
  ungroup() %>% 
  reframe(range(median))


fr %>%
  left_join(mapping) %>%
  filter(area_level == fertility_fit_level, period == 2018, variable == "asfr") %>%
  left_join(naomi::get_age_groups()) %>%
  mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
  group_by(iso3, area_id) %>%
  summarise(af = sum(median * age_mid_point),
            f = sum(median),
            macb = af/f)
    


# left_join(region) %>%
#   ggplot(aes(x=fct_rev(fct_relevel(iso3, plot_order)), y=macb)) +
#   geom_jitter(aes(color=region), shape=20, width =0.1, alpha = 0.5) +
#   # geom_point(data=nat_18 %>% filter(variable == "asfr") %>% group_by(iso3) %>%
#   #              mutate(median = median/median[age_group == "Y020_024"]) %>%
#   #              filter(age_group == "Y025_029"), shape = 21, size = 3, fill = "white", col = "black", alpha = 0.9) +
#   scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
#   # scale_y_continuous(labels = scales::label_percent())+
#   scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name")) +
#   labs(y="Mean age of childbearing", x=element_blank()) +
#   standard_theme() +
#   theme(legend.position = "right") +
#   # plot.margin = unit(c(1,0,5,0), "lines")) +
#   coord_flip(clip = "off", xlim = c(0, 36))

f4a <- fr %>%
  left_join(mapping) %>%
  filter(area_level == fertility_fit_level, period == 2018, variable == "asfr", iso3 != "SSD") %>%
  left_join(naomi::get_age_groups()) %>%
  mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
  group_by(iso3, area_id) %>%
  summarise(af = sum(median * age_mid_point),
            f = sum(median),
            macb = af/f) %>%
  left_join(region) %>%
  name_region(F) %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, plot_order)), y=macb)) +
  geom_jitter(shape=20, width =0.1, alpha = 0.2, aes(color = region)) +
  geom_point(data=nat_18 %>% filter(area_level == 0, period == 2018, variable == "asfr", iso3 != "SSD") %>%
               left_join(naomi::get_age_groups()) %>%
               mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
               group_by(iso3, area_id) %>%
               summarise(af = sum(median * age_mid_point),
                         f = sum(median),
                         macb = af/f), shape = 21, size = 3, fill = "white", col = "black", alpha = 0.9) +
  scale_manual("color", 2) +
  scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name", custom_match = cc_plot())) +
  labs(x=element_blank(), y="Mean age at childbearing (years)", color=element_blank(), tag = "A") +
  standard_theme() +
  theme(plot.tag = element_text(size = 16, face = "bold")) +
  coord_flip() +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

# 
age_dist <- fr %>%
  left_join(mapping) %>%
  filter(area_level == admin1_level,
         variable == "asfr",
         period == 2018) %>%
  left_join(naomi::get_age_groups()) %>%
  mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
  group_by(iso3, area_id) %>%
  summarise(af = sum(median * age_mid_point),
            f = sum(median),
            macb = af/f) %>%
  left_join(areas %>% lapply(select, -any_of("spectrum_region_code")) %>% bind_rows() %>% select(area_id)) %>%
  group_by(iso3) %>%
  group_split() %>%
  lapply(function(x) {
    c <- countrycode::countrycode(unique(x$iso3), "iso3c", "country.name")
    x %>%
      ggplot() +
      geom_sf(aes(geometry = geometry, fill=macb)) +
      scale_fill_gradientn(colours = pal,
                           breaks = seq(27, 32, 1),
                           limits = c(27, 32)) +
      labs(title = c, fill="Mean age of childbearing (years)") +
      theme_minimal()+
      coord_sf(datum=NA) +
      theme(legend.position = "bottom",
            legend.key.width = unit(2, "cm"),
            legend.text = element_text(size=13),
            plot.title = element_text(hjust = 0.5, size = 14),
            legend.title = element_text(size=13)) +
      guides(fill = guide_colorbar(title.position = "top",
                                   title.hjust = 0.5))

  }) %>% setNames(unique(fr$iso3))

pdf("~/Downloads/relative_asfr_admin1.pdf", width = 15, height = 10)
age_dist
dev.off()


f4b <- ggpubr::ggarrange(age_dist[["BDI"]] + theme(plot.title = element_text(margin = margin(5))), age_dist[["NAM"]], nrow=1, common.legend = TRUE, legend="bottom")
f4b <- annotate_figure(f4b, fig.lab = "B", fig.lab.pos = "top.left", fig.lab.size = 16, fig.lab.face = "bold")

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Figs/Fig 4.png", height = 1000, width = 600)
ggarrange(f4a, f4b, nrow = 2, heights = c(2,1))
dev.off()



    
fr %>%
  filter(area_level == 0,
         variable == "asfr",
         period %in% c(2000, 2010, 2020),
         # !iso3 %in% c("NER", "GAB", "GIN", "MWI")
  ) %>%
  name_region(F) %>%
  group_by(region, area_id, area_name, age_group) %>%
  summarise(
    # ratio_10 = median[period == 2010]/median[period == 2000],
    #         ratio_20 = median[period == 2020]/median[period == 2010],
    ratio_all = median[period == 2020]/median[period == 2000]) %>%
  group_by(age_group, region) %>%
  reframe(calculate_quantile(ratio_all)) %>%
  arrange(region) %>%
  mutate(across(-c(age_group, region), ~.x-100))

f5a <- fr %>%
  filter(area_level == 0,
        variable == "asfr",
        period %in% c(2000, 2010, 2020),
        iso3 != "SSD",
        # !iso3 %in% c("NER", "GAB", "GIN", "MWI")
        ) %>%
  name_region(F) %>%
  group_by(region, area_id, area_name, age_group) %>%
  summarise(
    # ratio_10 = median[period == 2010]/median[period == 2000],
    #         ratio_20 = median[period == 2020]/median[period == 2010],
             ratio_all = median[period == 2020]/median[period == 2000]) %>%
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
  pivot_longer(-c(region, area_id, area_name, age_group, age_group_label)) %>%
  # mutate(time = factor(name, labels=c("2000-2010", "2010-2020")),
  #        name = paste0(region, " | ", time)) %>%
  ggplot(aes(x=age_group_label, y=value-1)) +
    geom_boxplot(outlier.shape = NA, coef= 0) +
    geom_jitter(width = 0.2, alpha = 0.5, shape = 16, size = 2) +
    scale_y_continuous(labels = scales::label_percent(), name = "% ASFR change 2000-2020", limits=c(-.5, .25)) +
    geom_hline(aes(yintercept = 0), linetype=3) +
    facet_wrap(~region) +
    standard_theme() +
    labs(x=element_blank(), tag = "A") +
    theme(axis.text = element_text(size = 16),
          axis.title = element_text(size = 16),
          strip.text = element_text(size = 16),
          plot.tag = element_text(size = 20, face = "bold"),
          panel.background = element_rect(fill=NA, color="black"),
          panel.grid = element_blank())

# age_distribution_plots <- lapply(fr_list, function(fr) {
#   fr %>%
#     filter(variable == "asfr",
#            period %in% c(2000, 2010, 2020),
#            area_level == 1) %>%
#     ggplot(aes(x=desc(period), y=median, fill=age_group)) +
#     geom_col(position = "fill")  +
#     facet_wrap(~area_name) +
#     coord_flip()
# })

# 

f5b <- fr_list %>%
  lapply(data.frame) %>%
  bind_rows() %>%
  filter(variable == "asfr",
         area_level == 0,
         period > 1999,
         iso3 %in% c("LSO", "MWI", "MOZ", "ZMB")) %>%
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
  name_countries(F) %>%
  mutate(is_15 = ifelse(age_group_label == "15-19", "15-19", "Other age groups")) %>%
  ggplot(aes(x=period, y=median, group = age_group)) +
  geom_line(size=1.1, aes(color=factor(is_15))) +
  # geom_ribbon(aes(ymin = lower, ymax = upper, fill = age_group_label), alpha = 0.5, show.legend = FALSE) +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  scale_color_manual(values = c("red", "black")) +
  moz.utils::standard_theme() +
  labs(y = "Age-specific fertility rate", x=element_blank(), color = element_blank(), tag = "B") +
  facet_wrap(~iso3, nrow = 1) +
  coord_cartesian(clip="off", xlim=c(2000, 2025)) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        strip.text = element_text(size = 16),
        legend.text = element_text(size = 16),
        panel.spacing.x = unit(1, "lines"),
        plot.tag = element_text(size = 20, face = "bold"),
        panel.grid = element_blank())

f5c <- fr_list[["KEN"]] %>%
  filter(area_level ==1, age_group == "Y020_024", period >1999) %>%
  group_by(area_id) %>%
  mutate(median = median/median[period==2000]) %>%
  ggplot(aes(x=period, y=median, color=area_name)) +
    geom_line(size=1, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = . %>% filter(period == max(period)), aes(color =area_name, label = area_name), 
                              xlim = c(-Inf, Inf), nudge_x = 4, size=6, segment.linetype = 2, show.legend = FALSE, direction="y") +
    geom_hline(aes(yintercept = 1), linetype = 2) +
    scale_x_continuous(breaks = c(2000, 2010, 2020)) +
    standard_theme() +
    coord_cartesian(clip="off", xlim=c(2000, 2025)) +
    scale_percent() +
    labs(x=element_blank(), y="20-24 ASFR normalised to 2000", tag = "C") +
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 16),
      plot.margin = unit(c(1,9,1,0), "lines"),
          plot.tag = element_text(size = 20, face = "bold"),
          panel.grid = element_blank())

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Figs/Fig 5.png", width = 900, height = 1200)
ggarrange(f5a, f5b, f5c, nrow=3, widths = c(1, 1, 0.4))
dev.off()


fixed_dist <- lapply(list.files("outside_orderly/fit/outputs", full.names = T, recursive = T, pattern = "sd_report") %>%
                       grep("district", ., value = T), read_csv, show_col_types = F)

fixed_dist %>%
  bind_rows()  %>%
  filter(str_detect(hyper, "tips"),
         !iso %in% c("CAF", "SSD", "GNB")) %>%
  rename(iso3 = iso) %>%
  mutate(type = rep(c(0,5,6,10), times = 33),
         est = 100*(exp(Estimate)-1),
         Country = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot())) %>%
  left_join(region()) %>%
  select(region, Country, type, est) %>%
  mutate(est = sprintf("%1.0f", est)) %>%
  pivot_wider(names_from = type, values_from = est) %>%
  bind_rows(
    fixed_dist %>%
      bind_rows()  %>%
      filter(str_detect(hyper, "tips"),
             !iso %in% c("CAF", "SSD", "GNB")) %>%
      rename(iso3 = iso) %>%
      left_join(region()) %>%
      mutate(type = rep(c(0,5,6,10), times = 33),
             est = exp(Estimate)) %>%
      bind_rows(mutate(., region = "SSA")) %>%
      group_by(region, type) %>%
      reframe(calculate_quantile(est)) %>%
      mutate(across(`0.25`:`0.75`, ~round(.x-100, 0)),
             text = sprintf("%1.0f (%1.0f, %1.0f)", `0.5`, `0.25`, `0.75`),
             Country = "ZZZ") %>%
      select(region, Country, type, text) %>%
      pivot_wider(names_from = type, values_from = text)
  ) %>%
  factor_region() %>%
  arrange(region, Country) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Figs/Table 1.csv")

###############################
#### Supplementary figures ####
###############################

wpp <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/population/WPP2022/WPP2022_Demographic_Indicators_Medium.csv", show_col_types = F)

wpp <- wpp %>%
  select(iso3 = ISO3_code, period = Time, median = TFR) %>%
  filter(iso3 %in% ssa_iso3,
         period %in% 2000:2025) %>%
  mutate(source = "WPP2022")

fr_comp <- fr %>%
  filter(area_level == 0,
         variable == "tfr") %>%
  bind_rows(wpp)

recent_survey <- fr_plot %>% 
  lapply("[[", "national") %>%
  bind_rows() %>%
  distinct(iso3, survey_id) %>%
  separate_survey_id(F) %>%
  bind_rows(read_csv("global/phia_asfr.csv", show_col_types = F) %>%
              moz.utils::separate_survey_id(F) %>%
              distinct(iso3, year)) %>%
  group_by(iso3) %>%
  filter(year == max(year))

fr_comp_plot <- fr_comp %>%
  filter(!iso3 %in% c("ERI", "GNQ", "BWA", "SSD"),
         period > 1999) %>%
  mutate(source = ifelse(source == "tmb", "District-level fertility model", source)) %>%
  name_countries(custom_match = T) %>%
  ggplot(aes(x=period, y=median)) +
    geom_line(aes(color = source), size = 1) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.3, show.legend = F) +
    geom_vline(data = recent_survey %>% filter(iso3 != "SSD") %>% name_countries(T) %>% rename(period = year), aes(xintercept = period), linetype = 2) +
    scale_x_continuous(breaks = c(2005, 2015, 2025)) +
    facet_wrap(~iso3, ncol = 4) + 
    standard_theme() +
    scale_manual("color", 2) +
    scale_manual("fill", 2) +
    coord_cartesian(ylim = c(2,10)) +
    theme(panel.grid = element_blank()) +
    labs(x=element_blank(), y="Total fertility rate", color=element_blank(), fill =element_blank())

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/fr_comparison.png", height = 1300, width = 800)
fr_comp_plot
dev.off()

wpp_asfr <- read_csv("~/Imperial College London/HIV Inference Group - WP - Documents/Data/population/WPP2022/WPP2022_Fertility_by_Age5.csv", show_col_types = F)

wpp_asfr <- wpp_asfr %>%
  select(iso3 = ISO3_code, period = Time, median = ASFR, age_group_label = AgeGrp) %>%
  filter(iso3 %in% ssa_iso3,
         period %in% 2000:2025) %>%
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
  filter(age_group %in% unique(fr$age_group)) %>%
  mutate(source = "WPP2022",
         median = median/1000)

asfr_comp <- fr %>%
  filter(area_level == 0,
         variable == "asfr") %>%
  left_join(naomi::get_age_groups() %>% select(age_group, age_group_label)) %>%
  bind_rows(wpp_asfr %>% left_join(region()))

asfr_comp_plot <- asfr_comp %>%
  filter(!iso3 %in% c("ERI", "GNQ", "BWA", "SSD"),
         period == 2018) %>%
  mutate(source = ifelse(source == "tmb", "District-level fertility model", source)) %>%
  name_countries(custom_match = T) %>%
  ggplot(aes(x=age_group_label, y=median, group = source)) +
  geom_line(aes(color = source), size = 1) +
  geom_point(aes(color = source), size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.3, show.legend = F) +
  facet_wrap(~iso3, ncol = 4) + 
  standard_theme() +
  scale_manual("color", 2) +
  scale_manual("fill", 2) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 35, hjust=1)) +
  labs(x=element_blank(), y="Age-specific fertility rate", color=element_blank(), fill =element_blank())

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/asfr_comparison.png", height = 1300, width = 800)
asfr_comp_plot
dev.off()


######
    
area_label_df <- areas %>%
  lapply(select, -any_of("spectrum_region_code")) %>%
  bind_rows() %>% 
  filter(!iso3 %in% c("ERI", "GNQ", "BWA")) %>%
  st_drop_geometry() %>% 
  separate(area_id, into=c("iso3", NA), sep=3) %>% 
  count(iso3, area_level, area_level_label)

area_label_df %>%
  left_join(mapping %>%
              select(iso3, fertility_fit_level, admin1_level) %>%
              pivot_longer(-iso3, values_to = "area_level")
  ) %>%
  filter(!is.na(name)) %>%
  mutate(area_level_label = paste0(area_level_label, " (", n, ")")) %>%
  select(-c(area_level, n)) %>%
  pivot_wider(names_from=name, values_from = area_level_label) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot())) %>%
  select(country, admin1_level, fertility_fit_level) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/admin_level_label.csv")

####

mwi_15_dhs <- fr_plot[["MWI"]] %>%
  filter(area_level ==0, survey_id == "MWI2015DHS", variable == "tfr") %>%
  arrange(desc(period)) %>%
  mutate(tips = as.integer(row_number()-1),
         tips = ifelse(tips %in% c(5,6), tips, NA_integer_))

tips_overlap1 <- mwi_15_dhs %>%
  filter(period > 2004) %>%
  ggplot(aes(x=period, y=value)) +
  geom_line(size =1) +
  geom_point(size=2) +
  ggrepel::geom_label_repel(aes(label = tips), 
                            min.segment.length = 0,
                            segment.linetype = 2,
                            point.size=1, nudge_y = 0.5, 
                            nudge_x = 0.5, show.legend = FALSE) +
  standard_theme() +
  scale_x_continuous(breaks = seq(2005, 2015, 2), labels = as.character(seq(2005, 2015, 2))) +
  expand_limits(y=c(2, 8)) +
  labs(y="TFR", x=element_blank(), title = "Malawi | 2015 DHS")

tips_overlap2 <- fr_plot[["MWI"]] %>%
  filter(area_level ==0, survey_id != "MWI2015DHS", variable == "tfr", value > 4) %>%
  ggplot(aes(x=period, y=value, group=survey_id)) +
  geom_line(size =1, alpha =0.3) +
  geom_point(size=2, alpha = 0.3) +
  geom_line(data = mwi_15_dhs,
            size =1, color="red") +
  geom_point(data = mwi_15_dhs,
             size =2, color="red") +
  ggrepel::geom_label_repel(data = mwi_15_dhs, aes(label = tips), 
                            min.segment.length = 0,
                            segment.linetype = 2,
                            point.size=1, nudge_y = 1, 
                            nudge_x = 1, show.legend = FALSE, color="red") +
  standard_theme() +
  # scale_x_continuous(breaks = seq(2005, 2015, 2), labels = as.character(seq(2005, 2015, 2))) +
  expand_limits(y=c(2, 8)) +
  labs(y="TFR", x=element_blank(), title = "Malawi | All surveys")

ggpubr::ggarrange(tips_overlap1, tips_overlap2)

###

tips_survey_plot <- lapply(fr_plot, "[[", "national") %>%
  bind_rows() %>%
  separate(area_id, into=c(NA, "area_level", NA), sep=c(4,5), remove=FALSE, convert=TRUE) %>%
  separate_survey_id() %>%
  filter(survey_id %in% c("NER2006DHS", "NER2012DHS",
                          "BFA2010DHS",
                          "COD2013DHS",
                          # "ETH2019DHS",
                          "GHA2014DHS",
                          "MOZ2008MICS",
                          "NGA2018DHS",
                          "TGO2013DHS",
                          "SWZ2006DHS",
                          "NAM2006DHS"),
         area_id == iso3,
         variable == "tfr") %>%
  group_by(survey_id) %>%
  arrange(desc(period), survey_id, .by_group = TRUE) %>% 
  mutate(tips = as.integer(row_number()-1),
         tips = case_when(
           survey_id == "BFA2010DHS" & tips %in% c(5,6) ~ tips,
           survey_id == "COD2013DHS" & tips %in% c(10) ~ tips,
           survey_id == "GHA2014DHS" & tips %in% c(5,6, 10) ~ tips,
           survey_id == "NAM2006DHS" & tips %in% c(5,6) ~ tips,
           survey_id == "NER2012DHS" & tips %in% c(0, 5, 6) ~ tips,
           survey_id == "NGA2018DHS" & tips %in% c(10) ~ tips,
           survey_id == "SWZ2006DHS" & tips %in% c(5,6) ~ tips,
           survey_id == "MOZ2008MICS" & tips %in% c(10) ~ tips,
           survey_id == "NER2006DHS" & tips %in% c(0,5,6) ~ tips,
           survey_id == "TGO2013DHS" & tips %in% c(5,6) ~ tips,
           TRUE ~ NA_integer_
         )) %>%
  ggplot(aes(x=period, y=value, color=survey_id)) + 
  geom_point(show.legend = FALSE) + 
  geom_line(size=1, show.legend = FALSE) +
  expand_limits(y=0) +
  scale_y_continuous(labels = scales::label_number(1)) +
  moz.utils::standard_theme() +
  ggrepel::geom_label_repel(aes(label = tips), min.segment.length = 0,segment.linetype = 2,  point.size=1, nudge_y = 3, nudge_x = 1.5, show.legend = FALSE) +
  facet_wrap(~survey_id, ncol=3, scales = "free_y") +
  labs(y="Total fertility rate", x=element_blank()) +
  theme(panel.grid = element_blank())

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/tips_survey.png", height = 700, width = 600)
tips_survey_plot
dev.off()

####

fixed_dist %>%
  bind_rows()  %>%
  filter(hyper == "zeta2",
         !iso %in% c("CAF", "SSD", "GNB"))

asfr %>%
  bind_rows() %>%
  filter(!iso3 %in% c("CAF", "SSD", "GNB")) %>%
  distinct(iso3, survey_id, survtype, tips) %>%
  filter(tips %in% c(0,5,6,10),
         !(survtype == "MICS" & tips %in% c(5,6))
         )

# group_by(iso3) %>%
#   group_split() %>%
#   lapply(function(x) {
#     iso3_c <- unique(x$iso3)
#     cntry_name <- countrycode::countrycode(iso3_c, "iso3c", "country.name")
#     
#     x %>%
#       arrange(desc(period), survey_id) %>% 
#       group_by(survey_id) %>% 
#       mutate(tips = row_number()-1) %>%
#       ggplot(aes(x=period, y=value, color=survey_id)) + 
#       geom_point() + 
#       geom_line(size=1) +
#       moz.utils::standard_theme() +
#       ggrepel::geom_label_repel(aes(label = tips), show.legend = FALSE) +
#       facet_wrap(~survey_id) +
#       labs(y="TFR", x=element_blank(), title = cntry_name)
#   })
# 
# pdf(paste0("~/Downloads/fe.pdf"), h = 12, w = 20)
# fe
# dev.off()
###

asfr_sum <- asfr %>%
  bind_rows() %>%
  bind_rows(read.csv("global/phia_asfr.csv")) %>%
  group_by(survey_id) %>%
  summarise(births = round(sum(births)),
            pys = round(sum(pys)))

asfr_sum %>% ungroup() %>% summarise(births = sum(births), pys = sum(pys))
asfr_sum %>% separate_survey_id(F) %>% count(iso3) %>% reframe(calculate_quantile(n, percentage = F))
asfr_sum %>% separate(survey_id, remove = F, into = c(NA, "survtype"), sep = 7) %>% count(survtype)

# phia_nrow <- data.frame(survey_id = c(
#   "CIV2017PHIA",
#   "CMR2017PHIA", 
#   "LSO2017PHIA",
#   "MWI2017PHIA", 
#   "NAM2017PHIA",
#   "RWA2019PHIA", 
#   "SWZ2017PHIA",
#   "TZA2017PHIA", 
#   "UGA2017PHIA", 
#   "ZMB2016PHIA", 
#   "ZWE2016PHIA"
# ),
# n = c(8430,
#       12676,
#       6509,
#       10115,
#       8830,
#       14690,
#       5094,
#       15074,
#       14521,
#       10961,
#       11005))
# 
# mics_nrow <- lapply(list.files(paste0("archive/aaa_asfr/", asfr_id[1], "/depends"), pattern = "mics_women", full.names = TRUE), read.csv) %>%
#   bind_rows() %>%
#   count(survey_id)

old_areal_surveys <- data.frame(survey_id = c("MOZ1997DHS",
                                              "MOZ2003DHS",
                                              "KEN1998DHS",
                                              "RWA2000DHS",
                                              "RWA2013MIS",
                                              "RWA2017MIS",
                                              "ZAF1998DHS",
                                              "SEN2006MIS",
                                              "SEN2017DHS",
                                              "CIV2005AIS",
                                              "NER2006DHS",
                                              "NER2012DHS",
                                              "CMR1998DHS",
                                              "TZA1996DHS",
                                              "TZA2004DHS",
                                              "TCD2004DHS"),
                                data_type = "Areal")

long_mics <- asfr %>%
  bind_rows() %>%
  group_by(survey_id) %>%
  filter(tips == max(tips)) %>%
  distinct(survey_id, tips) %>%
  filter(str_detect(survey_id, "MICS"), tips > 5) %>%
  mutate(tips = 15)


cal_data <- asfr_sum %>%
  separate(survey_id, into=c("iso3", "survyear", "survtype"), sep=c(3, 7), remove=FALSE, convert=TRUE) %>%
  arrange(iso3, survyear) %>%
  left_join(old_areal_surveys) %>%
  left_join(long_mics) %>%
  mutate(data_type = case_when(
    is.na(data_type) & survtype != "MICS" ~ "Point",
    is.na(data_type) & survtype == "MICS" ~ "Areal",
    TRUE ~ data_type),
    tips = case_when(
      survtype == "DHS" ~ 15,
      is.na(tips) & survtype == "MICS" ~ 5,
      survtype == "PHIA" ~ 1,
      survtype %in% c("AIS", "MIS") ~ 5,
      TRUE ~ tips
    ),
    country = countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot())
  )
cal_data %>%
  select(Country = country, `Survey Year` = survyear, `Survey Type` = survtype, `Data Type` = data_type, `TIPS` = tips, 
         `Number of births` = births, `Person-years` = pys) %>%
  write_csv("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/calibration_data.csv")

### Survey availability by TIPS

raw_data <- list.files("outside_orderly/asfr/outputs", full.names = T, all.files = T, recursive = T, pattern = "asfr.rds") %>%
  lapply(readRDS) %>%
  lapply("[[", "national") %>%
  bind_rows()

remove_survey <- c(
  "GMB2005MICS",
  "SLE2010MICS", 
  "TGO2006MICS", 
  "KEN2009MICS", 
  "NGA2007MICS"
)
subnational_surveys <- c("KEN2009MICS", "KEN2011MICS")

survey_grid <- raw_data %>% 
  bind_rows() %>%
  filter(!survey_id %in% remove_survey,
         !survey_id %in% subnational_surveys,
         !(iso3 == "SWZ" & period == 2017), ## This seems dodgy
         !(period == 1995 & survey_id == "SWZ2000MICS"),
         !(period == 1999 & survey_id == "SWZ2014MICS"),
         !(period == 2004 & survey_id == "GMB2019DHS"),
         !(period == 2005 & survey_id == "GMB2010MICS"),
         !(period == 2013 & survey_id == "GMB2018MICS"),
         !(tips == 5 & survey_id == "BEN2014MICS"),
         !(tips == 6 & survey_id == "MOZ2015AIS"),
         !(tips == 5 & survey_id == "TGO2017MICS"),
         !(tips == 5 & survey_id == "SLE2017MICS"),
         !(survey_id == "AGO2006MIS" & tips > 5),
         !(survey_id == "TCD2019MICS" & tips > 4),
         !(survey_id == "SWZ2000MICS" & tips > 4),
         !(survey_id == "NGA2021MIS" & tips > 4),
         !(survey_id == "MLI2015MIS" & tips > 5),
         !(survey_id == "MLI2015MICS" & tips > 4),
         !(survey_id == "MLI2009MICS" & tips > 4),
         !(survey_id == "COD2017MICS" & tips > 10),
         !(survey_id == "CAF2006MICS" & tips == 5),
         !(survey_id == "CAF2018MICS" & tips == 5),
         
  ) %>%
  group_by(across(c(iso3, survey_id:period))) %>%
  summarise(pys = sum(pys)) %>%
  group_by(survey_id) %>%
  mutate(pys = median(pys)) %>%
  filter(period %in% range(period)) %>%
  mutate(type = ifelse(period == min(period), "min", "max")) %>%
  select(iso3, survey_id, survtype, period, pys, type) %>%
  pivot_wider(values_from = period, names_from = type) %>%
  mutate(min = min - 0.5,
         max = max + 0.5) %>%
  bind_rows(cal_data %>% filter(survtype == "PHIA") %>% mutate(min = survyear-0.5, max = survyear + 0.5))

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Figs/Fig 1 Survey.png", width = 600, height = 900)
cal_data %>%
  filter(!survey_id %in% remove_survey,
         !survey_id %in% subnational_surveys,
         iso3 != "SSD") %>%
  ggplot(aes(y=fct_rev(country), x=survyear)) +
    geom_point(aes(color = survtype, size = pys, shape = fct_rev(data_type))) +
    no_labels() +
    labs(shape = "Data type", size = "Person-years\n(thousands)", color = "Survey type") +
    scale_size_continuous(labels = scales::label_number(scale=1E-3)) +
    scale_manual("color", 5) +
    standard_theme() +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text = element_text(size = 16),
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 14),
          legend.box = "vertical") +
    guides(color = guide_legend(nrow = 2, override.aes = list(size = 4)),
           size = guide_legend(nrow = 2),
           shape = guide_legend(nrow = 2, override.aes = list(size = 4)))
dev.off()


png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/survey_plot_esa.png", width = 800, height = 1300)
survey_grid %>%
  left_join(region()) %>%
  filter(region == "ESA", iso3 != "SSD") %>%
  ggplot(aes(group = survey_id)) +
  geom_segment(aes(x=min, xend = max, y = survey_id, yend = survey_id, color = survtype), linewidth = 1.3) +
  facet_wrap(~countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot()), drop = TRUE, scales = "free_y", strip.position = "left", shrink = T, ncol = 1) +
  no_labels() +
  labs(color = "Survey type") +
  scale_manual("color", 5) +
  standard_theme() +
  theme(
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    axis.text = element_text(size = 14, face = "bold"),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#f4f4f4", color = NA),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13))
dev.off()

png("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/survey_plot_wca.png", width = 800, height = 1300)
survey_grid %>%
  left_join(region()) %>%
  filter(region == "WCA") %>%
  ggplot(aes(group = survey_id)) +
  geom_segment(aes(x=min, xend = max, y = survey_id, yend = survey_id, color = survtype), linewidth = 1.3) +
  facet_wrap(~countrycode::countrycode(iso3, "iso3c", "country.name", custom_match = cc_plot()), drop = TRUE, scales = "free_y", strip.position = "left", shrink = T, ncol = 1) +
  no_labels() +
  labs(color = "Survey type") +
  scale_manual("color", 5) +
  standard_theme() +
  theme(
    axis.ticks.y = element_blank(),
    strip.background = element_blank(),
    strip.text.y.left = element_text(angle = 0),
    axis.text = element_text(size = 14, face = "bold"),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#f4f4f4", color = NA),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13))
dev.off()

#### TFR and ASFR

library(tidyverse)
library(moz.utils)

lvl <- read_csv("global/iso_mapping_fit.csv", show_col_types = F)

dat <- list.files("outside_orderly/fit/test", full.names = T, all.files = T, recursive = T, pattern = "fr.csv") %>%
  lapply(grep, pattern = "district", value = T) %>%
  unlist() %>%
  lapply(read_csv, show_col_types = F) %>%
  bind_rows() %>%
  filter(iso3 != "SSD") %>%
  filter(iso3 %in% moz.utils::ssa_iso3()[!moz.utils::ssa_iso3() %in% c("COG", "SSD", "CAF", "NGA", "COD", "GHA")])

nm <- unique(dat$iso3)

overlay <- list.files("outside_orderly/asfr/outputs", full.names = T, all.files = T, recursive = T, pattern = "fr_plot.rds") %>%
  lapply(readRDS) %>%
  lapply(bind_rows) %>%
  bind_rows() %>%
  filter(iso3 != "SSD")

phia_fr <- read_csv("global/phia_asfr_admin1.csv", show_col_types = F) %>%
  moz.utils::separate_survey_id(F) %>%
  rename(value = asfr,
         area_id = area_id1)

phia_fr <- phia_fr %>%
  mutate(variable = "asfr") %>%
  bind_rows(phia_fr %>%
              group_by(iso3, survey_id, year, period, area_id) %>%
              summarise(value = 5*sum(value),
                        pys = sum(pys)) %>%
              mutate(variable = "tfr")) %>%
  ungroup()

overlay <- overlay %>%
  bind_rows(phia_fr) %>%
  left_join(distinct(dat, area_id, area_name))

exclude_years <- raw_data %>%
  filter(
    (iso3 == "SWZ" & period == 2017) | ## This seems dodgy
    (period == 1995 & survey_id == "SWZ2000MICS") |
    (period == 1999 & survey_id == "SWZ2014MICS") |
    (period == 2004 & survey_id == "GMB2019DHS") |
    (period == 2005 & survey_id == "GMB2010MICS") |
    (period == 2013 & survey_id == "GMB2018MICS") |
    (tips == 5 & survey_id == "BEN2014MICS") |
      (tips == 6 & survey_id == "MOZ2015AIS") |
      (tips == 5 & survey_id == "TGO2017MICS") |
      (tips == 5 & survey_id == "SLE2017MICS") |
      (survey_id == "AGO2006MIS" & tips > 5) |
      (survey_id == "TCD2019MICS" & tips > 4) |
      (survey_id == "SWZ2000MICS" & tips > 4) |
      (survey_id == "NGA2021MIS" & tips > 4) |
      (survey_id == "MLI2015MIS" & tips > 5) |
      (survey_id == "MLI2015MICS" & tips > 4) |
      (survey_id == "MLI2009MICS" & tips > 4) |
      (survey_id == "COD2017MICS" & tips > 10) |
      (survey_id == "CAF2006MICS" & tips == 5) |
      (survey_id == "CAF2010MICS" & tips == 5) |
      (survey_id == "CAF2018MICS" & tips == 5)
  ) %>%
  distinct(survey_id, period) %>%
  mutate(rem = F)

overlay <- overlay %>%
  left_join(exclude_years) %>%
  filter(is.na(rem))

nm_o <- unique(overlay$iso3)

overlay <- overlay %>%
  arrange(iso3) %>%
  group_by(iso3) %>%
  group_split()

names(overlay) <- nm_o

comp <- lapply(list.files("outside_orderly/fit/triple", full.names = T, all.files = T, recursive = T, pattern = "fr.csv"), read_csv, show_col_types = F) %>% bind_rows()

dat <- dat %>%
  mutate(source = "interactions") %>%
  bind_rows(comp %>% filter(iso3 %in% unique(dat$iso3)) %>% mutate(source = "triple")) %>%
  left_join(lvl) %>%
  filter(area_level == admin1_level | area_level ==0) %>%
  arrange(iso3) %>%
  group_by(iso3) %>%
  group_split()

names(dat) <- nm

remove_survey <- c(
  # "CIV2005AIS",
  # "CIV2006MICS", 
  "GMB2005MICS",
  # "MLI2009MICS", "MLI2015MICS", 
  "SLE2010MICS",
  "TGO2006MICS",
  # "BEN1996DHS",
  "KEN2009MICS", 
  # "COD2017MICS", 
  "NGA2007MICS"
  # "TZA2007AIS", "TZA2012AIS"
)


tfr_plot <- Map(function(dat, overlay) {
  
  country <- unique(dat$iso3)
  
  if(unique(dat$iso3) == "ZAF")
    val <- 5
  else
    val <- 10
  
  dat %>%
    filter(variable == "tfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line(aes(color = source)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = source), alpha = 0.3) +
    geom_point(data = overlay %>% filter(variable == "tfr", value < val, !survey_id %in% remove_survey), aes(y=value, size = pys)) +
    geom_point(data = overlay %>% filter(variable == "tfr", value < val, survey_id %in% remove_survey), aes(y=value, size = pys), shape=22, fill=NA) +
    guides(size = "none") +
    facet_wrap(~fct_inorder(area_name)) +
    standard_theme() +
    no_labels() +
    labs(title = country)
  
}, dat = dat, overlay = overlay[names(dat)])

# pdf("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/TFR.pdf", width = 15, height = 10)
pdf("~/Downloads/TFR_test3.pdf", width = 15, height = 10)
tfr_plot
dev.off()

asfr_plot <- Map(function(dat, overlay) {
  
  country <- unique(dat$iso3)
  
  dat %>%
    filter(variable == "asfr") %>%
    ggplot(aes(x=period, y=median)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    geom_point(data = overlay %>% filter(variable == "asfr", value <0.5, !survey_id %in% remove_survey), aes(y=value, color=survey_id, size = pys)) +
    geom_point(data = overlay %>% filter(variable == "asfr", value <0.5, survey_id %in% remove_survey), aes(y=value, color=survey_id, size = pys), shape=22, fill=NA) +
    facet_wrap(~age_group) +
    standard_theme() +
    no_labels() +
    labs(title = country)
  
}, dat = dat, overlay = overlay[names(dat)])

pdf("~/OneDrive - Imperial College London/Phd/Fertility/2023-12/Supplementary materials/ASFR.pdf", width = 15, height = 10)
asfr_plot
dev.off()