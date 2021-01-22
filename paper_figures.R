library(tidyverse)
library(sf)

folders <- list.files("archive", full.names = TRUE)
fits <- grep("_fit", folders, value=TRUE)
fits <- lapply(fits, list.files, full.names=TRUE)
fits <- lapply(fits, tail, n=1)
fits <- lapply(fits, list.files, full.names= TRUE)
fits <- lapply(fits, grep, pattern="_fr.csv", value=TRUE)
fr <- lapply(fits, read.csv)

fr <- fr %>% 
  bind_rows() %>%
  separate(area_id, into=c("iso3", NA), remove=FALSE, sep=3)

fr %>%
  filter(area_level == 0,
        variable == "asfr",
        period %in% c(2000, 2010, 2020)) %>%
  group_by(area_id, area_name, age_group) %>%
  summarise(ratio_10 = median[period == 2010]/median[period == 2000],
            ratio_20 = median[period == 2020]/median[period == 2010]) %>%
  pivot_longer(-c(area_id, area_name, age_group)) %>%
  mutate(name = factor(name, labels=c("2000-2010", "2010-2020"))) %>%
  ggplot(aes(x=age_group, y=value-1)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::label_percent(), name = "Fertility reduction") +
    geom_hline(aes(yintercept = 0), linetype=3) +
    facet_wrap(~name)

fr %>%
  filter(area_level == 0,
         variable == "asfr",
         period %in% c(2000, 2010, 2020)) %>%
  group_by(area_id, area_name, age_group) %>%
  summarise(`2000` = 1,
            `2010` = median[period == 2010]/median[period == 2000],
            `2020` = median[period == 2020]/median[period == 2000]) %>%
  pivot_longer(-c(area_id, area_name, age_group)) %>%
  type.convert() %>%
  # mutate(name = factor(name, labels=c("2000-2010", "2010-2020")) %>%
  ggplot(aes(x=name, y=value-1, group=age_group, color=age_group))+
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::label_percent(), name = "Fertility reduction") +
  scale_color_manual(values=c("red", rep("grey", 6))) +
  geom_hline(aes(yintercept = 0), linetype=3) +
  facet_wrap(~area_name)


## 15-19 flatlining between 2010-2020
fr %>%
  filter(area_level == 0,
         variable == "asfr",
         period %in% c(2000, 2010, 2020)) %>%
  group_by(area_id, area_name, age_group) %>%
  summarise(`2010` = 1,
            `2020` = median[period == 2020]/median[period == 2010]) %>%
  pivot_longer(-c(area_id, area_name, age_group)) %>%
  type.convert() %>%
  # mutate(name = factor(name, labels=c("2000-2010", "2010-2020")) %>%
  ggplot(aes(x=name, y=value-1, group=fct_rev(age_group), color=age_group))+
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent(), name = "Fertility reduction") +
  scale_color_manual(values=c("red", rep("grey", 6))) +
  geom_hline(aes(yintercept = 0), linetype=3) +
  facet_wrap(~area_name)


# fr %>%
#   filter(area_level == 0,
#          variable == "tfr",
#          period %in% c(2000, 2010, 2020)) %>%
#   group_by(area_id, area_name) %>%
#   summarise(ratio_10 = median[period == 2010]/median[period == 2000],
#             ratio_20 = median[period == 2020]/median[period == 2010]) %>%
#   pivot_longer(-c(area_id, area_name)) %>%
#   mutate(name = factor(name, labels=c("2000-2010", "2010-2020"))) %>%
#   ggplot(aes(x=name, y=value-1)) +
#   geom_boxplot() +
#   geom_point(position = position_jitter(width=0.05)) +
#   scale_y_continuous(labels = scales::label_percent(), name = "Fertility reduction") +
#   geom_hline(aes(yintercept = 0), linetype=3)

fr_list <- fr %>%
  group_split(iso3)

lvls <- fr %>%
  group_by(iso3) %>%
  filter(area_level == max(area_level)) %>%
  select(iso3, area_level) %>%
  distinct %>%
  .$area_level

areas_l <- grep("_data_areas", folders, value=TRUE)
areas_l <- lapply(tolower(unique(fr$iso3)), grep, areas_l, value=TRUE) %>% unlist
areas_l <- lapply(areas_l, list.files, full.names=TRUE)
areas_l <- lapply(areas_l, tail, n=1)
areas_l <- lapply(areas_l, list.files, full.names= TRUE)
areas_l <- lapply(areas_l, grep, pattern="_areas.geojson", value=TRUE)
areas_list <- lapply(areas_l, read_sf)

names(areas_list) <- unique(fr$iso3)

# age_distribution_plots <- lapply(fr_list, function(fr) {
#   fr %>%
#     filter(variable == "asfr",
#            period == 2020,
#            area_level == 1) %>%
#     ggplot(aes(x=area_name, y=median, fill=age_group)) +
#     geom_col(position = "fill")  +
#     coord_flip()
# })
# 
# pdf(paste0("~/Downloads/age_distribution.pdf"), h = 12, w = 20)
# age_distribution_plots
# dev.off()

plots <- Map(function(fr, areas, level) {
  
  fr %>%
    filter(area_level == level,
           variable == "tfr"
    ) %>%
    select(-epp_level) %>%
    left_join(areas) %>%
    ggplot() +
      geom_sf(aes(geometry = geometry, fill=median)) +
      viridis::scale_fill_viridis()
}, fr_list, areas_list, lvls)

fr_list_admin1 <- fr %>%
  filter(variable == "asfr",
         period > 1999) %>%
  group_split(iso3)

plots_admin1 <- lapply(fr_list_admin1, function(x) {
  
  country <- unique(filter(x, area_level == 0)$area_name)
  
  x %>%
    filter(area_level == 1) %>%
    ggplot(aes(x=period, y=median, group=age_group)) +
      geom_line(aes(color=age_group)) +
      geom_ribbon(aes(ymin=lower, ymax=upper, fill=age_group), alpha = 0.3) +
      facet_wrap(~area_name) +
      labs(title = country)
})

pdf(paste0("~/Downloads/test.pdf"), h = 12, w = 20)
plots_admin1
dev.off()


dist_fr <- Map(function(fr, areas, level) {
  
  fr %>%
    filter(area_level == level,
           variable == "tfr"
    ) %>%
    select(iso3, area_id, area_name, period, median) %>%
    left_join(areas) %>%
    select(iso3, area_id, area_name, period, median, geometry)

  
}, fr_list, areas_list, lvls)

tfr_2020_map <- lapply(dist_fr, function(fr) {
  
  fr %>%
    filter(period == 2020) %>%
    ggplot() +
      geom_sf(aes(geometry = geometry, fill=median)) +
      viridis::scale_fill_viridis()
})

pdf(paste0("~/Downloads/2020_tfr.pdf"), h = 12, w = 20)
tfr_2020_map
dev.off()


dist_fr %>%
  bind_rows() %>%
  filter(period == 2020) %>%
  ggplot() +
    geom_sf(aes(geometry = geometry, fill=median), size=0) +
    viridis::scale_fill_viridis()

## Box plot of TFR distributions
dist_fr %>%
  bind_rows() %>%
  filter(period %in% c(2000,2010,2020)) %>%
  mutate(period = factor(period)) %>%
  ggplot(aes(x=iso3, y=median, fill=period)) +
    geom_boxplot()

dist_fr %>%
  bind_rows() %>%
  filter(period %in% c(2020)) %>%
  mutate(period = factor(period)) %>%
  ggplot(aes(x=iso3, y=median)) +
  geom_boxplot() +
  labs(y="TFR", x=element_blank())

dist_fr %>%
  bind_rows() %>%
  # filter(iso3 == "ETH") %>%
  group_by(iso3, area_id, area_name) %>%
  summarise(
    # ratio_05 = median[period == 2005]/median[period == 2000],
    # ratio_10 = median[period == 2010]/median[period == 2005],
    # ratio_15 = median[period == 2015]/median[period == 2010],
    # ratio_20 = median[period == 2020]/median[period == 2015],

    ratio_10 = median[period == 2010]/median[period == 2000],
    ratio_20 = median[period == 2020]/median[period == 2010]
    
    ) %>%
  pivot_longer(-c(iso3, area_id, area_name)) %>%
  # left_join(areas_list[["ETH"]]) %>%
  # mutate(name = factor(name, labels=c("2000-2005", "2005-2010","2010-2015","2015-2020"))) %>%
  mutate(name = factor(name, labels=c("2000-2010", "2010-2020"))) %>%
  ggplot(aes(x=iso3, y=value, fill=name)) +
  geom_boxplot()
  # facet_wrap(~name)

dist_fr_change <- Map(function(fr, areas, level) {
  
  country <- unique(filter(fr, area_level == 0)$area_name)
  
  fr %>%
    filter(area_level == level,
           variable == "tfr") %>%
    group_by(iso3, area_id, area_name) %>%
    summarise(ratio_10 = median[period == 2020]/median[period == 2000],
              ratio_20 = median[period == 2020]/median[period == 2010]
              ) %>%
    pivot_longer(-c(iso3, area_id, area_name)) %>%
    left_join(areas) %>%
    mutate(name = factor(name, labels=c("2000-2010", "2010-2020"))) %>%
    ggplot() +
    geom_sf(aes(geometry = geometry, fill=value)) +
    scale_fill_gradient2(midpoint=1) +
    facet_wrap(~name, ncol=1) +
    labs(title = country)
  
  
  
}, fr_list, areas_list, lvls)

pdf(paste0("~/Downloads/dist_fr_change_00_20.pdf"), h = 12, w = 20)
dist_fr_change
dev.off()

ggpubr::ggarrange(tfr_2020_map[[5]], dist_fr_change[[5]])

