library(tidyverse)
library(sf)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode::countrycode(ssa_names, "country.name", "iso3c")

mapping <- read.csv("global/iso_mapping_fit.csv")

id <- lapply(ssa_iso3[!ssa_iso3 %in% c("SSD", "ETH", "GNQ", "BWA", "MWI", "RWA", "GNB", "CAF","SSD")], function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

id2 <- lapply(c("RWA", "MWI"), function(x){
  orderly::orderly_search(name = "aaa_mwi_rwa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

id3 <- lapply("ETH", function(x){
  orderly::orderly_search(name = "aaa_eth_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
})

dat <- lapply(paste0("archive/aaa_fit/", id, "/fr.csv"), read_csv, show_col_types = FALSE)
dat2 <- lapply(paste0("archive/aaa_mwi_rwa_fit/", id2, "/fr.csv"), read_csv, show_col_types = FALSE)
dat3 <- lapply(paste0("archive/aaa_eth_fit/", id3, "/fr.csv"), read_csv, show_col_types = FALSE)
dat <- c(dat, dat2, dat3)

fr_plot <- lapply(file.path("archive/aaa_fit", id, "depends/fr_plot.csv"), read.csv)
fr_plot2 <- lapply(file.path("archive/aaa_mwi_rwa_fit", id2, "depends/fr_plot.csv"), read.csv)
fr_plot3 <- lapply(file.path("archive/aaa_eth_fit", id3, "depends/fr_plot.csv"), read.csv)
fr_plot <- c(fr_plot, fr_plot2, fr_plot3)

pop <- lapply(file.path("archive/aaa_fit", id, "depends/interpolated_population.csv"), read.csv)
pop2 <- lapply(file.path("archive/aaa_mwi_rwa_fit", id2, "depends/interpolated_population.csv"), read.csv)
pop3 <- lapply(file.path("archive/aaa_eth_fit", id3, "depends/interpolated_population.csv"), read.csv)
pop <- c(pop, pop2, pop3)

pop <- pop %>% 
  bind_rows() %>%
  filter(year == 2015,
         sex == "female") %>%
  moz.utils::five_year_to_15to49("population")

# fr <- c(fr, "COD" = list(read.csv("archive/cod_fit_new/20210922-172127-8f9e6cd3/fr.csv")))

region <- read.csv("global/region.csv") %>%
  mutate(iso3 = toupper(iso3))

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

id_input <- lapply(names(fr_list), function(x){
  orderly::orderly_search(name = "aaa_areas_pull", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
}) %>% 
  setNames(names(fr_list))

areas_list <- lapply(file.path("archive/aaa_areas_pull", id_input, "naomi_areas.geojson"), sf::read_sf) %>%
  setNames(names(fr_list))

# areas_list <- c(areas_list, "COD" = list(read_sf("archive/cod_data_areas/20201112-171234-2710c17f/cod_areas.geojson")))

# areas_list <- areas_list[order(names(areas_list))]

foo <- read_sf("~/Downloads/Longitude_Graticules_and_World_Countries_Boundaries-shp/99bfd9e7-bb42-4728-87b5-07f8c8ac631c2020328-1-1vef4ev.lu5nk.shp") %>%
  filter(CNTRY_NAME %in% c("Botswana", "Western Sahara", "Mauritania", "Morocco", "Algeria", "Libya", "Tunisia", "Egypt", "Equatorial Guinea", "Somalia", "Djibouti", "Eritrea")) %>%
  bind_rows(read_sf("~/Downloads/sdn_adm_cbs_nic_ssa_20200831_shp/sdn_admbnda_adm0_cbs_nic_ssa_20200831.shp"))



nat_geom <- lapply(areas_list, function(x) {
  x %>%
    filter(area_level == 0) %>%
    select(area_id, geometry)
}) %>%
  bind_rows

dist_fr <- fr_list %>%
  Map(function(fr, areas, level) {
  
  fr %>%
    filter(area_level == level,
           variable == "tfr"
    ) %>%
    select(iso3, area_id, area_name, period, median) %>%
    left_join(areas) %>%
    select(iso3, area_id, area_name, period, median, geometry)
  
  
}, fr = ., areas_list[names(.)], lvls)

p1a <- dist_fr %>%
  bind_rows() %>%
  filter(period == 2015) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill=median), size=0) +
  geom_sf(data = foo, aes(geometry = geometry), fill="grey", size=0.3) +
  geom_sf(data = nat_geom, aes(geometry = geometry), fill=NA, size=0.3) +
  labs(fill = "TFR")+
  viridis::scale_fill_viridis() +
  theme_minimal() +
  coord_sf(datum = NA)

dist_15 <- dist_fr %>%
  bind_rows() %>%
  filter(period == 2015)

nat_15 <- fr %>%
  filter(area_level ==0, period == 2015) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) 

dist_15 %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  left_join(region) %>%
  left_join(pop %>% ungroup() %>% select(area_id, population)) %>%
  group_by(iso3) %>%
  mutate(population = population/sum(population)) %>%
  ggplot(aes(x=iso3, y=median)) +
    geom_jitter(aes(color=region, size = population), shape=20, width =0.1, alpha = 0.5) +
    geom_point(data=nat_15 %>% filter(variable == "tfr"), shape = 21, size = 3, fill = "white", col = "black", alpha = 0.9) +
  standard_theme() +
    labs(y="TFR", x=element_blank()) +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

###

nat_decline <- fr %>%
  filter(area_level ==0,
         variable == "tfr",
         period > 1999) %>%
  group_by(iso3, region) %>%
  summarise(decline = median[period == 2020]/median[period == 2000])

1-quantile(nat_decline$decline, c(0.25, 0.5, 0.75))
1-quantile(filter(nat_decline, region == "ESA")$decline, c(0.25, 0.5, 0.75))
1-quantile(filter(nat_decline, region == "WCA")$decline, c(0.25, 0.5, 0.75))

###

ntl_change <- fr %>%
  filter(area_level ==0,
         variable == "tfr") %>%
  group_by(iso3) %>%
  mutate(ntl_change = median/median[period == 2000]) %>%
  select(iso3, period, ntl_change)

dist_fr %>%
  bind_rows() %>%
  select(-geometry) %>%
  filter(period > 1999) %>%
  group_by(area_id) %>%
  mutate(dist_change = median/median[period == 2000]) %>%
  left_join(ntl_change) %>%
  mutate(dist_ntl_change_ratio = dist_change/ntl_change) %>%
  ggplot(aes(x=period,y=dist_ntl_change_ratio-1, group=area_id)) +
    geom_line(alpha = 0.1) +
    scale_y_continuous(labels = scales::label_percent()) +
    standard_theme() +
    labs(x=element_blank(), y="District change relative to national change")

#####

p <- fr %>%
  left_join(mapping %>% select(iso3, fertility_fit_level)) %>%
  filter(area_level %in% c(0, fertility_fit_level)) %>%
  group_by(iso3) %>%
  group_split() %>%
  lapply(function(x) {
    
    ntl_change <- x %>%
      filter(area_level ==0,
             variable == "tfr",
             period > 1999) %>%
      group_by(iso3) %>%
      mutate(ntl_change = median/median[period == 2000]) %>%
      select(iso3, period, ntl_change)
    
    x %>%
      filter(period > 1999,
             variable == "tfr") %>%
      select(iso3, area_id, period, median) %>%
      group_by(area_id) %>%
      mutate(dist_change = median/median[period == 2000]) %>%
      left_join(ntl_change) %>%
      mutate(dist_ntl_change_ratio = dist_change/ntl_change) %>%
      ggplot(aes(x=period,y=dist_ntl_change_ratio-1, group=area_id)) +
      geom_line(alpha = 0.1) +
      scale_y_continuous(labels = scales::label_percent()) +
      standard_theme() +
      labs(x=element_blank(), y="District change relative to national change", title = unique(ntl_change$iso3))
    
  })

fr %>%
  filter(iso3 == "UGA",
         variable == "tfr",
         area_level == 3,
         area_id != "UGA_3_022") %>%
  ggplot(aes(x=period, y=median, group = area_id)) +
    geom_line(alpha = 0.3) +
    geom_line(data = fr %>%
                filter(iso3 == "UGA",
                       variable == "tfr",
                       area_level == 0),
              size=2) +
  geom_line(data = fr %>%
              filter(variable == "tfr",
                     area_id == "UGA_3_022"),
            color="red",
            size=1)

#TZA SEN RWA MWI ETH
fr %>%
  filter(iso3 == "TZA",
         variable == "tfr",
         area_level == 4,
         # str_detect(area_id, "TZA_4_12|4_14"),
         !area_id %in% c("TZA_4_072dv", "TZA_4_048rr", "TZA_4_122um", "TZA_4_145tt")
         ) %>%
  ggplot(aes(x=period, y=median, group = area_id)) +
  geom_line(alpha = 0.2) +
  geom_line(data = fr %>%
              filter(iso3 == "TZA",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(variable == "tfr",
                     area_id %in% c("TZA_4_072dv", "TZA_4_048rr", "TZA_4_122um", "TZA_4_145tt")),
            aes(color=area_id),
            size=1.5)


  geom_line(data = fr %>%
              filter(iso3 == "UGA",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(variable == "tfr",
                     area_id == "UGA_3_022"),
            color="red",
            size=1)
    

p3a <- fr %>%
  filter(area_level == 0,
        variable == "asfr",
        period %in% c(2000, 2010, 2020),
        # !iso3 %in% c("NER", "GAB", "GIN", "MWI")
        ) %>%
  group_by(region, area_id, area_name, age_group) %>%
  summarise(ratio_10 = median[period == 2010]/median[period == 2000],
            ratio_20 = median[period == 2020]/median[period == 2010]) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label)) %>%
  pivot_longer(-c(region, area_id, area_name, age_group, age_group_label)) %>%
  mutate(time = factor(name, labels=c("2000-2010", "2010-2020")),
         name = paste0(region, " | ", time)) %>%
  ggplot(aes(x=age_group_label, y=value-1)) +
    geom_boxplot() +
    scale_y_continuous(labels = scales::label_percent(), name = "Fertility reduction") +
    geom_hline(aes(yintercept = 0), linetype=3) +
    facet_wrap(~time) +
    theme_minimal() +
    labs(x=element_blank(), title = "(A)") +
    theme(strip.text = element_text(size=12),
          axis.text = element_text(size=12),
          axis.title = element_text(size=14),
          panel.background = element_rect(fill=NA, color="black"))

fr %>%
  filter(area_level == 0,
         variable == "asfr",
         period %in% c(2000, 2010, 2020)) %>%
  group_by(region, area_id, area_name, age_group) %>%
  summarise(`2000` = 1,
            `2010` = median[period == 2010]/median[period == 2000],
            `2020` = median[period == 2020]/median[period == 2000]) %>%
  pivot_longer(-c(region, area_id, area_name, age_group)) %>%
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
         period %in% c(2000, 2005, 2010, 2015, 2020),
         region == "ESA") %>%
  group_by(region, area_id, area_name, age_group) %>%
  summarise(`2005` = 1,
            `2015` = median[period == 2015]/median[period == 2005]) %>%
  pivot_longer(-c(region, area_id, area_name, age_group)) %>%
  type.convert() %>%
  # mutate(name = factor(name, labels=c("2000-2010", "2010-2020")) %>%
  ggplot(aes(x=name, y=value-1, group=fct_rev(age_group), color=age_group))+
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent(), name = "Fertility reduction") +
  scale_color_manual(values=c("red", rep("grey", 6))) +
  geom_hline(aes(yintercept = 0), linetype=3) +
  facet_wrap(~area_name, ncol=5)

fr %>%
  filter(area_level == 0,
         variable == "asfr",
         period %in% c(2000, 2005, 2010, 2015, 2020)) %>%
  group_by(region, area_id, area_name, age_group) %>%
  summarise(`2005` = 1,
            `2020` = median[period == 2015]/median[period == 2005]) %>%
  pivot_longer(-c(region, area_id, area_name, age_group)) %>%
  type.convert() %>%
  filter(name == 2020) %>%
  group_by(area_id) %>%
  arrange(value, .by_group = TRUE) %>%
  mutate(n = row_number()) %>%
  filter(age_group == "Y015_019") %>%
  left_join(nat_geom) %>%
  ggplot() +
    geom_sf(aes(geometry = geometry, fill=n))

fr %>%
  filter(area_level == 0,
         variable == "tfr",
         iso3 != "GMB") %>%
  group_by(region, area_id, area_name) %>%
  mutate(rel_tfr = median/median[period==2000]) %>%
  filter(period >1999) %>%
  select(area_id, area_name, period, rel_tfr) %>%
  ggplot(aes(x=period, y=rel_tfr, group=area_name)) +
    geom_line(show.legend = FALSE, alpha=0.6) +
    geom_hline(aes(yintercept=1), linetype=2, color="red")+
    scale_y_continuous(labels = scales::label_percent()) +
    labs(y="Relative change in TFR", x=element_blank())+
    moz.utils::standard_theme()


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



age_distribution_plots <- lapply(fr_list, function(fr) {
  fr %>%
    filter(variable == "asfr",
           period %in% c(2000, 2010, 2020),
           area_level == 1) %>%
    ggplot(aes(x=desc(period), y=median, fill=age_group)) +
    geom_col(position = "fill")  +
    facet_wrap(~area_name) +
    coord_flip()
})
# 
# pdf(paste0("~/Downloads/age_distribution.pdf"), h = 12, w = 20)
# age_distribution_plots
# dev.off()


fr_list[["TZA"]] %>%
  filter(variable == "asfr",
         area_level == 1) %>%
  ggplot(aes(x=period, y=median, color=area_name)) +
    geom_line() +
    facet_wrap(~age_group)

asfr_time <- lapply(fr_list, function(fr) {
  fr %>%
    filter(variable == "asfr",
           area_level == 1) %>%
    ggplot(aes(x=period, y=median, color=age_group)) +
      geom_line() +
      facet_wrap(~area_name)
})

p3b <- fr_list[["LSO"]] %>%
  filter(variable == "asfr",
         area_level == 0,
         period > 1999) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label)) %>%
  ggplot(aes(x=period, y=median)) +
    geom_line(show.legend = FALSE, size=1.1, aes(color=age_group_label)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = age_group_label), alpha = 0.5, show.legend = FALSE) +
    geom_label(data = . %>% filter(period == max(period)), aes(color =age_group_label, label = age_group_label), nudge_x = 2, size=5, show.legend = FALSE) +
    moz.utils::standard_theme() +
    labs(y = "ASFR", x=element_blank(), title="(B)") +
  coord_cartesian(clip="off") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = unit(c(1,1,1,0), "lines"))

ggarrange(p3a, p3b, widths = c(2,1))

plots <- Map(function(fr, areas, level) {
  
  fr %>%
    filter(area_level == level,
           period == 2016,
           variable == "tfr"
    ) %>%
    select(-any_of(c("epp_level", "spectrum_region_code"))) %>%
    left_join(areas) %>%
    ggplot() +
      geom_sf(aes(geometry = geometry, fill=median))
      # viridis::scale_fill_viridis()
}, fr_list, areas_list[names(fr_list)], lvls)

asfr_list_admin1 <- fr %>%
  filter(variable == "asfr",
         period > 1999) %>%
  group_split(iso3)

tfr_list_admin1 <- fr %>%
  filter(variable == "tfr",
         period > 1999) %>%
  group_split(iso3)

asfr_plots_admin1 <- lapply(asfr_list_admin1, function(x) {
  
  country <- unique(filter(x, area_level == 0)$area_name)
  
  x %>%
    filter(area_level == 1) %>%
    ggplot(aes(x=period, y=median, group=age_group)) +
      geom_line(aes(color=age_group)) +
      geom_ribbon(aes(ymin=lower, ymax=upper, fill=age_group), alpha = 0.3) +
      facet_wrap(~area_name) +
      labs(title = country)
})

names(asfr_plots_admin1) <- unique(fr$iso3)

tfr_plots_admin1 <- lapply(tfr_list_admin1, function(x) {
  
  country <- unique(filter(x, area_level == 0)$area_name)
  
  x %>%
    filter(area_level == 1) %>%
    ggplot(aes(x=period, y=median)) +
    geom_line() +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha = 0.3) +
    facet_wrap(~area_name) +
    labs(title = country)
})

names(tfr_plots_admin1) <- unique(fr$iso3)

pdf(paste0("~/Downloads/asfr_admin1.pdf"), h = 12, w = 20)
asfr_plots_admin1
dev.off()

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
  
  
  
}, fr_list, areas_list[names(fr_list)], lvls)

admin1_fr_change <- Map(function(fr, areas, level) {
  
  country <- unique(filter(fr, area_level == 0)$area_name)
  
  fr %>%
    filter(area_level == 1,
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

names(dist_fr_change) <- names(fr_list)
names(admin1_fr_change) <- names(fr_list)
names(tfr_2020_map) <- names(fr_list)

pdf(paste0("~/Downloads/dist_fr_change_00_20.pdf"), h = 12, w = 20)
dist_fr_change
dev.off()

ggpubr::ggarrange(tfr_2020_map[["ZWE"]], dist_fr_change[["ZWE"]])

ggpubr::ggarrange(dist_fr_change[["ETH"]], dist_fr_change[["MOZ"]])
ggpubr::ggarrange(admin1_fr_change[["ETH"]], admin1_fr_change[["MOZ"]])

dist_fr[["ETH"]] %>%
  ggplot(aes(x=period, y=median, color=area_id)) +
    geom_line()


lvl_df <- read.csv("global/iso_mapping_fit.csv") %>%
  select(iso3, fertility_fit_level, admin1_level)

admin1_tfr <- fr %>%
  left_join(lvl_df) %>%
  filter(area_level == admin1_level, period == 2015, variable == "tfr")

dist_tfr_comparison <- fr %>%
  left_join(lvl_df) %>%
  filter(area_level == fertility_fit_level, period == 2015, variable == "tfr")
  
dist_fr_comparison <- fr %>%
    left_join(lvl_df) %>%
    filter(area_level == fertility_fit_level | area_level == admin1_level, period == 2015, variable == "tfr") %>%
    group_split(iso3)

names(dist_fr_comparison) <- names(areas_list)

int <- dist_fr_comparison %>%
  Map(function(x, areas) {
    
    admin1_level <- unique(x$admin1_level)
    admin1_id = paste0("area_id", admin1_level)
    
    areas_wide <- spread_areas(areas)
    
    int <- x %>%
      filter(area_level == fertility_fit_level) %>%
      left_join(areas_wide %>%
                  rename("admin1_id" = admin1_id)) %>%
      select(iso3, area_id, median, admin1_id)
    
    int %>%
      left_join(
        x %>%
          filter(area_level == admin1_level) %>%
          select(area_id, parent_median = median),
        by=c("admin1_id" = "area_id")
      )
    
  }, ., areas_list[names(.)])

int %>%
  bind_rows() %>%
  filter(!iso3 %in% c("MLI", "TCD", "KEN", "CAF")) %>%
  mutate(parent_ratio = median/parent_median,
         country_name = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  group_by(iso3) %>%
  mutate(lower = quantile(parent_ratio, 0.25),
         upper = quantile(parent_ratio, 0.75),
         diff = upper-lower) %>%
  ungroup() %>%
  mutate(country_name = fct_reorder(country_name, diff, min)) %>%
  ggplot(aes(x=country_name, y=parent_ratio)) +
    geom_boxplot() +
    geom_hline(aes(yintercept=1), linetype = 3, color="red") +
    labs(x=element_blank(), y="District TFR relative to its admin-1 area") +
    coord_flip() +
    moz.utils::standard_theme()

int %>%
  bind_rows() %>%
  filter(!iso3 %in% c("MLI", "TCD", "KEN", "CAF")) %>%
  mutate(parent_ratio = median/parent_median,
         country_name = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  group_by(iso3) %>%
  summarise(lower = quantile(parent_ratio, 0.25),
         upper = quantile(parent_ratio, 0.75),
         diff = upper-lower)  %>%
  filter(iso3 %in% c("GHA", "COG", "ETH"))

############

fig2a <- dist_fr_change[["BEN"]] + 
  labs(fill = "Proportion change", title = "(A)") +
  facet_wrap(~name, nrow=1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(datum = NA) +
  theme(strip.text = element_text(size = 12),
        plot.margin = unit(c(1,0,0,0), "lines"))

fig2b <- dist_fr[["ETH"]] %>%
  group_by(area_id) %>%
  filter(period > 1999) %>%
  mutate(increase = ifelse(median[period == 2010] > median[period == 2000], 1, 0)) %>%
  ggplot(aes(x=period, y=median, group = area_id, color=factor(increase))) + 
    geom_line(show.legend = FALSE, aes(size = factor(increase), alpha = factor(increase))) +
    scale_size_manual(values = c(0.7, 1.3)) +
    scale_alpha_manual(values = c(0.3, 1)) +
    theme_minimal() +
    labs(y="TFR", x=element_blank(), title = "(B)") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12),
          plot.margin = unit(c(1,1,1,0), "lines"))

fig2c <- dist_fr[["ETH"]] %>%
  group_by(area_id) %>%
  filter(period > 1999) %>%
  mutate(increase = ifelse(median[period == 2020] > median[period == 2010], 1, 0)) %>%
  ggplot(aes(x=period, y=median, group = area_id, color=factor(increase))) + 
  geom_line(show.legend = FALSE, aes(size = factor(increase), alpha = factor(increase))) +
  scale_size_manual(values = c(0.7, 1.3)) +
  scale_alpha_manual(values = c(0.3, 1)) +
  theme_minimal() +
  labs(y="TFR", x=element_blank(), title = "(C)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        plot.margin = unit(c(1,1,1,0), "lines"))

ggpubr::ggarrange(fig2a, ggpubr::ggarrange(fig2b, fig2c, ncol=1))  

#################

dist_fr_change[["ETH"]] + 
  labs(fill = "Proportion change", title = "(A)") +
  facet_wrap(~name, nrow=1) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  coord_sf(datum = NA) +
  theme(strip.text = element_text(size = 12),
        plot.margin = unit(c(1,0,0,0), "lines"))

fr %>% 
  filter(iso3 == "ETH", variable == "tfr", area_level == 2, period %in% c(2000,2010,2020)) %>% 
  group_by(area_id) %>% 
  summarise(ratio_10 = median[period == 2020]/median[period==2010]) %>% 
  filter(ratio_10 == max(ratio_10) |ratio_10 == min(ratio_10))

fr %>% 
  filter(area_id == "ETH", variable == "tfr",period %in% c(2000,2010,2020)) %>% 
  group_by(area_id) %>% 
  summarise(ratio_10 = median[period == 2020]/median[period==2010]) %>% 
  filter(ratio_10 == max(ratio_10) |ratio_10 == min(ratio_10))

####################

fr %>%
  filter(area_level == 0, variable == "tfr") %>%
  group_by(area_id) %>%
  summarise(change = median[period == 2015]/median[period==2000]) %>%
  ungroup() %>%
  summarise(median = median(change),
            lower = quantile(change, 0.25),
            upper = quantile(change, 0.75)
  )

fr %>%
  filter(area_level == 0,
         variable == "asfr",
         period %in% c(2000, 2010, 2020),
         !iso3 %in% c("NER", "GAB", "GIN", "MWI")) %>%
  group_by(region, area_id, area_name, age_group) %>%
  summarise(ratio_10 = median[period == 2010]/median[period == 2000],
            ratio_20 = median[period == 2020]/median[period == 2010]) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label)) %>%
  pivot_longer(-c(region, area_id, area_name, age_group, age_group_label)) %>%
  group_by(name, age_group_label) %>%
  summarise(median = median(value),
            lower = quantile(value, 0.25),
            upper = quantile(value, 0.75)
  )

##############

survey <- lapply(id, function(x) {
  read.csv(paste0("archive/aaa_fit/", x, "/depends/fertility_asfr.csv"))
})

survey %>%
  bind_rows() %>%
  distinct(survey_id, survtype) %>%
  filter(survtype != "MICS")

mics_asfr <- read.csv("global/mics_asfr.csv") %>%
  distinct(survey_id)

mics_asfr
