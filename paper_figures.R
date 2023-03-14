library(tidyverse)
library(sf)

ssa_names <- c("Angola", "Botswana", "Eswatini", "Ethiopia", "Kenya", "Lesotho",  "Malawi", "Mozambique", "Namibia", "Rwanda", "South Africa", "South Sudan", "Uganda", "United Republic of Tanzania", "Zambia", "Zimbabwe", "Benin", "Burkina Faso", "Burundi", "Cameroon", "Central African Republic", "Chad", "Congo", "Côte d'Ivoire", "Democratic Republic of the Congo", "Equatorial Guinea", "Gabon", "Gambia", "Ghana", "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Niger", "Nigeria", "Senegal", "Sierra Leone", "Togo")
ssa_iso3 <- countrycode::countrycode(ssa_names, "country.name", "iso3c")

mapping <- read.csv("global/iso_mapping_fit.csv")

id <- lapply(ssa_iso3[!ssa_iso3 %in% c("GNQ", "BWA")], function(x){
  orderly::orderly_search(name = "aaa_fit", query = paste0('latest(parameter:iso3 == "', x, '")'), draft = FALSE)
}) 

dat <- lapply(paste0("archive/aaa_fit/", id, "/fr.csv"), read_csv, show_col_types = FALSE)

fr_plot <- lapply(file.path("archive/aaa_fit", id, "depends/fr_plot.csv"), read.csv)

pop <- lapply(file.path("archive/aaa_fit", id, "depends/interpolated_population.csv"), read.csv)

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

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

age_dist <- fr %>%
  left_join(mapping) %>% 
  filter(area_level == admin1_level,
         variable == "asfr",
         period == 2015) %>%
  group_by(area_id) %>%
  mutate(median = median/median[age_group == "Y025_029"]) %>%
  filter(age_group == "Y020_024") %>%
  left_join(areas_list %>% lapply(select, -any_of("spectrum_region_code")) %>% bind_rows() %>% select(area_id)) %>%
  group_by(iso3) %>%
  group_split() %>%
  lapply(function(x) {
    c <- countrycode::countrycode(unique(x$iso3), "iso3c", "country.name")
    x %>%
      ggplot() +
        geom_sf(aes(geometry = geometry, fill=1-median)) +
        scale_fill_gradient2(midpoint=0, low =  wesanderson::wes_palette("Zissou1")[1],
                             high = wesanderson::wes_palette("Zissou1")[5],
                             breaks = seq(-0.1, 0.3, 0.1), 
                             labels = scales::label_percent(prefix = "+")) +
        labs(title = c, fill="25-29 ASFR relative to 20-24") +
      theme_minimal()+
      coord_sf(datum=NA)
      
  }) %>% setNames(unique(fr$iso3))

gmb_sen <- fr %>%
  left_join(mapping) %>% 
  filter(area_level == admin1_level,
         variable == "asfr",
         period == 2015) %>%
  group_by(area_id) %>%
  mutate(median = median/median[age_group == "Y025_029"]) %>%
  filter(age_group == "Y020_024") %>%
  left_join(areas_list %>% lapply(select, -any_of("spectrum_region_code")) %>% bind_rows() %>% select(area_id)) %>%
  filter(iso3 %in% c("SEN", "GMB")) %>%
  ggplot() +
    geom_sf(aes(geometry = geometry, fill=1-median)) +
    scale_fill_gradient2(midpoint=0, low =  wesanderson::wes_palette("Zissou1")[1],
                         high = wesanderson::wes_palette("Zissou1")[5],
                         breaks = seq(-0.1, 0.3, 0.1), 
                         labels = scales::label_percent(prefix = "+")) +
    labs(title = "Senegal and Gambia", fill="25-29 ASFR relative to 20-24") +
    theme_minimal()+
    coord_sf(datum=NA)

ggpubr::ggarrange(age_dist[["TZA"]], age_dist[["NGA"]], gmb_sen, nrow=1, common.legend = TRUE, legend="bottom")

plot_order <- c("SEN", "GMB", "GNB", "GIN", "SLE", "LBR", "MLI", "BFA", "CIV", "GHA", "TGO", "BEN", "NER", "NGA", "CMR", "TCD", "CAF", "SSD", "ETH", "GAB", "COG", "COD", "UGA", "KEN", "RWA", "BDI", "TZA", "AGO", "ZMB", "MWI", "MOZ", "ZWE", "NAM", "SWZ", "LSO", "ZAF")

pl <- fr %>%
  left_join(mapping) %>% 
  filter(area_level == fertility_fit_level,
         variable == "asfr",
         period == 2015) %>%
  group_by(area_id) %>%
  mutate(median = median/median[age_group == "Y020_024"]) %>%
  filter(age_group == "Y025_029") %>%
  filter(median < 1.8) %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, levels = plot_order)), y=median-1)) +
    geom_jitter(aes(color=region), shape=20, width =0.1, alpha = 0.5) +
    geom_point(data=nat_15 %>% filter(variable == "asfr") %>% group_by(iso3) %>%
                 mutate(median = median/median[age_group == "Y020_024"]) %>%
                 filter(age_group == "Y025_029"), shape = 21, size = 3, fill = "white", col = "black", alpha = 0.9) +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    scale_y_continuous(labels = scales::label_percent())+
    scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name")) +
    geom_hline(yintercept = 0, color="red", linetype = 3) +
    labs(x=element_blank(), y=element_blank()) +
    standard_theme() +
    theme(legend.position = "right",
          plot.margin = unit(c(1,0,5,0), "lines")) +
    coord_flip(clip = "off", xlim = c(0, 36), ylim = c(-0.3, .8))


pl + 
  annotate("segment", x=-3, xend=-3, y=-0.01, yend=-.35, arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("label", label = "Older age\ndistribution", x=-5, y=0.375, label.size=NA) +
  annotate("segment", x=-3, xend=-3, y=0.01, yend=.75, arrow = arrow(length = unit(0.3, "cm"))) +
  annotate("label", label = "Younger age\ndistribution", x=-5, y=-0.17255, label.size=NA)

fr %>%
  left_join(mapping) %>%
  filter(area_level == fertility_fit_level, period == 2016, variable == "asfr") %>%
  left_join(naomi::get_age_groups()) %>%
  mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
  group_by(iso3, area_id) %>%
  summarise(af = sum(median * age_mid_point),
            f = sum(median),
            macb = af/f) %>%
  left_join(region) %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, levels = plot_order)), y=macb)) +
    geom_jitter(aes(color=region), shape=20, width =0.1, alpha = 0.5) +
    # geom_point(data=nat_15 %>% filter(variable == "asfr") %>% group_by(iso3) %>%
    #              mutate(median = median/median[age_group == "Y020_024"]) %>%
    #              filter(age_group == "Y025_029"), shape = 21, size = 3, fill = "white", col = "black", alpha = 0.9) +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    # scale_y_continuous(labels = scales::label_percent())+
    scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name")) +
    labs(y="Mean age of childbearing", x=element_blank()) +
    standard_theme() +
    theme(legend.position = "right") +
          # plot.margin = unit(c(1,0,5,0), "lines")) +
    coord_flip(clip = "off", xlim = c(0, 36))

fr %>%
  left_join(mapping) %>%
  filter(area_level == 0, period == 2016, variable == "asfr") %>%
  left_join(naomi::get_age_groups()) %>%
  mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
  group_by(iso3, area_id) %>%
  summarise(af = sum(median * age_mid_point),
            f = sum(median),
            macb = af/f) %>%
  left_join(region) %>%
  left_join(fr %>% filter(area_level == 0, period == 2016, variable == "tfr")) %>%
  ggplot(aes(x=macb, y=median)) +
    geom_point() +
    geom_smooth(method = "lm", se=FALSE) +
    standard_theme() +
    labs(y="TFR", x="Mean age of child bearing")
  
fr %>%
  left_join(mapping) %>%
  filter(area_level == 0, variable == "asfr") %>%
  left_join(naomi::get_age_groups()) %>%
  mutate(age_mid_point = (age_group_start + age_group_start + age_group_span)/2) %>%
  group_by(iso3, period,region, area_id) %>%
  summarise(af = sum(median * age_mid_point),
            f = sum(median),
            macb = af/f) %>%
  ggplot(aes(x=period, y=macb, group =period)) +
    geom_boxplot() +
    standard_theme() +
    labs(x=element_blank(), y="MACB") +
    facet_wrap(~region)

pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")


dist_fr %>%
  bind_rows() %>%
  filter(period == 2015) %>%
  ggplot() +
  geom_sf(aes(geometry = geometry, fill=median), size=0) +
  geom_sf(data = foo, aes(geometry = geometry), fill="grey", size=0.3) +
  geom_sf(data = nat_geom, aes(geometry = geometry), fill=NA, size=0.3) +
  labs(fill = "TFR")+
  scale_fill_gradientn(colours = pal, breaks = seq(2,10,2), labels = as.character(seq(2,10,2))) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.title.align = 0.5) +
  coord_sf(datum = NA)

dist_15 <- dist_fr %>%
  bind_rows() %>%
  filter(period == 2015)

nat_15 <- fr %>%
  filter(area_level ==0, period == 2015) %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) 

c("SEN",
  )

dist_15 %>%
  mutate(country = countrycode::countrycode(iso3, "iso3c", "country.name")) %>%
  left_join(region) %>%
  left_join(pop %>% ungroup() %>% select(area_id, population)) %>%
  group_by(iso3) %>%
  mutate(population = population/median(population)) %>%
  ggplot(aes(x=fct_rev(fct_relevel(iso3, levels = plot_order)), y=median)) +
    geom_jitter(aes(color=region, size = population), shape=20, width =0.1, alpha = 0.5) +
    geom_point(data=nat_15 %>% filter(variable == "tfr"), shape = 21, size = 3, fill = "white", col = "black", alpha = 0.9) +
  standard_theme() +
  scale_x_discrete(labels = ~countrycode::countrycode(.x, "iso3c", "country.name")) +
    scale_size_continuous(breaks = c(0.5, 1, 10, 20, 100), labels=paste0(c(0.5, 1, 10, 20, 100), "x")) +
    labs(y="TFR", x=element_blank(), size = "District population relative\nto median district size", color = "Region") +
    scale_color_manual(values = wesanderson::wes_palette("Zissou1")[c(1,4)]) +
    theme(legend.title.align = 0.5) +
  # scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  coord_flip()

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

###
# Rwanda, Tanzania, Mozambique, Zambia, Angola, Zimbabwe, and Lesotho

ntl_asfr <- fr %>%
  filter(area_level == 0, 
         variable == "asfr",
         iso3 %in% c("RWA", "TZA", "MOZ", "ZMB", "ZWE", "LSO", "MWI")
         )

ntl_asfr %>%
  left_join(naomi::get_age_groups()) %>%
  ggplot(aes(x=period, y=median)) +
    geom_line(aes(color=age_group_label)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill=age_group_label), alpha = 0.3) +
    facet_wrap(~area_name) +
    standard_theme() +
    labs(x=element_blank(), y="ASFR", color=element_blank(), fill=element_blank())

dist_fr %>%
  bind_rows() %>%
  # filter(variable == "tfr") %>%
  group_by(iso3) %>%
  filter(period == 2015) %>%
  summarise(median = median(median)) %>%
  left_join(nat_15 %>% filter(variable == "tfr") %>% rename(nat_median = median)) %>%
  mutate(nat_lower = as.integer(nat_median < median)) %>%
  count(nat_lower)

#TZA SEN RWA MWI ETH
IIaa <- fr %>%
  filter(iso3 == "TZA",
         variable == "tfr",
         area_level == 4,
         # str_detect(area_id, "TZA_4_12|4_14"),
         !area_id %in% c("TZA_4_072dv", "TZA_4_048rr", "TZA_4_122um", "TZA_4_146me")
         ) %>%
  ggplot(aes(x=period, y=median, group = area_id)) +
  geom_line(alpha = 0.15) +
  geom_line(data = fr %>%
              filter(iso3 == "TZA",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(variable == "tfr",
                     area_id %in% c("TZA_4_072dv", "TZA_4_048rr", "TZA_4_122um", "TZA_4_146me")),
            aes(color=area_id),
            size=1.3) +
  scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1,4,5)], wesanderson::wes_palette("Rushmore1")[3])) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  standard_theme() +    
  expand_limits(y=c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(y="TFR", x=element_blank(), title="Tanzania")

IIab <- fr %>%
  filter(iso3 == "SEN",
         variable == "tfr",
         area_level == 2,
         # str_detect(area_id, "TZA_4_12|4_14"),
         !area_name %in% c("Diamniadio", "Koungueul", "Thilogne")
  ) %>%
  ggplot(aes(x=period, y=median, group = area_id)) +
  geom_line(alpha = 0.2) +
  geom_line(data = fr %>%
              filter(iso3 == "SEN",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(variable == "tfr",
                     area_name %in% c("Diamniadio", "Koungueul", "Thilogne")),
            aes(color=area_id),
            size=1.3) +
  scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1,4,5)])) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  standard_theme() +  
  expand_limits(y=c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(y=element_blank(), x=element_blank(), title = "Senegal")

IIac <- fr %>%
  filter(iso3 == "GHA",
         variable == "tfr",
         area_level == 2,
         # str_detect(area_id, "TZA_4_12|4_14"),
         !area_name %in% c("Ellembelle", "Shama", "Tarkwa-Nsuaem")
  ) %>%
  ggplot(aes(x=period, y=median, group = area_id)) +
  geom_line(alpha = 0.1) +
  geom_line(data = fr %>%
              filter(iso3 == "GHA",
                     variable == "tfr",
                     area_level == 0),
            size=2) +
  geom_line(data = fr %>%
              filter(variable == "tfr",
                     area_name %in% c("Ellembelle", "Shama", "Tarkwa-Nsuaem")),
            aes(color=area_id),
            size=1.3) +
  scale_color_manual(values = c(wesanderson::wes_palette("Zissou1")[c(1,4,5)])) +
  scale_y_continuous(labels = scales::label_number(accuracy = 1)) +
  standard_theme() +
  expand_limits(y=c(0, 10)) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(y=element_blank(), x=element_blank(), title="Ghana")

ggpubr::ggarrange(IIaa, IIab, IIac, nrow=1)

fr %>%
  filter(area_level == 1, variable == "tfr", iso3 == "ETH", period >1999) %>%
  ggplot(aes(x=period, y=median)) +
    geom_line(aes(color=area_name), size=1.1) +
    ggrepel::geom_label_repel(data = fr %>%
                                filter(area_level == 1, variable == "tfr", iso3 == "ETH", period == 2020),
                              aes(x=2020, label=area_name, color=area_name),
                              direction = "y",
                              xlim = c(-Inf, Inf), ylim = c(-Inf, Inf),
                              segment.linetype=2,
                              nudge_x = 5) +
    geom_line(data = fr %>%
                filter(area_id == "ETH", variable == "tfr", period >1999),
              size=2
    ) +
  standard_theme() +
  expand_limits(y=0) +
  coord_cartesian(clip="off") +
  theme(legend.position = "none",
    panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
    plot.margin = unit(c(0,10,0,0), "lines")) +
  labs(y="TFR", x=element_blank(), title="Ethiopia")
 
    

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
    scale_y_continuous(labels = scales::label_percent(), name = "% ASFR change 2000-2020", limits=c(-.5, .5)) +
    geom_hline(aes(yintercept = 0), linetype=3) +
    facet_wrap(~region) +
    theme_minimal() +
    labs(x=element_blank()) +
    theme(strip.text = element_text(size=12),
          axis.text = element_text(size=12),
          axis.title = element_text(size=14),
          panel.background = element_rect(fill=NA, color="black"))


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
p3b <- fr_list[["LBR"]] %>%
  filter(variable == "asfr",
         area_level == 0,
         period > 1999) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label)) %>%
  ggplot(aes(x=period, y=median)) +
    geom_line(show.legend = FALSE, size=1.1, aes(color=age_group_label)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = age_group_label), alpha = 0.5, show.legend = FALSE) +
    geom_label(data = . %>% filter(period == max(period)), aes(color =age_group_label, label = age_group_label), nudge_x = 3, size=5, show.legend = FALSE) +
    moz.utils::standard_theme() +
    labs(y = "ASFR", x=element_blank(), title="Liberia") +
  coord_cartesian(clip="off", xlim=c(2000, 2020)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        plot.margin = unit(c(1,5,1,0), "lines"))

p3c <- fr_list[["KEN"]] %>%
  filter(area_level ==1, age_group == "Y020_024", period >1999) %>%
  group_by(area_id) %>%
  mutate(median = median/median[period==2000]) %>%
  ggplot(aes(x=period, y=median, color=area_name)) +
    geom_line(size=1, show.legend = FALSE) +
    ggrepel::geom_label_repel(data = . %>% filter(period == max(period)), aes(color =area_name, label = area_name), 
                              xlim = c(-Inf, Inf), nudge_x = 5, size=4, segment.linetype = 2, show.legend = FALSE, direction="y") +
    standard_theme() +
    coord_cartesian(clip="off", xlim=c(2000, 2020)) +
    labs(x=element_blank(), y="ASFR normalised to 2000", title="Kenya | Provincial 20-24 ASFR") +
    theme(plot.margin = unit(c(1,7,1,0), "lines"))

gridExtra::grid.arrange(p3a, p3b, p3c, nrow=2, widths=c(2,1))

p3i <- gridExtra::grid.arrange(p3b, p3c, nrow=1)

gridExtra::grid.arrange(p3a, p3i)

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

