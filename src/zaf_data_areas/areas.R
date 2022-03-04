
#' Heath sub-district boundaries
#' * Received from Right to Care via HE2RO. Sent with note on untidyness of Gauteng
#'   and some uncertainty about exact boundaries.

tmpdir <- tempfile()
unzip("raw/r2c/ZA_HSD.zip", exdir = tmpdir)

sh_r2c <- read_sf(file.path(tmpdir, "ZA_HSD.shp"))
  
#' District and Local Municipality boundaries: Municipal Demarcation Board

tmpd <- tempfile()
unzip("raw/mdb/2016-Boundaries-District.zip", exdir = tmpd)
sh2 <- read_sf(list.files(tmpd, "shp$", full.names = TRUE))

tmpl <- tempfile()
unzip("raw/mdb/2016-Boundaries-Local.zip", exdir = tmpl)
sh3 <- read_sf(list.files(tmpl, "shp$", full.names = TRUE))



#' DHIS2: location hierarchy with short names

dhis2 <- read_csv("raw/dhis2/vwOrgunitStructureOU4.csv")

#' Simplify shapefile
sh_simple <- ms_simplify(sh_r2c, 0.1)

## check_boundaries(sh_simple)
## check_boundaries(sh_r2c, sh_simple)
## check_boundaries(sh_r2c %>% filter(PROVINCE == "GP"), sh_simple %>% filter(PROVINCE == "GP"))

## check_boundaries(sh_r2c %>% filter(PROVINCE == "WC"),
##                  sh_simple %>% ms_simplify(0.05) %>% filter(PROVINCE == "WC"))

## check_boundaries(sh_simple %>% filter(DISTRICT == "CPT"),
##                  sh_simple %>% ms_simplify(0.1) %>% filter(DISTRICT == "CPT"))



#' ## Recode area names

sh <- sh_simple %>%
  mutate(PROVINCE = recode(PROVINCE, "GP" = "GT"),
         OU4name = sub("Sub-District", "sub-District", ORGUNIT) %>%
           recode(
             "kz Abaqulusi Local Municipality" = "kz AbaQulusi Local Municipality",
             "kz Umhlabuyalingana Local Municipality" = "kz uMhlabuyalingana Local Municipality",
             "kz uMuziwabantu Local Municipality" = "kz Umuziwabantu Local Municipality",
             "kz Umzimkhulu Local Municipality" = "kz uMzimkhulu Local Municipality",
             "lp Fetakgomo-Greater Tubatse Local Municipality" = "lp Fetakgomo Tubatse Local Municipality",
             "nc Kh_i-Ma Local Municipality" = "nc Khâi-Ma Local Municipality"
           )
         ) %>%
  full_join(dhis2, by = "OU4name") 

#' Check no unmatched sub-districts
sh %>%
  filter(is.na(OU4name) | is.na(ORGUNIT))

sh <- sh %>%
  mutate(MAP_TITLE = sub("^...", "", OU4name)) %>%
  left_join(
    sh3 %>%
    as.data.frame %>%
    select(PROVINCE, MAP_TITLE, CAT_B)
  ) %>%
  mutate(hsd_code =
           case_when(!is.na(CAT_B) ~ as.character(CAT_B),
                     DISTRICT == "BUF" ~ DISTRICT,
                     DISTRICT == "CPT" ~ paste0(DISTRICT,
                                                recode(OU4short,
                                                       "CT Eastern SD" = "E",
                                                       "CT Northern SD" = "N",
                                                       "CT Southern SD" = "S",
                                                       "CT Western SD" = "W",
                                                       "CT Khayelitsha SD" = "KH",
                                                       "CT Klipfontein SD" = "KL",
                                                       "CT Mitch Plain SD" = "MP",
                                                       "CT Tygerberg SD" = "TY")),
                     DISTRICT == "EKU" ~ paste0(DISTRICT, sub("Ekurhuleni (..) SD$", "\\1", OU4short)),
                     DISTRICT == "JHB" ~ paste0(DISTRICT, sub("Johannesburg (.) SD$", "\\1", OU4short)),
                     DISTRICT == "NMA" ~ paste0(DISTRICT, sub("N Mandela (.) SD$", "\\1", OU4short)),
                     DISTRICT == "TSH" ~ paste0(DISTRICT, sub("Tshwane (.) SD$", "\\1", OU4short)),
                     DISTRICT == "MAN" ~ paste0(DISTRICT,
                                                recode(OU4short,
                                                       "Bloemfontein SD" = "1",
                                                       "Botshabelo SD" = "2",
                                                       "Naledi SD" = "3",
                                                       "Thaba N'chu SD" = "4")),
                     is.na(CAT_B) ~ recode(OU4name,
                                           "ec Dr Beyers Naudé Local Municipality" = "EC101",
                                           "ec Ingquza Hill Local Municipality" = "EC153",
                                           "fs Maluti-a-Phofung Local Municipality" = "FS194",
                                           "kz Umuziwabantu Local Municipality" = "KZN214",
                                           "kz Msunduzi Local Municipality" = "KZN225",
                                           "kz Nquthu Local Municipality" = "KZN242",
                                           "kz AbaQulusi Local Municipality" = "KZN263",
                                           "kz uPhongolo Local Municipality" = "KZN262",
                                           "kz Big 5 Hlabisa Local Municipality" = "KZN276",
                                           "kz uMhlabuyalingana Local Municipality" = "KZN271",
                                           "kz City of uMhlathuze Local Municipality" = "KZN282",
                                           "kz uMfolozi Local Municipality" = "KZN281",
                                           "mp Thembisile Hani Local Municipality" = "MP315",
                                           "mp City of Mbombela Local Municipality" = "MP326",
                                           "lp Collins Chabane Local Municipality" = "LIM345",
                                           "lp Lepelle-Nkumpi Local Municipality" = "LIM355",
                                           "lp Mookgophong/Modimolle Local Municipality" = "LIM368",
                                           "nw Madibeng Local Municipality" = "NW372",
                                           "nw Mahikeng Local Municipality" = "NW383",
                                           "nw Kagisano-Molopo Local Municipality" = "NW397",
                                           "nw JB Marks Local Municipality" = "NW405",
                                           "kz uMzimkhulu Local Municipality" = "KZN435",
                                           "lp Fetakgomo Tubatse Local Municipality" = "LIM476",
                                           "nc Sol Plaatje Local Municipality" = "NC091",
                                           "kz eThekwini Metropolitan Municipality Sub" = "ETH",
                                           .default = NA_character_))
         )


#' Check all locations have HSD code
sh %>% filter(is.na(hsd_code)) %>% nrow

#' Check HSD codes are unique
sh %>%
  as.data.frame %>%
  count(DISTRICT, hsd_code) %>%
  filter(n > 1)

#' Add Local Municipality level

sh <- sh %>%
  mutate(lm_code = if_else(grepl("^DC[1-9]", DISTRICT), hsd_code, DISTRICT),
         lm_name = if_else(grepl("^DC[1-9]", DISTRICT), OU4short, OU3short))

#' Add province code as spectrum_region_code

#' # Spectrum region codes

spectrum_region_code <- c("Eastern Cape" = 2,
                          "Free State" = 4,
                          "Gauteng" = 7,
                          "KwaZulu-Natal" = 5,
                          "Limpopo" = 9,
                          "Mpumalanga" = 8,
                          "Northern Cape" = 3,
                          "North West" = 6,
                          "Western Cape" = 1)

sh <- mutate(sh, spectrum_region_code = recode(OU2short, !!!spectrum_region_code))

zaf_wide <- sh %>%
  mutate(id0 = "ZAF",
         name0 = OU1short,
         id1 = paste0("ZAF_1_", PROVINCE),
         name1 = OU2short,
         id2 = paste0("ZAF_2_", DISTRICT),
         name2 = OU3short,
         id3 = paste0("ZAF_3_", lm_code),
         name3 = lm_name,
         id4 = paste0("ZAF_4_", hsd_code),
         name4 = OU4short) %>%
  arrange(factor(id1, paste0("ZAF_1_", c("WC", "NC", "EC", "FS", "KZN", "MP", "LIM", "NW", "GT"))),
          grepl("^DC", sub("ZAF_2_", "", id2)),
          as.integer(sub("ZAF_2_DC", "", id2)),
          id3,
          nchar(id4),
          id4) %>%
  mutate(id0 = as_factor(id0),
         id1 = as_factor(id1),
         id2 = as_factor(id2),
         id3 = as_factor(id3),
         id4 = as_factor(id4))
         

zaf <- gather_areas(zaf_wide)

zaf_area_hierarchy <- zaf %>%
  st_drop_geometry() %>%
  select(area_id, area_level, parent_area_id, area_name, spectrum_region_code) %>%
  mutate(area_sort_order = row_number())

zaf_area_levels <- zaf_area_hierarchy %>%
  count(area_level, name = "n_areas") %>%
  arrange(area_level) %>%
  mutate(area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Province",
                  `2` = "District",
                  `3` = "Local Municipality",
                  `4` = "Heath Sub-District"),
         display = TRUE,
         spectrum_level = if_else(area_level == 0, TRUE, FALSE),
         epp_level = if_else(area_level == 1, TRUE, FALSE),
         epp_urban_rural = FALSE,
         naomi_level = if_else(area_level == 4, TRUE, FALSE),
         pepfar_psnu_level = if_else(area_level == 2, TRUE, FALSE))

zaf_area_boundaries <- zaf %>%
  select(area_id, geometry)

zaf_area_map_dhis2 <- zaf_wide %>%
  st_drop_geometry() %>%
  {bind_rows(
     transmute(
       .,
       area_id = id0,
       OUuid   = OU1uid,
       OUname  = OU1name,
       OUcode  = OU1code,
       OUshort = OU1short
     ),
     transmute(
       .,
       area_id = id1,
       OUuid   = OU2uid,
       OUname  = OU2name,
       OUcode  = OU2code,
       OUshort = OU2short
     ),
     transmute(
       .,
       area_id = id2,
       OUuid   = OU3uid,
       OUname  = OU3name,
       OUcode  = OU3code,
       OUshort = OU3short
     ),
     transmute(
       .,
       area_id = id4,
       OUuid   = OU4uid,
       OUname  = OU4name,
       OUcode  = OU4code,
       OUshort = OU4short
     )
   )} %>%
  distinct() %>%
  mutate(iso3 = "ZAF") %>%
  select(iso3, everything())

zaf_areas <- zaf_area_levels %>%
  left_join(zaf_area_hierarchy) %>%
  left_join(zaf_area_boundaries) %>%
  st_as_sf() %>%
  mutate(center = st_point_on_surface(geometry),
         center_x = st_coordinates(center)[,1],
         center_y = st_coordinates(center)[,2],
         center = NULL) %>%
  select(-spectrum_level, -epp_level, -epp_urban_rural, -naomi_level, -pepfar_psnu_level) %>%
  select(-geometry, everything(), geometry) 

st_write(zaf_areas, "zaf_areas.geojson", delete_dsn = TRUE)
st_write(zaf_area_boundaries, "zaf_area_boundaries.geojson", delete_dsn = TRUE)
write_csv(zaf_area_hierarchy, "zaf_area_hierarchy.csv", na = "")
write_csv(zaf_area_levels, "zaf_area_levels.csv", na = "")
write_csv(zaf_area_map_dhis2, "zaf_area_map_dhis2.csv", na = "")


p_hierarchy <- zaf_areas %>%
  group_by(area_level) %>%
  mutate(label = sprintf("%s (%d)", area_level_label, n())) %>%
  ungroup() %>%
  mutate(label = fct_reorder(label, area_level)) %>%
  ggplot() +
  geom_sf() +
  facet_wrap(~label, nrow = 1) +
  th_map()

ggsave("zaf_area_hierarchy.png", p_hierarchy, h = 6, w = 12)

zaf_area_boundaries_coarse <- ms_simplify(zaf_area_boundaries, 0.1)

st_write(zaf_area_boundaries_coarse, "zaf_area_boundaries_coarse.geojson", delete_dsn = TRUE)

