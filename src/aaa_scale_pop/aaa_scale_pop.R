iso3_c <- iso3

files <- list.files("depends")

## Boundary file
area <- files[grepl(paste0(tolower(iso3), "_areas"), files)]
area_file <- paste0("depends/", area)

if (!basename(area_file) %in% files) {
  stop("Areas file not found: ", area_file)
}

areas <- read_area_merged(area_file) %>%
  mutate(iso3 = iso3)

worldpop <- read_csv("depends/population_worldpop_naomi.csv") %>%
  separate(calendar_quarter, into=c(NA, "year", NA), sep = c(2, 6), convert=TRUE)

# orderly_id <- orderly::orderly_search(query = paste0("latest(parameter:iso3 == '", iso3_c, "')"), "aaa_data_population_worldpop", draft = FALSE)
# 
# worldpop <- read_csv(paste0("archive/aaa_data_population_worldpop/", orderly_id, "/population_worldpop_naomi.csv")) %>%
#   filter(str_detect(area_id, "_5_")) %>%
#   separate(calendar_quarter, into=c(NA, "year", NA), sep = c(2, 6), convert=TRUE)

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

f_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/population/WPP2019", "WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx")
m_path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/population/WPP2019", "WPP2019_POP_F15_2_ANNUAL_POPULATION_BY_AGE_MALE.xlsx")

wpp_pop_f <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = f_path)
wpp_pop_m <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = m_path)

wpp_pop_f <- readxl::read_excel(wpp_pop_f, "ESTIMATES", skip = 16, na = "...")
wpp_pop_m <- readxl::read_excel(wpp_pop_m, "ESTIMATES", skip = 16, na = "...")

wpp_pop <- wpp_pop_f %>%
  mutate(sex = "female") %>%
  bind_rows(
    wpp_pop_m %>% mutate(sex = "male")
  ) %>%
  rename(index = 1, variant = 2, country = 3, notes = 4, countrycode = 5, area_type = 6, parent_code = 7, year = 8) %>%
  left_join(
    countrycode::codelist %>% select(countrycode = iso3n, iso3 = iso3c),
    by = "countrycode"
  ) %>%
  filter(iso3 == iso3_c,
         year %in% 1995:2020,
         !(year == 2020 & variant == "Medium variant")) %>%
  select(-c(index:parent_code)) %>%
  type.convert() %>%
  pivot_longer(-c(iso3, sex, year), names_to = "age_group_label", values_to = "pop") %>%
  mutate(age_group_label = age_group_label %>% 
           fct_collapse("80+" = c("80+", "80-84", "85-89", "90-94", "95-99", "100+")) %>%
           fct_relevel(c(paste0(0:15*5, "-", 0:15*5+4), "80+"))) %>%
  left_join(get_age_groups() %>% select(age_group, age_group_label)) %>%
  count(iso3, year, sex, age_group, source = "wpp19", wt = 1e3*pop, name = "population") %>%
  mutate(population = if_else(is.nan(population), 0, population))

#' Target size for age/sex
agesex_pop <- wpp_pop %>%
  filter(year %in% unique(worldpop$year)) %>%
  select(-source) %>%
  rename(wpp19pop = population)

#' Target population by area
area_pop <- worldpop %>%
  left_join(areas %>% select(area_id, area_level) %>% st_drop_geometry()) %>%
  group_by(source, year, area_level, area_id) %>%
  summarise(target_area_pop = sum(population)) %>%
  mutate(target_area_pop = target_area_pop / sum(target_area_pop)) %>%
  left_join(
    wpp_pop %>%
      count(year, wt = population)
  ) %>%
  mutate(target_area_pop = target_area_pop * n,
         n = NULL)

pop_wpp19 <- worldpop %>%
  left_join(agesex_pop) %>%
  left_join(area_pop) %>%
  # left_join(areas %>% select(area_id, area_level)) %>%
  mutate(source = paste0(source, "_wpp19"),
         population_base = population)

#' Do four iterations of proportional fitting

for(i in 1:4) {
  print(paste("iteration", i))
  
  pop_wpp19 <- pop_wpp19 %>%
    group_by(iso3, source, year, area_level, sex, age_group) %>%
    mutate(ratio_wpp = wpp19pop / sum(population),
           population = population * ratio_wpp) %>%
    group_by(iso3, source, area_id, year) %>%
    mutate(ratio_area = target_area_pop / sum(population),
           population = population* ratio_area) %>%
    ungroup
  
  print(
    pop_wpp19 %>% 
      summarise(min_age = min(ratio_wpp, na.rm=TRUE),
                max_age = max(ratio_wpp, na.rm=TRUE),
                min_area = min(ratio_area, na.rm=TRUE),
                max_area = max(ratio_area, na.rm=TRUE))
  )
}

female_pop <- pop_wpp19 %>%
  left_join(get_age_groups() %>% select(age_group, age_group_sort_order)) %>%
  filter(sex == "female", age_group_sort_order %in% 16:22) %>%
  select(area_id, year, age_group, population)

fertility_population <- crossing(area_id = areas$area_id,
         year = 1995:2020,
         age_group = unique(female_pop$age_group)) %>%
  left_join(female_pop) %>%
  group_by(area_id, age_group) %>%
  mutate(population = log(population),
         population = zoo::na.approx(population, na.rm=FALSE),
         population = exp(population)) %>%
  fill(population, .direction = "up")
  

write_csv(fertility_population, "fertility_population.csv")
