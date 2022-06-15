areas <- read_area_merged("depends/naomi_areas.geojson") %>%
  st_make_valid()

pop <- naomi_extract_worldpop(areas, iso3, years = c(2000, 2005, 2010, 2015, 2020))
write_csv(pop, "population_worldpop_naomi.csv", na = "")
