area_file <- paste0("depends/", tolower(iso3), "_areas.geojson")

if (!basename(area_file) %in% list.files("depends")) {
  stop("Areas file not found: ", area_file)
}


areas <- read_area_merged(area_file)
pop <- naomi_extract_worldpop(areas, iso3, years = c(2000, 2005, 2010, 2015, 2020))
write_csv(pop, "population_worldpop_naomi.csv", na = "")
