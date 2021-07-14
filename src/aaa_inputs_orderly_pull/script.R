
# This script will pull the following artefacts from the orderly archive
# for the downstream use in a parameterised orderly task:
# * Areas: latest from orderly archive
# * Population: latest from orderly, worldpop or gpw (pull from iso_mapping_fit in global resources)
# * ASFR: latest from orderly archive


# Prepare model inputs
files <- list.files("depends")

## Boundary file
area <- files[grepl(paste0(tolower(iso3), "_areas"), files)]
area_file <- paste0("depends/", area)

if (!basename(area_file) %in% files) {
  stop("Areas file not found: ", area_file)
}
areas <- read_area_merged(area_file)

st_write(areas, "naomi_areas.geojson", delete_dsn = TRUE)


## Population files
# Map iso3 parameter to population source
map <- read_csv("resources/iso_mapping_fit.csv")
pop_source <- map$pop_data_source[map$iso3 == iso3]

if(pop_source == "orderly") {
  
  pop_file <- files[grepl(paste0(tolower(iso3), "_population"), files)]
  pop <- read_csv(paste0("depends/", pop_file))
  
} else if (pop_source == "gpw") {
  
  pop <- read_csv("depends/population_gpw_naomi.csv")
  
} else if (pop_source == "worldpop") {
  
  pop <- read_csv("depends/population_worldpop_naomi.csv")
  
}

write_csv(pop, "naomi_population.csv")

# ASFR files
asfr_file <- files[grepl(paste0(tolower(iso3), "_asfr"), files)]

if (!basename(asfr_file) %in% files) {
  stop("ASFR file not found: ", area_file)
}

asfr <- read_csv(paste0("depends/", asfr_file))

write_csv(asfr, "fertility_asfr.csv")

# FR plotting files
fr_plot_file <- files[grepl(paste0(tolower(iso3), "_fr_plot"), files)]

if (!basename(fr_plot_file) %in% files) {
  stop("FR plot file not found: ", area_file)
}

fr_plot <- read_csv(paste0("depends/", fr_plot_file))

write_csv(fr_plot, "fertility_fr_plot.csv")






