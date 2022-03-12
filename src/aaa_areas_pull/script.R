
# This script will pull the following artefacts from the orderly archive
# for the downstream use in a parameterised orderly task:
# * Areas: latest from orderly archive


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