areas <- read_sf("ssd_areas_in.geojson")

sf::st_write(areas, "ssd_areas.geojson", delete_dsn = TRUE)

# int <- read_sf("src/ssd_data_areas/ssd_areas_in.geojson") %>%
#   mutate(center_x = 1,
#          center_y = 1,
#          naomi_level = ifelse(area_level, TRUE, FALSE))
# 
# st_write(int, "src/ssd_data_areas/ssd_areas_in.geojson", delete_dsn = TRUE)
