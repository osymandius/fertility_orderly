areas <- read_sf("ssd_areas_in.geojson")

sf::st_write(areas, "ssd_areas.geojson", delete_dsn = TRUE)

# int <- sf::read_sf("src/ssd_data_areas/ssd_areas_in.geojson")[1:11,] %>%
#   mutate(area_name = ifelse(area_name == "Central African Republic", "South Sudan", area_name)) %>%
#   select(-X1)
# #   mutate(center_x = 1,
# #          center_y = 1,
# #          naomi_level = ifelse(area_level, TRUE, FALSE))
# # 
# st_write(int, "src/ssd_data_areas/ssd_areas_in.geojson", delete_dsn = TRUE)
