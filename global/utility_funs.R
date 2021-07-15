aggregate_mics_admin1 <- function(dat, areas, areas_wide, admin1_lvl) {
  
  mics_area_level <- dat %>%
    left_join(areas %>% select(area_id, area_level) %>% st_drop_geometry()) %>%
    .$area_level %>%
    unique
  
  if(mics_area_level > admin1_lvl) {
    area_id_level <- paste0("area_id", mics_area_level)
    
    dat <- dat %>%
      left_join(areas_wide %>% select(all_of(area_id_level), area_id1) %>% st_drop_geometry %>% distinct, by=c("area_id" = area_id_level)) %>%
      select(-area_id) %>%
      rename(area_id = area_id1) %>%
      type.convert()
  } else
    dat <- type.convert(dat)
}