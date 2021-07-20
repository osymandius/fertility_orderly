aggregate_mics_admin1 <- function(dat, areas, areas_wide, admin1_lvl) {
  
  dat <- dat %>%
    left_join(areas %>% select(area_id, area_level) %>% st_drop_geometry()) %>%
    group_split(area_level)
  
  dat <- lapply(dat, function(x) {
    
    mics_area_level <- unique(x$area_level)
    
    if(mics_area_level > 1) {
      area_id_level <- paste0("area_id", mics_area_level)
      
      x <- x %>%
        left_join(areas_wide %>% select(all_of(area_id_level), area_id1) %>% st_drop_geometry %>% distinct, by=c("area_id" = area_id_level)) %>%
        select(-area_id) %>%
        rename(area_id = area_id1) %>%
        type.convert()
    } else {
      type.convert(x)
    }
    
  })

}