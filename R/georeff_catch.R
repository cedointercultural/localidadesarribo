georref_catch <- function(catchlocations, georeflocskm){
  
  catch.code <- catchlocations %>% 
    mutate(catch_code = paste(NOM_ENT, NOM_LOC, fishery_office, sep="_"))
  
  locs.catch.geo <- catch.code %>% 
    dplyr::left_join(georeflocskm, by = c("NOM_ENT", "NOM_LOC","fishery_office")) %>% 
    dplyr::distinct(catch_code, rnp_code, NOM_LOC, NOM_ENT, deci_lat, deci_lon, fishery_office, fishery_office_ent, distance_km)
  
  geo.codes <- locs.catch.geo %>% 
    keep_when(!is.na(deci_lon)) %>% 
    dplyr::distinct(catch_code) %>% 
    dplyr::pull(catch_code)

  locs.catch.nooffice <- catch.code %>% 
    kep_when(!catch_code %in% geo.codes) %>% 
    dplyr::left_join(georeflocskm, by = c("NOM_ENT", "NOM_LOC")) %>% 
  
  return(locs.catch.geo)
  
}