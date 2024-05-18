#' Georeferrence list of catch locations
#'
#' @param catchlocations 
#' @param georeflocskm 
#'
#' @return georeffcatchlocs
#' @export
#'
#' @examples
georref_catch <- function(catchlocations, georeflocskm){
  
  catch.code <- catchlocations %>% 
    mutate(catch_code = paste(rnp_code, NOM_ENT, NOM_LOC, fishery_office, sep="_"))
  
  locs.catch.geo <- catch.code %>% 
    dplyr::left_join(georeflocskm, by = c("NOM_ENT", "NOM_LOC","fishery_office")) %>% 
    dplyr::distinct(catch_code, rnp_code, NOM_LOC, NOM_ENT, deci_lat, deci_lon, fishery_office, fishery_office_ent, distance_km)
  
  geo.codes <- locs.catch.geo %>% 
    keep_when(!is.na(deci_lon)) %>% 
    dplyr::distinct(catch_code) %>% 
    dplyr::pull(catch_code)

  locs.catch.nooffice <- catch.code %>% 
    keep_when(!catch_code %in% geo.codes) %>% 
    rename(fishery_office_catch = fishery_office) %>%
    dplyr::left_join(georeflocskm, by = c("NOM_ENT", "NOM_LOC")) 
  
  locs.catch.geo.off.coor <- locs.catch.geo %>% 
    keep_when(!is.na(deci_lon))  %>% 
    dplyr::group_by(catch_code, rnp_code, NOM_LOC, NOM_ENT, fishery_office, fishery_office_ent, distance_km) %>%
    summarise(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon), .groups = 'drop')
  
  locs.catch.geo.noff.coor <- locs.catch.nooffice %>% 
    keep_when(!is.na(deci_lon)) %>%
    select(-fishery_office) %>% 
    dplyr::rename(fishery_office = fishery_office_catch) %>%
    dplyr::group_by(catch_code, rnp_code, NOM_LOC, NOM_ENT, fishery_office, fishery_office_ent, distance_km) %>%
    summarise(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon), .groups = 'drop')
   
 existing.locs <- dplyr::bind_rows(locs.catch.geo.off.coor, locs.catch.geo.noff.coor) %>% 
    dplyr::distinct(catch_code) %>% 
    dplyr::pull(catch_code)
 
 missing.locs <- locs.catch.geo %>% 
    keep_when(!catch_code %in% existing.locs) %>% 
    distinct(NOM_ENT, NOM_LOC) 
    
  readr::write_csv(missing.locs,here::here("outputs","missing_locs.csv"))  
  
    #get_place <- function(thiscommunity,thisstate){georeflocskm %>% keep_when(grepl(thiscommunity, NOM_LOC)) %>% keep_when(grepl(thisstate, NOM_ENT))}
  
  georeffcatchlocs <- dplyr::bind_rows(locs.catch.geo.off.coor, locs.catch.geo.noff.coor)  %>% 
    dplyr::group_by(catch_code, rnp_code, NOM_LOC, NOM_ENT, fishery_office, fishery_office_ent) %>%
    summarise(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon), .groups = 'drop')
  
  
  return(georeffcatchlocs)
  
}