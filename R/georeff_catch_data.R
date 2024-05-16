#' Georeferrence all catch data
#'
#' @param catchdata 
#' @param georeffcatchlocs
#' @param pac.buffer 
#'
#' @return georeffcatchlocs
#' @export
#'
#' @examples
georref_catch_data <- function(catchdata, georeffcatchlocs, pac.buffer){
  
  catch.code <- catchdata %>% 
    mutate(catch_code = paste(rnp_code, NOM_ENT, NOM_LOC, fishery_office, sep="_")) %>% 
    left_join(georeffcatchlocs, by = c("catch_code")) 
  
  catch.spatial <- catch.code %>% 
    sf::st_as_sf(coords = c("deci_lon", "deci_lat"), crs = 4326) 
  
  catch.spatial <- catch.spatial %>% 
    sf::st_difference(pac.buffer) 
  
  catch.geo.locs <- catch.spatial %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    dplyr::as_tibble()
   
  
  #count number of records lost
  lost.records <- catch.code %>% 
    keep_when(is.na(deci_lat))
  
  
  return(catch.geo.locs)
  
}