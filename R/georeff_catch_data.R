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
  
  georeff.codes <- georeffcatchlocs %>% 
    dplyr::distinct(catch_code, deci_lat, deci_lon) 

  catch.species <- unique(catch.code$species)
  
  fresh.sp <- c("TRUCHA","CARPA")
  
    
  catch.code <- catchdata %>% 
    mutate(catch_code = paste(rnp_code, NOM_ENT, NOM_LOC, fishery_office, sep="_")) %>% 
    dplyr::left_join(georeff.codes, by = c("catch_code")) %>% 
    keep_when(!is.na(deci_lat))
  
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