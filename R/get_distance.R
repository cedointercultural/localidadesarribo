#' Get distance to nearest fishery office
#'
#' @param thisloc 
#' @param fishery.offices 
#'
#' @return
#' @export
#'
#' @examples
get_distance <- function(thisloc, fishery.offices){
  
  fishery.office.tbl <- fishery.offices %>% 
    sf::st_drop_geometry() 
  
  print(thisloc)
  
  this.point <- georef.locs.sp[thisloc,]
  
  this.point.tbl <- this.point %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    dplyr::as_tibble()
  
  this.distance <- this.point %>% 
    sf::st_distance(fishery.offices) %>% 
    dplyr::as_tibble() %>% 
    tidyr::pivot_longer(cols = everything(), names_to = "fishery_office", values_to = "distance") %>% 
    dplyr::bind_cols(fishery.office.tbl) %>% 
    keep_when(distance == min(distance))
  
  this.point.dis <- this.point.tbl  %>% 
    mutate(fishery_office = this.distance$NOM_LOC, fishery_office_ent = this.distance$NOM_ENT, distance = this.distance$distance) 
  
  return(this.point.dis)
}