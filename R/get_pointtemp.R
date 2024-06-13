#' Get temperature for catch points
#'
#' @param eachcatchpoint 
#' @param this.depth 
#' @param catch.pacific.spatial 
#' @param gc.ocean.grid 
#' @param glorys.files 
#'
#' @return
#' @export
#'
#' @examples
get_pointtemp <- function(eachcatchpoint, this.depth, catch.pacific.spatial, gc.ocean.grid, glorys.files){
  
  this.catch.spatial <- catch.pacific.spatial[eachcatchpoint,]
  print(eachcatchpoint)
  print(this.catch.spatial)
  
  this.distance <- sf::st_distance(this.catch.spatial, gc.ocean.grid)
  
  min.distance <- min(this.distance)
  
  this.catch.geo <- this.catch.spatial %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    dplyr::as_tibble() %>% 
    select(-geom) %>% 
    mutate(month_no = dplyr::case_when(
      month  == "DICIEMBRE" ~ 12,
      month == "NOVIEMBRE" ~ 11,
      month == "OCTUBRE" ~ 10,
      month == "SEPTIEMBRE" ~ 9,
      month == "AGOSTO" ~ 8,
      month == "JULIO" ~ 7,
      month == "JUNIO" ~ 6,
      month == "MAYO" ~ 5,
      month == "ABRIL" ~ 4,
      month == "MARZO" ~ 3,
      month == "FEBRERO" ~ 2,
      month == "ENERO" ~ 1))
  
  indices <- which(this.distance == units::set_units(min.distance, "m"), arr.ind = TRUE)
  
  df.dist <- as.data.frame(indices)
  df.dist$dist <- as.numeric(this.distance[indices])
  
  catch.indices <- df.dist$col
  
  this.ocean.grid <- gc.ocean.grid %>% 
    dplyr::filter(id_pol %in% catch.indices)
  
  plot(this.ocean.grid)
  
  glorys.files.no <- 1:length(glorys.files)
  
  list.tempvalues <- list()
  
  list.tempvalues <- lapply(glorys.files.no, get_glorysdata, glorys.files, this.ocean.grid, this.catch.geo, this.depth, list.tempvalues)
  
  this.point.tempdata <- dplyr::bind_cols(list.tempvalues)
  
  
  if(eachcatchpoint==1){
    
    readr::write_csv(this.point.tempdata, here::here("outputs","catch_tempdata.csv"), append = FALSE)
    
  } else {
    
    readr::write_csv(this.point.tempdata, here::here("outputs","catch_tempdata.csv"), append = TRUE)
    
  }
  
  return(this.point.tempdata)
  
}
