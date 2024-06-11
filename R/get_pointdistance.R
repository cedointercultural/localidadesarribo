#' Title Get distance from catch to neares grid polygons
#'
#' @param eachcatchpoint 
#' @param catch.pacific.spatial 
#' @param gc.ocean.grid 
#'
#' @return this.ocean.grid
#' @export
#'
#' @examples
get_pointdistance<- function(eachcatchpoint, catch.pacific.spatial, gc.ocean.grid){
  
  this.catch.spatial <- catch.pacific.spatial[eachcatchpoint,]
  
  this.distance <- sf::st_distance(this.catch.spatial, gc.ocean.grid)
  
  min.distance <- 2*(min(this.distance))
  
  #
  indices <- which(this.distance < units::set_units(min.distance, "m"), arr.ind = TRUE)
  
  df.dist <- as.data.frame(indices)
  df.dist$dist <- as.numeric(this.distance[indices])
  
  catch.indices <- df.dist$col
  
  this.ocean.grid <- gc.ocean.grid %>% 
    dplyr::filter(id_pol %in% catch.indices)
  
  return(this.ocean.grid)
}