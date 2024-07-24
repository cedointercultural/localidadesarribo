#' Get raster resolution, description and plot
#'
#' @param thisraster 
#'
#' @return this.terra.rast terra raster
#' @export
#'
#' @examples
get_nc_description <- function(thisraster){
  
  this.terra.rast <- terra::rast(thisraster)
  this.terra.res <- terra::res(this.terra.rast)
  print(this.terra.rast)
  system(paste0("cdo sinfon ",thisraster), wait = TRUE)
  
  return(this.terra.rast)                       
}