#' Extract hyperslab of selected coordinates
#'
#' @param this.coord.file 
#' @param min.lon 
#' @param max.lon 
#' @param min.lat 
#' @param max.lat 
#'
#' @return
#' @export
#'
#' @examples
hyerslab_spatial_area <- function(this.coord.file, min.lon, max.lon, min.lat, max.lat){
  
  print(this.coord.file)  
  dim.file <- gsub("[:.:]nc","_dim.nc", this.coord.file)
  
  print(dim.file)
  ClimateOperators::cdo(paste0("sellonlatbox,-180,180,-90,90 ",this.coord.file, " ", dim.file)) 
  ClimateOperators::cdo(paste0("sinfo ",dim.file))
  #dim.rast <- terra::rast(dim.file)
  #terra::plot(dim.rast)
  
  slab.file <- gsub("[:.:]nc","_slab.nc", dim.file)
  print(slab.file)
  ClimateOperators::ncks(paste0("-F -d lon,",min.lon,",",max.lon, " -d lat,", min.lat,",",max.lat," ", dim.file," -O ", slab.file))
  
  ClimateOperators::cdo(paste0("sinfo ",slab.file))
  #slab.ras <- terra::rast(slab.file)
  #terra::plot(slab.ras)
}