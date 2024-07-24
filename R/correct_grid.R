#' regrid files to assign lat lon dimensions
#'
#' @param this.file 
#' @param this.var 
#' @param weight.dir 
#'
#' @return
#' @export
#'
#' @examples
correct_grid <- function(this.file, this.var){
  
  print(this.file)
  #create weight file  
  #https://code.mpimet.mpg.de/boards/1/topics/8676?page=2&r=8685
  this.sethalo.file <- gsub("[:.:]nc","_sethalo.nc", this.file)
  
  #fixes issue with missing values after remapping
  system(paste0("cdo sethalo,0,1 ",this.file," ", this.sethalo.file), wait=TRUE)
  this.terra.rast<- get_nc_description(this.sethalo.file)
  terra::plot(this.terra.rast)
  
  
  weight.file <- gsub("_sethalo.nc","_weight.nc", this.sethalo.file)
  
 if(!file.exists(weight.file)){
  
   #use cdo to regrid the files, this will assign a lat and lon dimension
   # remap / converts from the native grid to a generic cartesian grid
   # https://github.com/trondkr/cmip6
   #MPI-ESM1 output has lat lon as variables, not dimensions so it cannot be used directly
   #generate a weights file
   
   ClimateOperators::cdo(paste0("genbil,r802x404 ", this.sethalo.file," ", weight.file, sep="")) 
   
 }
  
  this.coord.file <- gsub("_weight.nc","_coord.nc", weight.file)
  
  if(!file.exists(this.coord.file)){

    #Use weight file to regrid the file   
    ClimateOperators::cdo(paste0("remap,r802x404,",weight.file," ", this.sethalo.file," ",this.coord.file,sep=""))
     
    this.terra.rast<- get_nc_description(this.coord.file)
    terra::plot(this.terra.rast)
    
  } 
  }