#' Interpolate delta anomaly
#'
#' @param this.delta.file 
#' @param glorys.nc 
#'
#' @return inter.raster
#' @export
#'
#' @examples
interpolate_delta <- function(this.delta.file, glorys.nc, delta.path){
  #check GLORYS file and plot
  system(paste0("cdo infon ",glorys.nc),wait = TRUE)
  system(paste0('cdo griddes ',glorys.nc,' > glorys3d.grd')) # just need to run this once
  
  #get depth levels from GLORYS
  zlevs.glorys <- tidync::tidync(glorys.nc) %>% 
    tidync::activate("D3") %>% 
    tidync::hyper_tibble() %>% 
    dplyr::pull('sfc') %>% 
    paste(collapse=",")
  
  #create name for interpolated file
  this.inter.file <- gsub("_delta","_interp", this.delta.file)
  
  #interpolate vertically and horizontally
  system(paste0("cdo -intlevel,",zlevs.glorys," -remapdis,",glorys.nc," ",this.delta.file," ",this.inter.file), wait=TRUE)
  system(paste0("cdo sinfon ",this.inter.file), wait=TRUE)
  #check file and plot
  this.terra.rast <- terra::rast(this.inter.file)
  this.terra.res <- terra::res(this.terra.rast)
  terra::plot(this.terra.rast)
  
  
  #Set all missing values to the nearest non missing value:
  #cdo setmisstonn infile outfile
  #https://code.mpimet.mpg.de/projects/cdo/embedded/index.html#x1-3220002.6.11
  
  #create name for miss value file
  this.miss.file <- gsub("_delta","_miss", this.delta.file)
  
  system(paste0("cdo setmisstonn ",this.inter.file," ",this.miss.file), wait=TRUE)
  #check file and plot
  this.terra.rast <- terra::rast(this.miss.file)
  this.terra.res <- terra::res(this.terra.rast)
  terra::plot(this.terra.rast)
  
  #create mask based on GLORYS file by setting all values above 0 to 1
  #cdo -gec,0 ERA5_land_sea_mask.nc ERA5_land_sea_mask_all_ones.nc
  #to set land values missing, ocean is 1
  #https://code.mpimet.mpg.de/boards/53/topics/10933
  
  #create name for ocean mask file
  this.mask.file <- paste0(delta.path,"ocean_mask.nc")
  
  #assign 1 to ocean and missing to file   
  system(paste0("cdo -gec,0 ",glorys.nc, " ",this.mask.file),wait=TRUE)
  this.terra.rast<- get_nc_description(this.mask.file)
  terra::plot(this.terra.rast)
  
  #create name for masked interpolated data
  this.maskinterp.file <- gsub("_delta","_minterp", this.delta.file)
  
  #divide the input data by the mask file data (the result of division by 0 is missing value)
  #cdo -div infile_r360x180.nc this.mask.file infile_r360x180_mask_land_2.nc
  system(paste0("cdo -div ",this.miss.file, " ",this.mask.file, " ", this.maskinterp.file),wait=TRUE)
  this.terra.rast<- get_nc_description(this.maskinterp.file)
  terra::plot(this.terra.rast)
  
}