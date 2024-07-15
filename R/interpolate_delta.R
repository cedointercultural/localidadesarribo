#' Interpolate delta anomaly
#'
#' @param this.delta.file 
#' @param glorys.nc 
#'
#' @return inter.raster
#' @export
#'
#' @examples
interpolate_delta <- function(this.delta.file, glorys.nc){
  
  print(this.delta.file)
  ClimateOperators::cdo("-sinfo",this.delta.file)
  
  # GLORYS grid file (only have to run once)
  ClimateOperators::cdo("griddes",glorys.nc,">","glorys3d.grd") 
  ClimateOperators::cdo("zaxisdes",glorys.nc)  #shows depth, z axis
  
  #is_3d <- ifelse(grepl('zos',this.delta.file),F,T)
  
  # vertical levels as a character string
  zlevs.glorys <- tidync::tidync(glorys.nc) %>% 
    tidync::activate("D3") %>% 
    tidync::hyper_tibble() %>% 
    dplyr::pull('sfc') %>% 
    paste(collapse=",")
 
  zlevs.esm <- tidync::tidync(this.delta.file) %>% 
    tidync::activate("D4") %>% 
    tidync::hyper_tibble() %>% 
    dplyr::pull('lev') %>% 
    paste(collapse=",")
  
   
  this.inter.file <- gsub("_delta","_interp", this.delta.file)
  print(this.inter.file)
  
  #note this is used only for 3D variables

  #system(paste0("cdo -intlevel3d,",zlevs.glorys," -remapdis,","glorys3d.grd"," ",this.delta.file," ",this.inter.file), wait=TRUE)
  
  system(paste0("cdo -intlevel,",zlevs.glorys," -remapdis,","glorys3d.grd"," ",this.delta.file," ",this.inter.file), wait=TRUE)
  ClimateOperators::cdo("sinfon",this.delta.file)
  ClimateOperators::cdo("sinfon",this.inter.file)
  system(paste0("cdo -remapdis,", "glorys3d.grd ", this.delta.file," ", this.inter.file), wait=TRUE)
  #ClimateOperators::cdo("-intlevel,",zlevs,"-remapdis", "glorys3d.grd", this.delta.file, this.inter.file)

 delta.rast <- terra::rast(this.delta.file)
 terra::plot(delta.rast)
  
inter.rast <- terra::rast(this.inter.file)
terra::plot(inter.rast)  
#system(paste0("ncdump ",this.inter.file," > test.cdf")) 

this.inter.file <- gsub("_delta","_interp2", this.delta.file)

#START HERE
#use cdo to only interpolate vertically
system(paste0("cdo -intlevel,",zlevs.glorys," ",this.delta.file," ",this.inter.file), wait=TRUE)
ClimateOperators::cdo("sinfon",this.inter.file)

inter.rast <- terra::rast(this.inter.file)
terra::plot(inter.rast)  

#use nco to interpolate 

  }

