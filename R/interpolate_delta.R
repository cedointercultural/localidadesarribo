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
  
  is_3d <- ifelse(grepl('zos',this.delta.file),F,T)
  
  # vertical levels as a character string
  zlevs <- tidync::tidync(glorys.nc) %>% 
    tidync::activate("D0") %>% 
    tidync::hyper_tibble() %>% 
    dplyr::pull('depth') %>% 
    paste(collapse=",")
  
  this.inter.file <- gsub("_delta","_interp", this.delta.file)
  print(this.inter.file)
  
  #note this is used only for 3D variables
  system(paste0("cdo -remapdis,","glorys3d.grd"," ",this.delta.file," ",this.inter.file), wait=TRUE)
  
  #  system(paste0("cdo intlevel,",zlevs, " -selname,tos ",this.delta.file," ",this.inter.file), wait=TRUE)
  # cdo intlevel,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200 -selname,thetao thetao_Omon_inmcm4_historical_r1i1p1_197001-197412.nc interim.nc
  
  # system(paste0("cdo -intlevel3d,",zlevs," -remapdis,","glorys3d.grd"," ",this.delta.file," ",this.inter.file), wait=TRUE)
  #ClimateOperators::cdo("-remapdis,", "glorys3d.grd", this.delta.file, this.inter.file)
  #ClimateOperators::cdo("-intlevel,",zlevs,"-remapdis", "glorys3d.grd", this.delta.file, this.inter.file)
  
  system(paste0("cdo -O remapdis,",glorys.nc, " ",this.delta.file," ",this.inter.file), wait =TRUE)
}

