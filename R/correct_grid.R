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
correct_grid <- function(this.file, this.var, weight.dir){
  
  #this.file <- "/home/atlantis/storagebucket/ESM_CMIP6/MPI_ESM/tos_Omon_MPI-ESM1-2-HR_historical_r10i1p1f1_gn_198501-198912.nc"
  print(this.file)
  this.coord.file <- gsub("[:.:]nc","_coord.nc", this.file)
  #create weight file  
  weight.file <- stringr::str_split(this.file, pattern="/") %>% 
    unlist %>% 
    .[7] %>% 
    gsub("[:.:]nc","_weight.nc", .) %>% 
    paste0(weight.dir,.)
    
 
  #use cdo to regrid the files, this will assign a lat and lon dimension
  # remap / converts from the native grid to a generic cartesian grid
  # https://github.com/trondkr/cmip6
  #MPI-ESM1 output has lat lon as variables, not dimensions so it cannot be used directly
  #generate a weights file
  ClimateOperators::cdo("sinfo", this.file)
  ClimateOperators::cdo(paste0("genbil,r802x404 ", this.file," ", weight.file, sep="")) 
  
   
  #Use weight file to regrid the file   
  ClimateOperators::cdo(paste0("remap,r802x404,",weight.file," ", this.file," ",this.coord.file,sep=""))
  ClimateOperators::cdo("sinfo", this.coord.file)
  }