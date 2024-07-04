#' Merge GLORYS files
#'
#' @param glorys.monthly.files 
#' @param merged.dir 
#' @param glorys.name 
#'
#' @return
#' @export
#'
#' @examples
merge_glorys <- function(glorys.monthly.files, merged.dir, glorys.name){
  
  merge.file <- paste0(merged.dir, glorys.name)
  
  ClimateOperators::cdo("-mergetime", glorys.monthly.files, merge.file)
  
  #this option can be used to create smaller files, not sure how R treats netcdf v2 files
  #https://code.mpimet.mpg.de/boards/1/topics/908
  # ClimateOperators::cdo("-f nc2 mergetime", list.realization.files, merge.file)
  #read netcdf info
  
  ClimateOperators::cdo("sinfo",merge.file)
  
  clim.glorys.file <- gsub("_merged.nc","_clim.nc", merge.file)
  
  #calculate climatology, monthly means across time period 
  ClimateOperators::cdo("ymonmean", merge.file, clim.glorys.file)
  ClimateOperators::cdo("sinfo",clim.glorys.file)
  
  return(clim.glorys.file)
  
}
