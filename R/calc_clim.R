#' Calculate climatology and delta anomalies
#'
#' @param this.rel 
#' @param merged.hist.files 
#' @param merged.future.files 
#'
#' @return 
#' @export
#'
#' @examples
cal_clim <- function(this.rel, depth.hist.files, depth.future.files){
  
  hist.file <- grep(this.rel, depth.hist.files, value=TRUE)
  future.file <- grep(this.rel, depth.future.files, value=TRUE)
  
  print(hist.file)
  print(future.file)
  clim.hist.file <- gsub("depth","clim", hist.file)
  delta.future.file <- gsub("depth","delta", future.file)
  delta.future.file <- gsub("merged_files","delta_files", delta.future.file)
  #calculate climatology, monthly means across time period 
  print("calculate climatology")
  ClimateOperators::cdo("ymonmean", hist.file, clim.hist.file)
  ClimateOperators::cdo("sinfo",clim.hist.file)
  clim.rast <- terra::rast(clim.hist.file)
  terra::plot(clim.rast)
  print("calculate anomalies")
  #calculate anomalies, deltas by substracting climatology from projections   
  ClimateOperators::cdo("ymonsub", future.file, "-ymonmean", hist.file, delta.future.file)
  ClimateOperators::cdo("sinfo",delta.future.file)
  
  return(delta.future.file)
}
