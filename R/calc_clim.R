#' Calculate climatology and delta anomalies
#'
#' @param this.rel 
#' @param merged.hist.files 
#' @param merged.future.files 
#'
#' @return delta.plot plot of anomalies
#' @export
#'
#' @examples
cal_clim <- function(this.rel, merged.hist.files, merged.future.files){
  
  hist.file <- grep(this.rel, merged.hist.files, value=TRUE)
  future.file <- grep(this.rel, merged.future.files, value=TRUE)
  
  clim.hist.file <- gsub("merged","clim", hist.file)
  delta.future.file <- gsub("merged","delta", future.file)
  #calculate climatology, monthly means across time period 
  ClimateOperators::cdo("ymonmean", hist.file, clim.hist.file)
  ClimateOperators::cdo("sinfo",clim.hist.file)
  clim.rast <- terra::rast(clim.hist.file)
  terra::plot(clim.rast)
  #calculate anomalies, deltas by substracting climatology from projections   
  ClimateOperators::cdo("ymonsub", future.file, "-ymonmean", hist.file, delta.future.file)
  ClimateOperators::cdo("sinfo",delta.future.file)
  delta.rast <- terra::rast(delta.future.file)
  delta.plot <- terra::plot(delta.rast)
  
  return(delta.plot)
}
