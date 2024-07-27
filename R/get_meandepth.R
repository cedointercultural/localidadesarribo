#' Get mean depth data
#'
#' @param this.file 
#' @param depthname description
#'
#' @return this.file.data
#' @export
#'
#' @examples
get_meandepth <- function(this.file, depthname, eachcluster){
  
  #get data for monthly mean aggregated by depth
  vert.nc.tibble <- tidync::tidync(this.file) %>% 
    tidync::hyper_tibble() 
  #extract time
  tunit <- ncmeta::nc_atts(this.file, "time") %>% 
    tidyr::unnest(cols = c(value)) %>% 
    dplyr::filter(name == "units")
  #convert time into date
  vert.time.date <- RNetCDF::utcal.nc(tunit$value, vert.nc.tibble$time) %>% 
    tidyr::as_tibble() %>% 
    dplyr::select(year, month)
  #create data tibble
  vert.this.file.data <- vert.nc.tibble %>% 
    dplyr::select(-lon, -lat, -time) %>% 
    dplyr::bind_cols(vert.time.date) %>% 
    dplyr::distinct(year,month) %>% 
    dplyr::mutate(cluster = eachcluster, depth_m = depthname)
  
  return(vert.this.file.data)
}
