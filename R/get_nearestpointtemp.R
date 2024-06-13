#' Obtain temp values for catch
#'
#' @param this.time.interval 
#' @param this.depth 
#' @param catch.pacific.spatial 
#' @param glorys.files 
#' @param catch.geo 
#' @param start.time.interval 
#'
#' @return
#' @export
#'
#' @examples
get_nearestpointtemp <- function(this.time.interval, this.depth, catch.pacific.spatial, glorys.files, catch.geo, start.time.interval, out.dir){
  
  time.interval <- start.time.interval[this.time.interval]
  
  this.year.no <- time.interval %>% 
    strsplit("-") %>% 
    unlist() %>% 
    .[1]
  
  this.month.no <- time.interval %>% 
    strsplit("-") %>% 
    unlist() %>% 
    .[2]
  
  this.catch.spatial <- catch.pacific.spatial %>%
    dplyr::filter(year==this.year.no, month_no==this.month.no) %>% 
    dplyr::mutate(id_pol=1:nrow(.))
  
  this.catch.geo <- catch.geo %>% 
    dplyr::filter(year==this.year.no, month_no==this.month.no)
  
  this.date <- paste0(this.year.no,"-",this.month.no)
  
  this.glorys.file <- grep(this.date, glorys.files, value = TRUE)
  
  
  glorys.rast <- terra::rast(this.glorys.file) %>% 
      terra::project("EPSG:4269")
    
  #terra::plot(glorys.rast)
    
  glorys.points <- terra::as.points(glorys.rast)
    
  glorys.point.sf <- sf::st_as_sf(glorys.points) %>% 
      dplyr::mutate(id_pol=1:nrow(.))
  
  point.no <- 1:nrow(this.catch.spatial)
  
  point.depth <- lapply(point.no, get_temppoint, glorys.point.sf, this.catch.spatial, this.depth, this.catch.geo) %>% 
    dplyr::bind_rows()
  
  data.table::fwrite(point.depth, here::here(out.dir,paste(this.date,"catchtempdata.csv",sep="_")))
  

}
