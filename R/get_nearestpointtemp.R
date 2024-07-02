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
  
  this.start.time.interval <- start.time.interval[this.time.interval]
  this.year.no <- this.start.time.interval %>% 
    strsplit("-") %>% 
    unlist() %>% 
    .[1]
  
  this.month.no <- this.start.time.interval %>% 
    strsplit("-") %>% 
    unlist() %>% 
    .[2]
  
  this.date <- paste0(this.year.no,"-",this.month.no)
  
  print(this.date)
  out.file <- paste(this.date,"catchtempdata.csv",sep="_")
  
  if(file.exists(here::here(out.dir,out.file))==FALSE){
    
    print("File not found, analyzing")  
    this.catch.spatial <- catch.pacific.spatial %>%
      dplyr::filter(year==this.year.no, month_no==this.month.no) %>% 
      dplyr::mutate(id_pol=1:nrow(.))
    
    this.catch.geo <- catch.geo %>% 
      dplyr::filter(year==this.year.no, month_no==this.month.no)
    
    this.glorys.file <- grep(this.date, glorys.files, value = TRUE)
    
    glorys.rast <- terra::rast(this.glorys.file) %>% 
      terra::project("EPSG:4269")
    
    #terra::plot(glorys.rast)
    
    glorys.points <- terra::as.points(glorys.rast)
    
    glorys.point.sf <- sf::st_as_sf(glorys.points) %>% 
      dplyr::mutate(id_pol=1:nrow(.))
    
    point.no <- 1:nrow(this.catch.spatial)
    
    point.data.list <- list()
    
    for(thispointspatial in point.no){
      
      this.point.catch.spatial <- this.catch.spatial[thispointspatial,]
      this.point.catch.geo <- this.catch.geo[thispointspatial,]
      
      #print(thispointspatial)
      
      #print(this.point.catch.spatial)
      
      
      this.distance <- sf::st_distance(this.point.catch.spatial, glorys.point.sf)
      
      min.distance <- min(this.distance)
      
      indices <- which(this.distance == units::set_units(min.distance, "m"), arr.ind = TRUE)
      
      df.dist <- as.data.frame(indices)
      df.dist$dist <- as.numeric(this.distance[indices])
      
      catch.indices <- df.dist$col
      
      this.temp <- glorys.point.sf %>% 
        dplyr::filter(id_pol %in% catch.indices)
      
      #print(this.temp)
      
      depth.temps <- this.temp %>% 
        sf::st_drop_geometry() %>% 
        tidyr::pivot_longer(cols=1:31, names_to="depth", values_to = "temp") %>% 
        tidyr::separate(depth, into=c("thetao", "depth"), sep="=") %>%
        dplyr::select(-thetao, -id_pol) %>% 
        dplyr::mutate(depth = as.numeric(depth)) %>% 
        dplyr::filter(depth<=this.depth)
      
      mean.temp <- mean(depth.temps$temp)
      
      this.point.depth <- depth.temps %>% 
        dplyr::mutate(depth = as.character(depth), depth = paste0(depth,"_m")) %>%
        tidyr::pivot_wider(names_from = depth, values_from = temp) %>%
        dplyr::bind_cols(this.point.catch.geo, .) %>% 
        dplyr::select(year, month_no, month, everything()) %>% 
        dplyr::mutate(mean_temp = mean.temp)
      
      point.data.list[[thispointspatial]]<- this.point.depth
    }
    
    point.depth <- dplyr::bind_rows(point.data.list)
    
    data.table::fwrite(point.depth, here::here(out.dir,out.file))
    
    return("file analyzed")
    
  } else {
    
    return("file already done")
  }
}
