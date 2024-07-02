#' Get temperature by cluster
#'
#' @param this.time.interval 
#' @param glorys.files 
#' @param num.clusters 
#' @param time.interval 
#' @param start.time.interval 
#' @param out.dir 
#'
#' @return
#' @export
#'
#' @examples
get_meanclustertemp <- function(this.time.interval, glorys.files, cluster.locs, num.clusters, time.interval, start.time.interval, out.dir){
  
  #open each GLORYS file once and loop through all the clusters
  
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
    
    this.glorys.file <- grep(this.date, glorys.files, value = TRUE)
    
    glorys.rast <- terra::rast(this.glorys.file) %>% 
      terra::project("EPSG:4269")
    
    #extract depth layers < 10m, first eight layers
    #depth.layers <- c("thetao_depth=0.49402499","thetao_depth=1.541375","thetao_depth=2.645669","thetao_depth=3.819495","thetao_depth=5.0782242",  "thetao_depth=6.4406142", "thetao_depth=7.9295602", "thetao_depth=9.5729971")
    
    depth.glorys <- terra::subset(glorys.rast, terra::names(glorys.rast)[1:8])
    mean.glorys <- terra::mean(depth.glorys, na.rm=TRUE)
    
    this.date.cluster.locs <- cluster.locs %>%
      dplyr::filter(year==as.numeric(this.year.no), month_no==as.numeric(this.month.no)) 
    
    cluster.list <- list()
    
    if(nrow(this.date.cluster.locs) > 0){
      
      for(thiscluster in 1:length(num.clusters)){
        
        eachcluster <- num.clusters[thiscluster]
        print(eachcluster)
        this.cluster <- this.date.cluster.locs %>% 
          dplyr::filter(Cluster_Label == eachcluster) 
        
        if(nrow(this.cluster) > 0){
        
          
          convex.hull <- st_convex_hull(st_union(this.cluster)) 
          
          spvector.convex.hull <- sf::as_Spatial(convex.hull) 
          spat.convex.hull <- terra::vect(spvector.convex.hull)
          
          convex.depth.temp <- terra::extract(depth.glorys, spat.convex.hull, fun=mean, na.rm=TRUE) %>% 
            dplyr::mutate(Cluster_Label=eachcluster) %>% 
            dplyr::select(-ID)
          
          convex.temp <- terra::extract(mean.glorys, spat.convex.hull, fun=mean, na.rm=TRUE) %>% 
            dplyr::select(-ID)
          
          this.cluster.frame <- this.cluster %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(cluster = eachcluster,
                          date = this.date,
                          mean_temp = convex.temp$mean) %>% 
            dplyr::left_join(convex.depth.temp, by = "Cluster_Label")
          
          cluster.list[[thiscluster]] <- this.cluster.frame
          
          #use to visualize polygon     
          # gc.grid.plot <- ggplot2::ggplot() + 
          #     tidyterra::geom_spatraster(data = depth.glorys) +
          #     ggplot2::geom_sf() +
          #     ggplot2::geom_sf(data = convex.hull, color = "red", alpha=0.4) +
          #     ggplot2::geom_sf(data = catch.pacific.spatial, color = "darkblue") +
          #     ggplot2::labs(x = "Longitude",
          #                   y="Latitude",
          #                   title = "Georeferenced catch data",
          #                   subtitle = "Grid 0.083° × 0.083° degree")
          
        } # end loop this cluster date
        
        
      } # end loop through clusters
      
      this.cluster.temps <- dplyr::bind_rows(cluster.list)
      data.table::fwrite(this.cluster.temps, here::here(out.dir,out.file))
      
    } #end cluster by date has rows
    
    
    
  } # end file.exists
} # end function

