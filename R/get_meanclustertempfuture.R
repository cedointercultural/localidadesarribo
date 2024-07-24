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
get_meanclustertempfuture <- function(this.time.interval, future.files, cluster.locs, num.clusters, time.interval, start.time.interval, out.dir){
  
  #open each file once and loop through all the clusters
  
  dir.create(out.dir)
  
  this.date <- start.time.interval[this.time.interval]
  
  print(this.date)
  out.file <- paste(this.date,"catchtempdata.csv",sep="_")
  
  this.future.file <- paste0(this.date,".nc")
  
  if(file.exists(here::here(out.dir,out.file))==FALSE){
    
    this.glorys.file <- grep(this.future.file, future.files, value = TRUE)
    
    glorys.rast <- terra::rast(this.glorys.file) %>% 
      terra::project("EPSG:4269")
    
    depth.lyrs.10 <- c("thetao_sfc=6","thetao_sfc=7.92956018447876","thetao_sfc=9.572997093200684")
    
    #extract depth layers < 10m, first eight layers
    #depth.layers <- c("thetao_depth=0.49402499","thetao_depth=1.541375","thetao_depth=2.645669","thetao_depth=3.819495","thetao_depth=5.0782242",  "thetao_depth=6.4406142", "thetao_depth=7.9295602", "thetao_depth=9.5729971")
    
    depth.glorys.10 <- terra::mean(terra::subset(glorys.rast, terra::names(glorys.rast)[1:length(depth.lyrs.10)]))
    depth.glorys <- glorys.rast
    depth.glorys.30 <- terra::mean(depth.glorys, na.rm=TRUE)
    
        
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
        
          
          convex.hull <- sf::st_convex_hull(sf::st_union(this.cluster)) 
          
          spvector.convex.hull <- sf::as_Spatial(convex.hull) 
          spat.convex.hull <- terra::vect(spvector.convex.hull)
          
          convex.depth.temp <- terra::extract(depth.glorys, spat.convex.hull, fun=mean, na.rm=TRUE) %>% 
            dplyr::mutate(Cluster_Label=eachcluster) %>% 
            dplyr::select(-ID)
          
          #r_avg <- mean(STACK)
          convex.temp.30 <- terra::extract(depth.glorys.30, spat.convex.hull, fun=mean, na.rm=TRUE) %>% 
            dplyr::mutate(Cluster_Label=eachcluster) %>% 
            dplyr::select(-ID)
          convex.temp.10 <- terra::extract(depth.glorys.10, spat.convex.hull, fun=mean, na.rm=TRUE) %>% 
            dplyr::mutate(Cluster_Label=eachcluster) %>% 
            dplyr::select(-ID)
          
          
          this.cluster.frame <- this.cluster %>% 
            dplyr::as_tibble() %>% 
            dplyr::mutate(cluster = eachcluster,
                          date = this.date,
                          mean_temp_30m = convex.temp.30$mean,
                          mean_temp_10m = convex.temp.10$mean) %>% 
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

