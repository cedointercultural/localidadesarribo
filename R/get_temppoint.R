get_temppoint <- function(thispointspatial, glorys.point.sf, this.catch.spatial, this.depth, this.catch.geo){
  
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
  
  return(this.point.depth)
  
  
}
