get_clustercoords <- function(this.cluster, cluster.path){
  
  print(this.cluster)    
  this.cluster.points <- cluster.future %>% 
    dplyr::filter(Cluster_Label==this.cluster)
  
  convex.hull <- sf::st_convex_hull(sf::st_union(this.cluster.points)) 
  spvector.convex.hull <- sf::as_Spatial(convex.hull) 
  coord.pol <- spvector.convex.hull@polygons[[1]]@Polygons[[1]]@coords %>% as.data.frame()
  plot(convex.hull)
  
  cluster.file <- paste0(cluster.path,"/cluster_coords_",this.cluster,".txt")
  file.create(cluster.file)
  cat("# lon lat", sep = "\n", file = cluster.file, append = TRUE)
  
  
  for(eachrow in 1:nrow(coord.pol)){
    
    this.row <- coord.pol[eachrow,] 
    this.row[,'V1']=format(round(this.row[,'V1'],8),nsmall=8)
    this.row[,'V2']=format(round(this.row[,'V2'],6),nsmall=6)
    
    this.char <- this.row %>% 
      as.character() %>% 
      stringr::str_trim() %>% 
      paste0(.,collapse=" ")
    
    cat(this.char, sep = "\n", file = cluster.file, append = TRUE)
    
  }
  
}
