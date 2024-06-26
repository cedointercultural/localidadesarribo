#' Get monthly GLORYS data for each file
#'
#' @param eachglorysfile 
#' @param glorys.files 
#' @param this.ocean.grid 
#' @param this.catch.geo 
#' @param this.depth 
#' @param list.tempvalues 
#'
#' @return list.tempvalues
#' @export
#'
#' @examples
get_glorysdata <- function(eachglorysfile, glorys.files, this.ocean.grid, this.catch.geo, this.depth, list.tempvalues){
  
  print(eachglorysfile)
  glorys.file <- glorys.files[eachglorysfile]
  print(glorys.file)
  
  year_month <- stringr::str_extract(glorys.file, "[0-9]{4}-[0-9]{2}")
  print(year_month)
  
  this.date <- paste0(this.catch.geo$year,"-",this.catch.geo$month_no)
  
  if(year_month==this.date){
    print("Match")

    year_sel <- year_month %>% strsplit("-") %>% unlist() %>% .[1]
    month_sel <- year_month %>% strsplit("-") %>% unlist() %>% .[2]
    
    glorys.rast <- terra::rast(glorys.file) %>% 
      terra::project("EPSG:4269")
    
    terra::plot(glorys.rast)
    
    glorys.points <- terra::as.points(glorys.rast)
    
    glorys.point.sf <- sf::st_as_sf(glorys.points)
    
    this.distance <- sf::st_distance(this.catch.spatial, gc.ocean.grid)
    
    
    this.rast.data <- terra::mask(glorys.rast, this.ocean.grid) %>% 
      tidyterra::drop_na(.)
    
  } else {
    print("No match")
  }
  
  
  mean.raster <- terra::global(this.rast.data, 'mean', na.rm=TRUE) %>% 
    tibble::rownames_to_column() %>% 
    tidyr::separate(rowname, c("depth_name", "depth"), sep = "=") %>%
    dplyr::mutate(depth = as.numeric(depth)) %>%
    dplyr::filter(depth<this.depth)
  
  mean.depth <- mean(mean.raster$mean)
  
  depth.temps <- mean.raster %>% 
    dplyr::select(-depth_name) %>% 
    dplyr::mutate(depth = paste0("depth_",as.character(depth))) %>% 
    tidyr::pivot_wider(names_from = depth, values_from = mean) %>% 
    dplyr::mutate(mean_temp = mean.depth, year_temp = year_sel, month_temp = month_sel)
  
  this.point.depth <- dplyr::bind_cols(this.catch.geo, depth.temps)
  print(this.point.depth)
 
  list.tempvalues[[eachglorysfile]] <- this.point.depth
  
  return(list.tempvalues)
  
}
