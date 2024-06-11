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
  
  glorys.file <- glorys.files[eachglorysfile]
  
  year_month <- stringr::str_extract(glorys.file, "[0-9]{4}-[0-9]{2}")
  
  year_sel <- year_month %>% strsplit("-") %>% unlist() %>% .[1]
  month_sel <- year_month %>% strsplit("-") %>% unlist() %>% .[2]
  
  glorys.rast <- terra::rast(glorys.file) %>% 
    terra::project("EPSG:4269")
  terra::plot(glorys.rast)
  
  this.rast.data <- terra::mask(glorys.rast, this.ocean.grid) %>% 
    tidyterra::drop_na(.)
  
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
    dplyr::mutate(mean_depth = mean.depth, year = year_sel, month = month_sel)
  
  this.point.depth <- dplyr::bind_cols(this.catch.geo, depth.temps)
  
  glorys.nc = tidync::tidync(this.rast.data) %>%
    tidync::hyper_tibble()%>%
    dplyr::filter(depth == min(depth))
  
  this.rast.tibble <- tidyterra::as_tibble(this.rast.data) 
  
  list.tempvalues[[eachglorysfile]] <- this.rast.tibble
  
  return(list.tempvalues)
  
}
