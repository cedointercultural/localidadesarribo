#' Get GLORYS climatology data
#'
#' @param min_lon 
#' @param max_lon 
#' @param min_lat 
#' @param max_lat 
#' @param out_dir 
#' @param data.set.id 
#'
#' @return
#' @export
#'
#' @examples
get_climatologyglorys<- function(min_lon, max_lon, min_lat, max_lat, out_dir, data.set.id){
  
  cm$subset(
    dataset_id = data.set.id,
    variables = list(var.id),
    minimum_longitude = min_lon,
    maximum_longitude = max_lon,
    minimum_latitude = min_lat,
    maximum_latitude = max_lat,
    minimum_depth= 0,
    maximum_depth= 500,
    output_directory = out_dir,
    force_download = TRUE,
  )
  
  Sys.sleep(15)
  
}
