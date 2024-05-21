#' Georeferrence all catch data
#'
#' @param catchdata 
#' @param georeffcatchlocs
#' @param pac.buffer 
#'
#' @return georeffcatchlocs
#' @export
#'
#' @examples
georref_catch_data <- function(georeffcatchlocs, catchdata, pac.buffer){
  
  georeff.codes <- georeffcatchlocs %>% 
    dplyr::distinct(catch_code, deci_lat, deci_lon) 

  fresh.sp <- c("TRUCHA","CARPA", "BAGRE")
  
  catch.code <- catchdata %>%
    keep_when(!species %in% fresh.sp) %>%
    mutate(catch_code = paste(rnp_code, NOM_ENT, NOM_LOC, fishery_office, sep="_")) %>% 
    dplyr::left_join(georeff.codes, by = c("catch_code")) 
  
  georef.catch.data <- catch.code %>% 
    keep_when(!is.na(deci_lat)) %>% 
    dplyr::group_by(rnp_code, NOM_LOC, NOM_ENT, fishery_office, species, landed_w_kg, value_mxn,
                    year, month) %>%
    summarise(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon), .groups = 'drop')
  
  
  permitdata.loc <- permitdata %>% 
    select(-NOM_LOC) 
  
  missing.georef.catch.data <- catch.code %>% 
    keep_when(is.na(deci_lat)) 
  
  missing.geoloc <- missing.georef.catch.data %>% 
    dplyr::distinct(rnp_code, NOM_ENT, NOM_LOC, fishery_office, species, landed_w_kg, value_mxn,
                    year, month) %>% 
    dplyr::left_join(permitdata.loc, by = c("rnp_code","fishery_office","NOM_ENT")) 
  
  rnp.geoloc <- missing.geoloc %>%
    keep_when(!is.na(deci_lat)) %>% 
    dplyr::group_by(rnp_code, NOM_LOC, NOM_ENT, fishery_office, species, landed_w_kg, value_mxn,
                    year, month) %>%
    summarise(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon), .groups = 'drop')
  
  nocoords.data <- missing.geoloc %>% 
    keep_when(is.na(deci_lat)) %>% 
    distinct(NOM_ENT, NOM_LOC) 
  
  readr::write_csv(nocoords.data, here::here("outputs","nocoords_data.csv"))
  
  all.catch.code <- rbind(georef.catch.data, rnp.geoloc) %>% 
    dplyr::group_by(rnp_code, NOM_LOC, NOM_ENT, fishery_office, species, landed_w_kg, value_mxn,
                    year, month) %>%
    summarise(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon), .groups = 'drop')
  
  catch.spatial <- all.catch.code %>% 
    sf::st_as_sf(coords = c("deci_lon", "deci_lat"), crs = 4326)
  
  sf::sf_use_s2(FALSE)
  
  #buffer Pacific to eliminate communities not in Gulf of California
  #pac.polygon <- sf::st_read(here::here("data-raw","pac_mx_gc.shp")) %>%   
  #  sf::st_transform(crs = 4269) 
  
  gc.polygon <- sf::st_read(here::here("data-raw","SHP","Golfo_california_wetland_poly_WGS84.shp")) %>%   
    sf::st_transform(crs = 4269) 
  
#  pac.buffer <- pac.polygon %>% #https://epsg.io/4269#google_vignette
 #   sf::st_buffer(0.5)

  gc.buffer <- gc.polygon %>% #https://epsg.io/4269#google_vignette
    sf::st_buffer(0.5)
  
  catch.spatial <- catch.spatial %>% 
    sf::st_difference(gc.buffer) 

  ggplot2::ggplot(gc.buffer) + 
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = catch.spatial) +
  
  
  catch.geo.locs <- catch.spatial %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    dplyr::as_tibble()
   
  
  
  
  return(catch.geo.locs)
  
}