#' Georeferrence all catch data
#'
#' @param catchdata 
#' @param georefcatchlocs
#' @param pac.buffer 
#'
#' @return georefcatchlocs
#' @export
#'
#' @examples
georef_catch_data <- function(georefcatchlocs, catchdata){
  
  georeff.codes <- georefcatchlocs %>% 
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
    sf::st_as_sf(coords = c("deci_lon", "deci_lat"), crs = 4269)
  
  sf::sf_use_s2(FALSE)
  
  #buffer Pacific to eliminate communities not in Gulf of California
  pac.polygon <- sf::st_read(here::here("data-raw","SHP","pacific_polygon.shp")) %>%   
    sf::st_transform(crs = 4269) 
  
  gc.polygon <- sf::st_read(here::here("data-raw","SHP","Golfo_california_wetland_poly_WGS84.shp")) %>%   
    sf::st_transform(crs = 4269) 
  
  pac.buffer <- pac.polygon %>% #https://epsg.io/4269#google_vignette
   sf::st_buffer(0.5)

#  gc.buffer <- gc.polygon %>% #https://epsg.io/4269#google_vignette
#    sf::st_buffer(0.5)
  
  catch.pacific <- catch.spatial %>% 
    sf::st_difference(pac.buffer) 

  catch.plot <- ggplot2::ggplot(gc.polygon) + 
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = catch.spatial, color = "red") +
    ggplot2::geom_sf(data = catch.pacific, color = "blue") +
    ggplot2::geom_sf(data = pac.polygon)

ggplot2::ggsave(catch.plot, here::here("outputs","catch_plot.png"), width = 10, height = 10)
    
  
  catch.geo.locs <- catch.spatial %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    dplyr::as_tibble() %>% 
    select(-geometry)
   
  readr::write_csv(catch.geo.locs, here::here("outputs","georefcatch_loc_month.csv"))
  
  return(catch.plot)
  
}