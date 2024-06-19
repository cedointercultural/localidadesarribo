#' Georeferrence all catch data
#'
#' @param catchdata 
#' @param georefcatchlocs
#'
#' @return catch.plot Plot of catch locations in the Gulf of California and point in the Pacific that were eliminated
#' @export
#'
#' @examples
georef_catch_data <- function(georefcatchlocs, catchdata){
  
  georeff.codes <- georefcatchlocs %>% 
    dplyr::distinct(catch_code, deci_lat, deci_lon) 
  
  #eliminate freshwater and aquarium species 
  fresh.sp <- c("TRUCHA","CARPA", "BAGRE", "ORNATO", "PECES DE ORNATO", "OTRAS")
  
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
    summarise(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon), .groups = 'drop') %>% 
    #eliminate all locations south of Cabo Corrientes 20.43386153865933, -105.69209230968688
    keep_when(deci_lat>20.43386153865933)
  
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
  
  sf::st_write(catch.pacific, dsn="outputs/gpkg_grid.gpkg", layer='goc_catch', layer_options = "OVERWRITE=YES", append = FALSE)
  
  
  catch.plot <- ggplot2::ggplot(gc.polygon) + 
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = catch.spatial, color = "red") +
    ggplot2::geom_sf(data = catch.pacific, color = "blue") +
    ggplot2::geom_sf(data = pac.polygon) +
    ggplot2::labs(x = "Longitude",
                  y="Latitude",
                  title = paste0("Georeferenced catch data:", nrow(catch.spatial), " points"),
                  subtitle = "Blue points are catch data in the Gulf of California, \n Red points are catch data in the Pacific Ocean")
  # 
  
  ggplot2::ggsave(filename= "catch_plot.png", plot=catch.plot, path=here::here("outputs"), width = 10, height = 10)
  
  catch.geo.locs <- catch.spatial %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    dplyr::as_tibble() %>% 
    select(-geometry)
 
  catch.geo.pacific <- catch.pacific %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    dplyr::as_tibble() %>% 
    select(-geometry)
  
   
  readr::write_csv(catch.geo.locs, here::here("outputs","georefcatch_loc.csv"))
  readr::write_csv(catch.geo.pacific, here::here("outputs","georefcatch_loc_pacific.csv"))
  
  
  catch.plot.inter <- ggplot2::ggplot(gc.polygon) + 
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = catch.pacific, ggplot2::aes(color = species)) +
    ggplot2::labs(x = "Longitude",
                  y="Latitude",
                  title = "Georeferenced catch data",
                  subtitle = "Colors are different species") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
ggplot2::ggsave(filename= "catch_plot_sp.png", plot=catch.plot.inter, path=here::here("outputs"), width = 10, height = 10)
  
  
  catch.ngoc <- catch.pacific %>% 
    dplyr::mutate(deci_lon = sf::st_coordinates(.)[,1],
                  deci_lat = sf::st_coordinates(.)[,2]) %>% 
    keep_when(deci_lat>28)
  
  catch.plot.ngoc <- ggplot2::ggplot(gc.polygon) + 
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = catch.ngoc, ggplot2::aes(color = species)) +
    ggplot2::labs(x = "Longitude",
                  y="Latitude",
                  title = "Georeferenced catch data",
                  subtitle = "Colors are different species") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom")
  
  ggplot2::ggsave(filename= "catch_plot_ngoc.png", plot=catch.plot.ngoc, path=here::here("outputs"), width = 10, height = 10)
  

  return(catch.plot)
  
}