#' Make ocean grid
#'
#' @param catch.geo.pacific 
#'
#' @return gc.grid.plot
#' @export
#'
#' @examples
make_grid <- function(){
  
  catch.pacific.spatial <- sf::st_read(dsn="outputs/gpkg_grid.gpkg", layer='goc_catch')  %>% 
    sf::st_transform(4269)
  
  sf::sf_use_s2(FALSE)
  
  gc.polygon <- sf::st_read(here::here("data-raw","SHP","Golfo_california_wetland_poly_WGS84.shp")) %>%   
    sf::st_transform(crs = 4269) %>% 
    sf::st_make_valid()
  
  #simplify GOC polygon otherwise it takes too much memory for intersection
  #https://www.r-bloggers.com/2021/03/simplifying-geospatial-features-in-r-with-sf-and-rmapshaper/
  sf::sf_use_s2(TRUE)
  
  gc.polygon.simple <- sf::st_simplify(gc.polygon, preserveTopology = FALSE, dTolerance = 3000)
  
  
  #make grid at scale of finest oceanographic data
  #0.083° × 0.083° degree grid Global Ocean Physics Reanalysis
  #31 Dec 1992 to 26 Feb 2024 50 vertical levels
  #https://data.marine.copernicus.eu/product/GLOBAL_MULTIYEAR_PHY_001_030/description
  #0.25° × 0.25° degree grid Global Ocean Biogeochemistry Hindcast
  #1993-2019 75 vertical levels
  
  gc.polygon.grid <- sf::st_make_grid(gc.polygon.simple, cellsize = c(0.083, 0.083), what = "polygons") %>% 
    sf::st_as_sf() %>% 
    sf::st_set_crs(4269) 
  #https://epsg.io/4269
  
  gc.ocean.grid <- sf::st_intersection(gc.polygon.grid, gc.polygon.simple) 
  
  gc.grid.plot <- ggplot2::ggplot(gc.ocean.grid) + 
    ggplot2::geom_sf() +
    ggplot2::geom_sf(data = gc.polygon.simple, color = "red", alpha=0.4) +
    ggplot2::geom_sf(data = catch.pacific.spatial, color = "darkblue") +
    ggplot2::labs(x = "Longitude",
                  y="Latitude",
                  title = "Georeferenced catch data",
                  subtitle = "Grid 0.083° × 0.083° degree")
  
  #https://mapping-in-r-workshop.ryanpeek.org/02_import_export_gpkg
  # If an existing layer already exists, to overwrite the LAYER add: ( layer_options = "OVERWRITE=YES" )
 # sf::st_write(gc.polygon.simple, dsn="outputs/gpkg_grid.gpkg", layer='goc_simple', layer_options = "OVERWRITE=YES", append = FALSE)
 # sf::st_write(gc.ocean.grid, dsn="outputs/gpkg_grid.gpkg", layer='goc_grid', layer_options = "OVERWRITE=YES", append = FALSE)
  
  sf::st_write(gc.ocean.grid, dsn="outputs/goc_grid.shp", delete_layer = TRUE)
  sf::st_write(gc.polygon.simple, dsn="outputs/goc_simple_polygon.shp", delete_layer = TRUE)
  
  # Caution/Note, if you want to overwrite the entire gpkg database file, use "delete_dsn = TRUE". This replaces the file and everything that may have been in it!
  
  return(gc.grid.plot)
  
}
