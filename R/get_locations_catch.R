#' @title Get fishery office from catch records
#'
#' @param catchdata timeseries of catch for Gulf of California states
#' 
#' @return catch.locations
#' @export
#'
#' @examples
get_locations_catch <- function(catchdata){

  catch.locations <- catchdata %>%
    distinct(rnp_code, NOM_ENT, NOM_LOC, fishery_office) %>% 
    dplyr::mutate(fishery_office = dplyr::if_else(fishery_office=="PENITA DE JALTEMBA","LA PENITA DE JALTEMBA", 
                                           dplyr::if_else(fishery_office=="LA PENITA","LA PENITA DE JALTEMBA", 
                                                          dplyr::if_else(fishery_office=="CABO SAN LUCAS","LOS CABOS", 
                                                                         dplyr::if_else(fishery_office=="ESCUINAPA","ESCUINAPA DE HIDALGO",
                                                                                        dplyr::if_else(fishery_office=="CRUZ DE HUANACAXTLE","LA CRUZ DE HUANACAXTLE", fishery_office))))))
  return(catch.locations)
}
