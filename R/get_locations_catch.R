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
    distinct(rnp_code, NOM_ENT, NOM_LOC, fishery_office)

  return(catch.locations)
}
