#' @title Get fishery office from catch records
#'
#' @param this.catchfileno
#' @param catch.files catch files
#' @param coast.selecc selected coastal states
#' @param inegigc all communities in Gulf of California coastal states
#' @param locs.pesca corrected location names
#' 
#' @return catch.file.rnp
#' @export
#'
#' @examples
get_fishery_office_catch <- function(catchdata){


  fish.offices <- catchdata %>%
    dplyr::distinct(fishery_office, NOM_ENT) %>%
    dplyr::rename(NOM_LOC = fishery_office) %>% 
    dplyr::left_join(inegigc, by=c("NOM_ENT","NOM_LOC")) 
    
    
  
  return(fish.offices)
}
