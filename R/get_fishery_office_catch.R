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

  googlesheets4::gs4_deauth()
  
  #CONTINUE HERE, ADD COL NAMES
  fish.offices.conapesca <- googlesheets4::read_sheet("1FS2Rs9rL5OQH4n4PM5HmYJqQ0RGtWgWBoC1njpoHXRY") %>% 
    keep_when(CVE_LOC!="NULL") %>% 
    mutate(CVE_LOC = as.character(CVE_LOC)) %>% 
    dplyr::pull(CVE_LOC)

  fish.offices.catch <- catchdata %>%
    dplyr::distinct(fishery_office, NOM_ENT) %>%
    dplyr::rename(NOM_LOC = fishery_office) %>% 
    dplyr::left_join(inegigc, by=c("NOM_ENT","NOM_LOC")) %>% 
    dplyr::mutate(CVE_LOC = as.character(CVE_LOC)) %>% 
    keep_when(CVE_LOC %in% fish.offices.conapesca)
  
  
  #fish.offices.catch %>% keep_when(is.na(`Oficina de Pesca`)) %>% View()  
    
    
  
  return(fish.offices.catch)
}
