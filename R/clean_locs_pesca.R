
#' Import and prepare list of fishery names
#'
#' @return locs.pesca
#' @export
#'
#' @examples
clean_locs_pesca <- function(){
  
  ents.coast.selecc <- toupper(c("Baja California Sur", "Baja California", "Sonora", "Sinaloa", "Nayarit", "Jalisco"))
  coast.selecc <- stringi::stri_trans_general(str = ents.coast.selecc, id = "Latin-ASCII")
  
  googlesheets4::gs4_deauth()
  
  locs.pesca <- googlesheets4::read_sheet("1eJXjwlHqEzSAZaTmE_XyN93fu6EI0I8igRH4AdvXOl8") %>% 
    mutate(NOM_LOC= stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC)) %>% 
    mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT)) %>% 
    mutate(NOM_LOC_REV= stringi::stri_trans_general(str = NOM_LOC_REV, id = "Latin-ASCII"), NOM_LOC_REV = toupper(NOM_LOC_REV)) %>% 
    mutate(NOM_ENT_REV= stringi::stri_trans_general(str = NOM_ENT_REV, id = "Latin-ASCII"), NOM_ENT_REV = toupper(NOM_ENT_REV)) %>%
    distinct(NOM_LOC, NOM_ENT, NOM_LOC_REV, NOM_ENT_REV) %>% 
    keep_when(!is.na(NOM_LOC_REV)) %>% 
    dplyr::filter(NOM_ENT %in% coast.selecc)
  
  return(locs.pesca)
}
