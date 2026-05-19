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
    dplyr::mutate(CVE_LOC = as.character(CVE_LOC)) %>% 
    dplyr::select("Entidad Federativa","CVE_LOC","Oficina de Pesca") %>% 
   dplyr::rename(NOM_ENT="Entidad Federativa", fishery_office ="Oficina de Pesca") %>% 
   tidyr::unnest(CVE_LOC)  %>% 
    dplyr::mutate(NOM_ENT = toupper(NOM_ENT), fishery_office = toupper(fishery_office))  %>% 
    dplyr::mutate(fishery_office=stringi::stri_trans_general(str = fishery_office, id = "Latin-ASCII"))


  catchdata.rev <- catchdata %>%
    dplyr::mutate(fishery_office = toupper(as.character(fishery_office))) %>% 
     dplyr::mutate(NOM_ENT = dplyr::if_else(fishery_office == "GUERRERO NEGRO", "BAJA CALIFORNIA SUR",
                                           dplyr::if_else(fishery_office =="EJIDO VILLA JESUS MARIA", "BAJA CALIFORNIA",
                                                          dplyr::if_else(fishery_office=="SAN QUINTIN", "BAJA CALIFORNIA",
                                                                         dplyr::if_else(fishery_office=="BAHIA TORTUGAS", "BAJA CALIFORNIA SUR",
                                                                                        dplyr::if_else(fishery_office=="BAHIA DE LOS ANGELES", "BAJA CALIFORNIA",
                                                                                                       dplyr::if_else(fishery_office=="ENSENADA", "BAJA CALIFORNIA",
                                                                                                       dplyr::if_else(fishery_office=="LA PAZ", "BAJA CALIFORNIA SUR", NOM_ENT
                                                                                                                      
                                                                                                                      
                                                                                                       ))))))))
 
  catch.offices <- catchdata.rev %>% 
  dplyr::distinct(fishery_office, NOM_ENT)
    
   
  fish.offices.catch <- catch.offices %>% 
    dplyr::left_join(fish.offices.conapesca, by=c("fishery_office","NOM_ENT"))
  
  
  #fish.offices.catch %>% keep_when(is.na(`Oficina de Pesca`)) %>% View()  
    
  catchdata <- catchdata.rev
    
  #update Rdata
  usethis::use_data(catchdata, overwrite = TRUE)
  #creates template describing the data
  data.description <-sinew::makeOxygen(catchdata)
  cat(data.description, file=here::here("R",paste0("data-","catchdata")))
  
  #rebuild and load the package
  devtools::load_all() # restarts and loads
  
  return(fish.offices.catch)
}
