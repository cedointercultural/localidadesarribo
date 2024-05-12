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
get_fishery_office_catch <- function(this.catchfileno, catch.files, coast.selecc, inegigc, locs.pesca){

  correct.ent <- locs.pesca %>%
    select(-NOM_LOC_REV)
  
  correct.loc <- locs.pesca %>%
    select(-NOM_ENT_REV)
  
  this.catchfile <- catch.files[this.catchfileno]
  print(this.catchfile)
  
  if(grepl("xlsx", this.catchfile)){

    catch.file <- readxl::read_xlsx(this.catchfile)

  } else if(grepl("csv", this.catchfile)){

  catch.file <- data.table::fread(this.catchfile, skip=2)
  }

 
  if("NOMBREOFICINA" %in% colnames(catch.file)){

    catch.file.oficina <- catch.file %>%
     keep_when(NOMBREESTADO %in% coast.selecc) %>%
     dplyr::rename(NOM_LOC = `NOMBREOFICINA`,  NOM_ENT = `NOMBREESTADO`) %>% 
      distinct(NOM_LOC, NOM_ENT) 
  
  }
  
  if("NOMBRE OFICINA" %in% colnames(catch.file)){
    
    catch.file.oficina <- catch.file %>%
      keep_when(`NOMBRE ESTADO` %in% coast.selecc) %>%
      dplyr::rename(NOM_LOC = `NOMBRE OFICINA`, NOM_ENT = `NOMBRE ESTADO`) %>% 
      distinct(NOM_LOC, NOM_ENT) 
      
  }
  
  if("NOMBRE OFICINA \r\nDE PESCA" %in% colnames(catch.file)){
    
    catch.file.oficina <- catch.file %>%
      keep_when(`ENTIDAD` %in% coast.selecc) %>% 
      dplyr::rename(NOM_LOC = `NOMBRE OFICINA \r\nDE PESCA`, NOM_ENT = `ENTIDAD`) %>% 
      distinct(NOM_LOC, NOM_ENT) 
  }
  
  fish.offices <- catch.file.oficina %>%
    dplyr::mutate(NOM_ENT = toupper(stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"))) %>%
    dplyr::mutate(NOM_LOC = toupper(stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"))) %>%
    mutate(NOM_LOC= gsub("Ã‘", "N", NOM_LOC), NOM_LOC= gsub("A'", "N", NOM_LOC)) %>% 
    dplyr::left_join(correct.ent, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_LOC = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, NOM_LOC)) %>%
    select(-NOM_LOC_REV, -NOM_ENT_REV) %>% 
    dplyr::left_join(inegigc, by = c("NOM_LOC", "NOM_ENT"))
  
  print(fish.offices %>% keep_when(is.na(CVE_ENT)))

  return(fish.offices)
}
