#' Extract RNPA codes from catch files
#'
#' @param this.catchfileno
#' @param catch.files catch files
#' @param coast.selecc selected coastal states
#' @param locs.pesca corrected location names
#' @param permit.locs.geo geolocated permits
#' @param inegi.cost.comm.all INEGI coastal communities
#'
#' @return catch.file.rnp
#' @export
#'
#' @examples
get_rnpa_catch <- function(this.catchfileno, catch.files, coast.selecc, locs.pesca, permit.locs.geo, inegi.cost.comm.all, georef.locs.gc){

  this.catchfile <- catch.files[this.catchfileno]
  print(this.catchfile)
  
  rnp.codes <- unique(permit.locs.geo$rnp_code)
  
  correct.ent <- locs.pesca %>%
    select(-NOM_LOC_REV)
  
  correct.loc <- locs.pesca %>%
    select(-NOM_ENT_REV) 
  
   if(grepl("xlsx", this.catchfile)){

    catch.file <- readxl::read_xlsx(this.catchfile)

  } else if(grepl("csv", this.catchfile)){

  catch.file <- data.table::fread(this.catchfile, skip=2)
  }

 
  if("TIPOAVISO" %in% colnames(catch.file)){

    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= NOMBREESTADO, NOM_LOC= NOMBRESITIODESEMBARQUE, rnp_code = RNPAUNIDADECONOMICA) %>%
      keep_when(TIPOAVISO=="MENORES") %>% 
      keep_when(NOM_ENT %in% coast.selecc) 
  }
  
  if("TIPO AVISO" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `NOMBRE ESTADO`, NOM_LOC= `NOMBRE SITIO DESEMBARQUE`, rnp_code = `RNPA UNIDAD ECONOMICA`) %>%
      keep_when(`TIPO AVISO`=="MENORES") %>% 
      keep_when(NOM_ENT %in% coast.selecc)
    
  }
  
  if("PUERTO DE ARRIBO" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `ENTIDAD`, NOM_LOC= `PUERTO DE ARRIBO`, rnp_code = `RNPA UNIDAD ECONOMICA`) %>%
      keep_when(NOM_ENT %in% coast.selecc)
    
  }
  
  
  rnp.uncoded <- catch.file.small %>%
    mutate(rnp_code = as.character(rnp_code)) %>%
    dplyr::filter(!rnp_code %in% rnp.codes) %>% 
    dplyr::select(NOM_ENT, NOM_LOC, rnp_code) %>%
    keep_when(rnp_code !="9999999999", NOM_LOC !="NO CONSIDERADO") %>% 
    distinct(NOM_ENT, NOM_LOC, rnp_code) %>% 
    mutate(NOM_LOC= gsub("Ã‘", "N", NOM_LOC))
  
  print(colnames(rnp.uncoded))
  
  this.permit.corr <- rnp.uncoded %>%
    dplyr::left_join(correct.ent, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_LOC = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, NOM_LOC)) %>% 
    select(-NOM_LOC_REV, -NOM_ENT_REV)
  
  this.permit.inegi <- this.permit.corr %>% 
    dplyr::left_join(inegi.cost.comm.all, by =c("NOM_ENT","NOM_LOC")) %>% 
    select(rnp_code, NOM_ENT, NOM_MUN, NOM_LOC, CVE_LOC, CVE_MUN, CVE_ENT, deci_lat, deci_lon)
  
  this.permit.inegi.locs <- this.permit.inegi %>%
    keep_when(is.na(deci_lat)) %>% 
    select(-deci_lat, -deci_lon) %>% 
    dplyr::left_join(georef.locs.gc, by = c("NOM_ENT", "NOM_LOC")) %>% 
    dplyr::rename(deci_lat = dec_lat_google, deci_lon = dec_lon_google) %>%
    keep_when(!is.na(deci_lat)) %>% 
    select(rnp_code, NOM_ENT, NOM_LOC, deci_lat, deci_lon)
  
  permit.inegi.locs.geo <- this.permit.inegi %>%
    keep_when(!is.na(deci_lat))  %>% 
    select(rnp_code, NOM_ENT, NOM_LOC, deci_lat, deci_lon) %>% 
    dplyr::bind_rows(this.permit.inegi.locs) %>% 
    distinct(rnp_code, NOM_ENT, NOM_LOC, deci_lat, deci_lon)
  
  return(permit.inegi.locs.geo)
}
