#' Extract RNPA codes from catch files
#'
#' @param this.catchfileno
#' @param catch.files catch files
#' @param coast.selecc selected coastal states
#' @param locs.pesca corrected location names
#' @param permit.locs.geo geolocated permits
#' @param georef.locs.km georreferenced locations
#' @param fishery.office.table INEGI coastal communities
#'
#' @return catch.file.rnp
#' @export
#'
#' @examples
get_rnpa_catch <- function(this.catchfileno, catch.files, coast.selecc, locs.pesca, permit.locs.geo, georef.locs.km, fishery.office.table){

  this.catchfile <- catch.files[this.catchfileno]
  print(this.catchfile)
  
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
      dplyr::rename(NOM_ENT= NOMBREESTADO, NOM_LOC= NOMBRESITIODESEMBARQUE, rnp_code = RNPAUNIDADECONOMICA, fishery_office = NOMBREOFICINA) %>%
      keep_when(TIPOAVISO=="MENORES") %>% 
      keep_when(NOM_ENT %in% coast.selecc) 
  }
  
  if("TIPO AVISO" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `NOMBRE ESTADO`, NOM_LOC= `NOMBRE SITIO DESEMBARQUE`, rnp_code = `RNPA UNIDAD ECONOMICA`, fishery_office = `NOMBRE OFICINA`) %>%
      keep_when(`TIPO AVISO`=="MENORES") %>% 
      keep_when(NOM_ENT %in% coast.selecc)
    
  }
  
  if("PUERTO DE ARRIBO" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `ENTIDAD`, NOM_LOC= `PUERTO DE ARRIBO`, rnp_code = `RNPA UNIDAD ECONOMICA`, fishery_office = `NOMBRE OFICINA \r\nDE PESCA`) %>%
      keep_when(NOM_ENT %in% coast.selecc)
    
  }
  

  if("NOMBRESITIODESEMBARQUE" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `NOMBREESTADO`, NOM_LOC= `NOMBRESITIODESEMBARQUE`, rnp_code = `RNPAUNIDADECONOMICA`, fishery_office = `NOMBREOFICINA`) %>%
      keep_when(NOM_ENT %in% coast.selecc)
    
  }
  
  rnp.uncoded <- catch.file.small %>%
    mutate(rnp_code = as.character(rnp_code)) %>%
    keep_when(rnp_code !="9999999999", NOM_LOC !="NO CONSIDERADO") %>% 
    mutate(NOM_LOC= gsub("Ã‘", "N", NOM_LOC), fishery_office = gsub("Ã‘", "N", fishery_office)) %>% 
    dplyr::distinct(rnp_code, NOM_ENT, NOM_LOC, fishery_office)
 
  print(colnames(rnp.uncoded))
  
  this.permit.corr <- rnp.uncoded %>%
    dplyr::left_join(correct.ent, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_LOC = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, NOM_LOC)) %>% 
    select(-NOM_LOC_REV, -NOM_ENT_REV)
  
   correct.ent.fish <- correct.ent %>% 
    dplyr::rename(fishery_office = NOM_LOC)
 
  correct.loc.fish <- correct.loc %>% 
    dplyr::rename(fishery_office = NOM_LOC)
  
  this.permit.corr.fish <- this.permit.corr %>%
    dplyr::left_join(correct.ent.fish, by = c("fishery_office","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc.fish, by = c("fishery_office","NOM_ENT")) %>%
    mutate(fishery_office = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, fishery_office)) %>% 
    select(-NOM_LOC_REV, -NOM_ENT_REV)
  
  
  this.permit.inegi.locs <- this.permit.corr.fish %>%
    dplyr::left_join(georef.locs.km, by = c("NOM_ENT", "NOM_LOC","fishery_office")) %>% 
    distinct(rnp_code, NOM_LOC, NOM_ENT, deci_lat, deci_lon, fishery_office, fishery_office_ent, distance_km)
  
  this.permit.inegi.locs.corr <- this.permit.inegi.locs %>% 
    keep_when(is.na(deci_lat)) %>% 
    dplyr::select(rnp_code, NOM_ENT, NOM_LOC, fishery_office) %>%
    dplyr::left_join(fishery.office.table, by = c("fishery_office")) %>%
    
  this.permit.geo <- this.permit.inegi.locs %>% 
    keep_when(!is.na(deci_lat)) 
  
  this.permit.geo.corr <- this.permit.geo %>% 
    dplyr::group_by(rnp_code,NOM_LOC, NOM_ENT, fishery_office, fishery_office_ent) %>%
    dplyr::summarize(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon)) %>% 
    dplyr::bind_rows(this.permit.inegi.locs.corr) %>% 
    distinct(rnp_code, NOM_ENT, NOM_LOC, deci_lat, deci_lon, fishery_office, fishery_office_ent) %>% 
    dplyr::group_by(rnp_code,NOM_LOC, NOM_ENT, fishery_office, fishery_office_ent) %>%
    dplyr::summarize(deci_lat = mean(deci_lat), deci_lon = mean(deci_lon))
  
  #ultimo paso ya que se elimine distancia
  
  rnp.codes.geo <- unique(this.permit.inegi.locs$rnp_code)
  
   permit.inegi.locs.geo <- permit.locs.geo %>%
    keep_when(!rnp_code %in% rnp.codes.geo)  %>% 
    select(rnp_code, NOM_ENT, NOM_LOC, deci_lat, deci_lon) %>% 
    dplyr::bind_rows(this.permit.inegi.locs) %>% 
    distinct(rnp_code, NOM_ENT, NOM_LOC, deci_lat, deci_lon)
  
  return(permit.inegi.locs.geo)
}
