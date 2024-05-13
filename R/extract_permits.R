#' Extract permit information
#'
#' @param thispermitfile.no 
#' @param permit.files 
#' @param inegi.cost.comm 
#' @param coast.selecc 
#' @param locs.pesca 
#' @param locs.inegi.1 
#' @param locs.inegi.2 
#' @param locs.google 
#'
#' @return this.permit.inegi
#' @export
#'
#' @examples
#' @description
#' Extract permit with location information to match with catch records
#' 
extract_permits <- function(thispermitfile.no, permit.files, inegi.cost.comm.all, 
                            coast.selecc, locs.pesca, georef.locs.km){

  this.permitfile <- permit.files[thispermitfile.no]

  print(paste("file", this.permitfile))

  this.permit.data <- readxl::read_xlsx(this.permitfile)

  names.data <- colnames(this.permit.data)
  print(names.data)

  correct.ent <- locs.pesca %>%
    select(-NOM_LOC_REV)

  correct.loc <- locs.pesca %>%
    select(-NOM_ENT_REV) 

  if("CLAVE DE RNP" %in% names.data) {

    this.permit.data <- this.permit.data %>%
    dplyr::rename(rnp_code =`CLAVE DE RNP`) 
  }


  if("RNPA" %in% names.data){
    this.permit.data <- this.permit.data %>%
    dplyr::rename(rnp_code =`RNPA`)
  }

  if("rnpa" %in% names.data){
    this.permit.data <- this.permit.data %>%
      dplyr::rename(rnp_code =`rnpa`) %>% 
      keep_when(tipo_embarcacion=="Menor")
    
  }
  
  
  if("RNP_TITULAR" %in% names.data) {

    this.permit.data <- this.permit.data %>%
      dplyr::rename(rnp_code = RNP_TITULAR) %>% 
      keep_when(TIPO_EMBARCACION!="MAYOR")
  }

  if("RNPA ACTIVO" %in% names.data) {

    this.permit.data <- this.permit.data %>%
      dplyr::rename(rnp_code =`RNPA ACTIVO`)

  }

  if("ENTIDAD" %in% names.data){

    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_ENT=ENTIDAD)
  }

  if("ESTADO" %in% names.data){

    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_ENT=ESTADO)
  }

  if("entidad" %in% names.data){
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_ENT=entidad)
  }
  
  
  if("NOMBRE ESTADO" %in% names.data){

    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_ENT=`NOMBRE ESTADO`)
  }

  if("LOCALIDAD" %in% names.data){

    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_LOC=LOCALIDAD)
  }

  if("localidad" %in% names.data){
    
    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_LOC=localidad)
  }
  

  if("NOMBRE OFICINA" %in% names.data){

    this.permit.data <- this.permit.data %>%
      dplyr::rename(NOM_LOC=`NOMBRE OFICINA`)
  }
  
  if("TIPO DE TRAMITE" %in% names.data) {
    
    this.permit.data <- this.permit.data %>%
      keep_when(`TIPO DE TRAMITE`!= "FOMENTO")
  }

  if(class(this.permit.data$rnp_code)=="numeric" | class(this.permit.data$rnp_code)=="double"){
    
    this.permit.data <- this.permit.data %>%
      mutate(rnp_code=as.character(rnp_code))
  }
  
  this.permit.loc <- this.permit.data %>%
    dplyr::mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT), NOM_ENT = stringr::str_squish(NOM_ENT)) %>%
    dplyr::mutate(NOM_LOC = stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC), NOM_LOC = stringr::str_squish(NOM_LOC)) %>%
    dplyr::mutate(NOM_LOC=gsub("A'", "N", NOM_LOC)) %>% 
    dplyr::filter(NOM_ENT %in% coast.selecc) %>%
    keep_when(!is.na(NOM_LOC)) %>%
    distinct(rnp_code, NOM_ENT, NOM_LOC)
    

  this.permit.corr <- this.permit.loc %>%
     dplyr::left_join(correct.ent, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_LOC = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, NOM_LOC)) %>% 
    select(-NOM_LOC_REV, -NOM_ENT_REV)

  this.permit.inegi.locs <- this.permit.corr %>% 
      dplyr::left_join(georef.locs.km, by = c("NOM_ENT", "NOM_LOC")) %>% 
     keep_when(!is.na(rnp_code) & !is.na(deci_lat) & !is.na(deci_lon)) %>% 
    distinct(rnp_code, NOM_LOC, NOM_ENT, deci_lat, deci_lon, fishery_office, fishery_office_ent, distance_km)
  
 # this.permit.inegi.locs %>% View()
  
  return(this.permit.inegi.locs)
}

