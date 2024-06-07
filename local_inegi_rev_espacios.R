#' Obtener localidades INEGI
#'
#' @author Hem Nalini Morzaria Luna
#' @date Mar 2024
#' @description Obtener localidades del catalogo de INEGI usando el servicio JSON
#' @description https://www.inegi.org.mx/servicios/catalogounico.html

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

# paquetes por sesion
.packages = c("dplyr","jsonlite","tidyr","magrittr", "googlesheets4","readr","here")

# instala paquetes si no estan instalados
.inst <- .packages %in% installed.packages()
if(length(.packages[!.inst]) > 0) install.packages(.packages[!.inst])

# Load packages into session
lapply(.packages, require, character.only=TRUE)

#especificar directorio donde se guardo el archivo de codigos por entidad
ent.dat <- readr::read_csv(here::here("edos_gcyuc.csv")) %>%
  dplyr::mutate(NOM_ENT = toupper(NOM_ENT)) %>%
  dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>%
  dplyr::rename(cve_ent = CVE_ENT)

#leer googlesheet directamente
googlesheets4::gs4_deauth()
locs.pesca <- googlesheets4::read_sheet("1Nww_0aSf2yQv9YqK8LqmJmIM2wwpHZCSpG1NcGfIfHQ")

#usar nombres revisados para busqueda
locs.pesca.faltantes <- locs.pesca %>%
  dplyr::mutate(NOM_LOC_ORIGINAL = dplyr::if_else(is.na(NOM_LOC_REVISADO), NOM_LOC_ORIGINAL, NOM_LOC_REVISADO)) %>%
  dplyr::mutate(NOM_ENT = stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII")) %>%
  dplyr::mutate(NOM_LOC_ORIGINAL = stringi::stri_trans_general(str = NOM_LOC_ORIGINAL, id = "Latin-ASCII")) %>%
  dplyr::distinct(NOM_ENT, NOM_LOC_ORIGINAL)

no.locs <- 1:nrow(locs.pesca.faltantes)

#funcion de busqueda
local_inegi <- function(locnum, locs.pesca.faltantes, ent.dat) {

  this.data <- locs.pesca.faltantes[locnum,]

#extraer nombre localidad a buscar
this.loc <- this.data %>%
  dplyr::mutate(NOM_LOC_ORIGINAL=gsub(" ", "%20", NOM_LOC_ORIGINAL)) %>%
  dplyr::pull(NOM_LOC_ORIGINAL)

this.ent <- this.data %>%
  dplyr::left_join(ent.dat, by = "NOM_ENT") %>%
  dplyr::pull(cve_ent)

  Sys.sleep(0.45)

  #buscar localidad
  this.url <- paste0("https://gaia.inegi.org.mx/wscatgeo/localidades/buscar/",this.loc)
  this.json.data <- jsonlite::fromJSON(this.url)

  #si la localidad no existe en la base imprimir error
  if("mensaje" %in% names(this.json.data)){

    print(this.json.data$mensaje)
  }

  #si la localidad existe en la base, revisar que corresponda a la entidad
  if("datos" %in% names(this.json.data)){

    data.locs <- this.json.data$datos %>%
      dplyr::select(cvegeo, cve_agee, cve_agem,cve_loc, nom_loc, latitud, longitud) %>%
      dplyr::mutate(cve_ent = as.character(cve_agee), no_ent = nchar(cve_ent)) %>%
      dplyr::mutate(cve_ent = dplyr::if_else(no_ent==1, paste0("0",cve_ent), cve_ent)) %>%
      dplyr::mutate(NOM_LOC_ORIGINAL = this.loc) %>%
      dplyr::filter(cve_ent == this.ent) %>%
      dplyr::left_join(ent.dat, by = "cve_ent")

    return(data.locs)

  }

}

#revisa todas las localidades de la hoja de Google Sheets que no tienen nombre revisado y filtra que esten en el mismo estado


data.locs <- lapply(no.locs, local_inegi, locs.pesca.faltantes, ent.dat)
#data.locs <- parallel::mclapply(no.locs, local_inegi, locs.pesca.faltantes, ent.dat)


locs.encontradas <- data.locs %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(CVE_LOC= cvegeo, CVE_MUN = paste0(cve_ent, cve_agem)) %>%
  dplyr::select(NOM_ENT, NOM_LOC_ORIGINAL,nom_loc, CVE_LOC, CVE_MUN, latitud, longitud)

readr::write_csv(locs.encontradas, here::here("outputs","localidades_pesqueras_encontradas.csv"))
