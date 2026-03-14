#' Obtener localidades Google Map
#'
#' @author Hem Nalini Morzaria Luna
#' @date Apr 2024
#' @description Obtener localidades utilizando el API de Google Maps
#' @description https://guides.library.duke.edu/r-geospatial/geocode

Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/

# paquetes por sesion
.packages = c("dplyr","jsonlite","tidyr","magrittr", "googlesheets4","readr","here","ggmap", "mapview")

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


ggmap::register_google(key = "AIzaSyC8Rmok0CK4pqJ_pkZGKGfKBcD7JaqG9qM", write = TRUE)

#Use mutate_geocode() to create a new data frame with added columns for latitude and longitude coordinates (output = "latlon" argument). output = "latlona" returns latitude, longitude, and the matched address (to cross-check against your address) as new columns.

locs.data <- locs.pesca.faltantes %>% 
  mutate(loc = paste0(NOM_LOC_ORIGINAL, ", ", "MEXICO"))

geocode_googlemaps <- function(locs.data){
  
  addr.geo <- mutate_geocode(locs.data, location = loc, output = "latlona")

  return(addr.geo)
  
}

geo.locs<- geocode_googlemaps(locs.data)

View(geo.locs)


readr::write_csv(geo.locs, here::here("localidades_google_map.csv"))
