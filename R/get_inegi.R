get_inegi <- function(coast.selecc){

comunidadesgc <- data.table::fread(paste0("~/coastalvulnerability-inputs/datos_INEGI_2020/conjunto_de_datos/","conjunto_de_datos_iter_00_cpv2020.csv"))
  
comunidadesgc.data <- comunidadesgc %>%
  dplyr::rename(CVE_ENT=ENTIDAD, CVE_MUN=MUN, CVE_LOC=LOC) %>% 
  dplyr::select(NOM_ENT, NOM_MUN, NOM_LOC, CVE_ENT, CVE_MUN, CVE_LOC, POBTOT, POBFEM, POBMAS, LONGITUD, LATITUD) %>% 
  dplyr::mutate(across(POBTOT:POBMAS, as.numeric)) %>%
  make_code() %>% 
  dplyr::filter(NOM_ENT %in% coast.selecc)
  
#convert from lat long to decimal degrees
inegigc <- comunidadesgc.data %>%
  tidyr::separate(LONGITUD,c("lon_deg","lon"),"°") %>%
  tidyr::separate(lon,c("lon_min","lon_sec"),"'") %>%
  dplyr::mutate(lon_sec = gsub("\".*\ W","",lon_sec)) %>%
  tidyr::separate(LATITUD,c("lat_deg","lat"),"°") %>%
  tidyr::separate(lat,c("lat_min","lat_sec"),"'") %>%
  dplyr::mutate(lat_sec = gsub("\".*\ N","",lat_sec)) %>%
  dplyr::mutate(across(lon_deg:lat_sec,as.numeric)) %>%
  dplyr::mutate(dec_lon= (lon_deg + (lon_min / 60) + (lon_sec / 3600))*-1,
                dec_lat=(lat_deg + (lat_min / 60) + (lat_sec / 3600))) %>%
  dplyr::mutate(deci_lon = dec_lon, deci_lat = dec_lat) %>% 
  dplyr::filter(!is.na(deci_lat)) %>% 
  dplyr::select(NOM_ENT, NOM_MUN, NOM_LOC, CVE_ENT, CVE_MUN, CVE_LOC, POBTOT, POBFEM, POBMAS, deci_lon, deci_lat)
  

usethis::use_data(inegigc, overwrite = TRUE)
#creates template describing the data
data.description <-sinew::makeOxygen(inegigc)
cat(data.description, file=here::here("R",paste0("data-","inegigc.R")))

}