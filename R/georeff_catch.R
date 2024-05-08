georref_catch <- function(this.catchfileno, catch.files, coast.selecc, permit.locs.geo){
  
  this.catchfile <- catch.files[this.catchfileno]
  print(this.catchfile)
  
  if(grepl("xlsx", this.catchfile)){
    
    catch.file <- readxl::read_xlsx(this.catchfile)
    
  } else if(grepl("csv", this.catchfile)){
    
    catch.file <- data.table::fread(this.catchfile, skip =2)
  }
  
  
  if("TIPOAVISO" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= NOMBREESTADO, NOM_LOC= NOMBRESITIODESEMBARQUE, rnp_code = RNPAUNIDADECONOMICA) %>%
      keep_when(TIPOAVISO=="MENORES") %>% 
      keep_when(NOM_ENT %in% coast.selecc)
    
    catch.file.rnp <- catch.file.small %>%
      dplyr::left_join(permit.locs.geo, by = c("rnp_code","NOM_LOC","NOM_ENT"))
  }
  
  
  catch.file.rnp <- catch.file %>%
    dplyr::filter(`NOMBRE ESTADO` %in% coast.selecc) %>%
    dplyr::group_by(`RNPA UNIDAD ECONOMICA`,`NOMBRE PRINCIPAL`,`NOMBRE ESPECIE`,`AÑO CORTE`) %>%
    dplyr::summarise(tot_weight_kg=sum(`PESO DESEMBARCADO_KILOGRAMOS`), tot_value_pesos=sum(VALOR_PESOS)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rnp_code=`RNPA UNIDAD ECONOMICA`, catch_name = `NOMBRE PRINCIPAL`, sp_name = `NOMBRE ESPECIE`, year = `AÑO CORTE`)
  
  catch.file.rnp <- catch.file %>%
    dplyr::filter(`NOMBRE ESTADO` %in% coast.selecc) %>%
    dplyr::group_by(`RNPA UNIDAD ECONOMICA`,`NOMBRE PRINCIPAL`,`NOMBRE ESPECIE`,`AÑO CORTE`) %>%
    dplyr::summarise(tot_weight_kg=sum(`PESO DESEMBARCADO_KILOGRAMOS`), tot_value_pesos=sum(VALOR_PESOS)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(rnp_code=`RNPA UNIDAD ECONOMICA`, catch_name = `NOMBRE PRINCIPAL`, sp_name = `NOMBRE ESPECIE`, year = `AÑO CORTE`)
  
  if(this.catchfileno==1){
    data.table::fwrite(catch.file.rnp, here::here("outputs","catch_files","sonora_conapesca_2000-2023"), append = FALSE)
    
  } else if(this.catchfileno>1){
    data.table::fwrite(catch.file.rnp, here::here("outputs","catch_files","sonora_conapesca_2000-2023"), append = TRUE)
  }
  
  
  return(catch.file.rnp)
  
}