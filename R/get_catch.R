#' Get and stadarize catch records
#'
#' @param this.catchfileno 
#' @param catch.files 
#' @param coast.selecc 
#' @param locs.pesca 
#'
#' @return this.permit.corr
#' @export
#'
#' @examples
get_catch <- function(this.catchfileno, catch.files, coast.selecc, locs.pesca){
  
  this.catchfile <- catch.files[this.catchfileno]
  print(this.catchfileno)
  print(this.catchfile)
  
  correct.ent <- locs.pesca %>%
    select(-NOM_LOC_REV)
  
  correct.loc <- locs.pesca %>%
    select(-NOM_ENT_REV) 
  
  if(grepl("xlsx", this.catchfile)){
    
    these.sheets <- readxl::excel_sheets(this.catchfile)
    print(these.sheets)
    
    if(length(these.sheets)>1){
      
      catch.sm.sheets <- grep("MEN", these.sheets, value = TRUE)
      
      catch.this.file <- list()
      
      for(i in 1:length(catch.sm.sheets)){
        
        this.excel.catch <- readxl::read_xlsx(this.catchfile, sheet=catch.sm.sheets[i])
        
        if("ZONA" %in% colnames(this.excel.catch)){
          
          this.excel.catch <- this.excel.catch %>% 
            select(-ZONA)
          
        } 
        
        print(colnames(this.excel.catch))
        
        catch.this.file[[i]] <- this.excel.catch
      }
      
      catch.file <- do.call(rbind, catch.this.file)
      
         
    } else {
      
      
      if(grepl("2023", this.catchfile)){
        #files from 2018-2023 start in row 3
        catch.file <- readxl::read_xlsx(this.catchfile, skip = 2)

      } else {
        
        catch.file <- readxl::read_xlsx(this.catchfile)
        
        
      }
    }
  
  } else if(grepl("csv", this.catchfile)){
    
    
    readr::read_csv(file="x\næøå", locale = readr::locale(encoding = "UTF-8"))
    #catch.file <- data.table::fread(this.catchfile, skip=2)
    
  catch.file <- readr::read_csv(this.catchfile, skip=2,  locale = readr::locale(encoding = "UTF-8"))
  }
  
  
  if("NOMBRESITIODESEMBARQUE" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
     keep_when(TIPOAVISO=="MENORES") %>% 
    dplyr::rename(NOM_ENT= NOMBREESTADO, NOM_LOC= NOMBRESITIODESEMBARQUE, rnp_code = RNPAUNIDADECONOMICA, fishery_office = NOMBREOFICINA,
                  species= `NOMBREPRINCIPAL`, landed_w_kg= `PESODESEMBARCADO`, value_mxn= VALOR, year = ANIOCORTE, month=MESCORTE)
      
  }
  
  if("AÑO CORTE" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `NOMBRE ESTADO`, NOM_LOC= `NOMBRE SITIO DESEMBARQUE`, rnp_code = `RNPA UNIDAD ECONOMICA`, fishery_office = `NOMBRE OFICINA`,
                    species= `NOMBRE PRINCIPAL`, landed_w_kg= `PESO DESEMBARCADO_KILOGRAMOS`, value_mxn= VALOR_PESOS, year = `AÑO CORTE`, month=`MES CORTE`) %>%
      keep_when(`TIPO AVISO`=="MENORES")
    
  }
  
  if("ANO CORTE" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `NOMBRE ESTADO`, NOM_LOC= `NOMBRE SITIO DESEMBARQUE`, rnp_code = `RNPA UNIDAD ECONOMICA`, fishery_office = `NOMBRE OFICINA`,
                    species= `NOMBRE PRINCIPAL`, landed_w_kg= `PESO DESEMBARCADO_KILOGRAMOS`, value_mxn= VALOR_PESOS, year = `ANO CORTE`, month=`MES CORTE`) %>%
      keep_when(`TIPO AVISO`=="MENORES")
    
  }
   
  if("AO CORTE" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      dplyr::rename(NOM_ENT= `NOMBRE ESTADO`, NOM_LOC= `NOMBRE SITIO DESEMBARQUE`, rnp_code = `RNPA UNIDAD ECONOMICA`, fishery_office = `NOMBRE OFICINA`,
                    species= `NOMBRE PRINCIPAL`, landed_w_kg= `PESO DESEMBARCADO_KILOGRAMOS`, value_mxn= VALOR_PESOS, year = `AO CORTE`, month=`MES CORTE`) %>%
      keep_when(`TIPO AVISO`=="MENORES")
    
  }
  
  if("PUERTO DE ARRIBO" %in% colnames(catch.file)){
    
    catch.file.small <- catch.file %>%
      mutate(value_mxn = `PRECIO\r\nCAPTURA` * `PESO DESEMBARCADO\r\n (Kg)`) %>% 
      dplyr::rename(NOM_ENT= `ENTIDAD`, NOM_LOC= `PUERTO DE ARRIBO`, rnp_code = `RNPA UNIDAD ECONOMICA`, fishery_office = `NOMBRE OFICINA \r\nDE PESCA`,
                    species= `ESPECIE\r\n(NOMBRE COMÚN)`, landed_w_kg= `PESO DESEMBARCADO\r\n (Kg)`, year = `AÑO\r\n CORTE`, month = `MES\r\n CORTE`)
  }
  
  # problem.line<- catch.file.small[210562,]
  # 
  # if(utf8::utf8_valid(problem.line$NOM_LOC)==FALSE){
  #   
  #   problem.line <- catch.file.small[210561,]
  #    
  #   catch.file.small[210561,]$NOM_LOC <- "CAMPO GUEMEZ"
  # }
  # 
  #  
  rnp.uncoded <- catch.file.small %>%
    mutate(NOM_LOC= gsub("Ã‘", "N", NOM_LOC),
           NOM_LOC= gsub("A'", "N", NOM_LOC),
           NOM_LOC= gsub("\\?", "N", NOM_LOC),
           fishery_office = gsub("Ã‘", "N", fishery_office), 
           species = gsub("Ã‘", "N", species)) %>% 
    mutate(fishery_office= stringi::stri_trans_general(str = fishery_office, id = "Latin-ASCII"), fishery_office = toupper(fishery_office)) %>% 
    mutate(NOM_LOC= stringi::stri_trans_general(str = NOM_LOC, id = "Latin-ASCII"), NOM_LOC = toupper(NOM_LOC)) %>% 
    mutate(NOM_ENT= stringi::stri_trans_general(str = NOM_ENT, id = "Latin-ASCII"), NOM_ENT = toupper(NOM_ENT)) %>% 
    dplyr::select(rnp_code, NOM_ENT, NOM_LOC, fishery_office, species, landed_w_kg, value_mxn, year, month)
  
  this.permit.corr <- rnp.uncoded %>%
    dplyr::left_join(correct.ent, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_ENT = dplyr::if_else(!is.na(NOM_ENT_REV), NOM_ENT_REV, NOM_ENT)) %>%
    dplyr::left_join(correct.loc, by =c("NOM_LOC","NOM_ENT")) %>%
    mutate(NOM_LOC = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, NOM_LOC)) %>% 
    select(-NOM_LOC_REV, -NOM_ENT_REV) 

  correct.loc.fishery <- locs.pesca %>%
    select(NOM_LOC_REV, NOM_LOC, NOM_ENT) %>% 
    rename(fishery_office = NOM_LOC)

  #eliminate records from freshwater fisheries    
  
   
  fishery.catch.locs <- this.permit.corr %>% 
    dplyr::left_join(correct.loc.fishery, by =c("fishery_office","NOM_ENT")) %>%
    mutate(fishery_office = dplyr::if_else(!is.na(NOM_LOC_REV), NOM_LOC_REV, fishery_office)) %>% 
    select(-NOM_LOC_REV) %>% 
    keep_when(NOM_ENT %in% coast.selecc) %>% 
    keep_when(NOM_LOC!="NO CONSIDERADO") %>%
    keep_when(!grepl("(CULT)",NOM_LOC)) %>%
    keep_when(!grepl("(CULT\\.)",NOM_LOC)) %>% 
    keep_when(!grepl("PRESA ",NOM_LOC)) %>%
    keep_when(!grepl("LAGO ",NOM_LOC)) %>% 
    keep_when(!grepl("RIO ",NOM_LOC)) %>% 
    keep_when(!grepl("ESTANQUE ",NOM_LOC)) %>% 
    keep_when(!grepl("ARROYO ",NOM_LOC)) 
    
  
  data.table::fwrite(fishery.catch.locs, here::here("data-raw","fisheries_data",paste0("catch_file_",this.catchfileno,".csv")))
  
 
}