#' @description
#' Codigo para extraer localidades de datos de captura CONAPESCA
#' @author Hem Nalini Morzaria Luna, Rebeca Navarrete Torices
#' @date Febrero 2024

# fijar codificación
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")# https://www.r-bloggers.com/web-scraping-and-invalid-multibyte-string/
#Instalar estas bibliotecas para el paquete xlsx
#sudo apt-get install default-jdk default-jre libbz2-dev

# Instalar paquetes necesario, los instala de ser necesario
.packages = c("data.table","tidyverse","here", "readr","sf","readxl")
#install.packages(.packages, dependencies = TRUE)

# Load packages into session 
lapply(.packages, require, character.only=TRUE)

#para montar disco con datos CONAPESCA
system("gcsfuse cedo_model_data $HOME/mount-folder", wait = TRUE)

#F i l t r a r AVISOS_MAYORES_MENORES_COSECHA_2018 - 2022

#funcion para sacar cosecha del Pacifico, estados del Golfo de California y extraer las localidades
#solo para 2018-2022, estos archivos tienen la misma estructura
#el proposito de una funcion es no tener que repetir el codigo, solo tener que pasarle nuevas variables

#requiere el año, los estados del pacifico y el nombre del archivo 

avisos_cosecha <- function(estearchivo, estados.pacifico){ #argumentos que pasan a la funcion
  
  archivo.cosecha <- paste0("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_", estearchivo,".csv")

  avisos.cosecha <- data.table::fread(archivo.cosecha, skip = 2) %>% 
    dplyr::mutate(indice = 1:nrow(.))
  
  head(avisos.cosecha) #ver nombres de columnas
  
  #filtrar estados del pacifico
  sitios.pacifico <- subset(avisos.cosecha,`NOMBRE ESTADO` %in% estados.pacifico)
  
  readr::write_csv(sitios.pacifico, paste0("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_",estearchivo,".csv")) #crea tablas captura_pacifico_año
  
  loc.cosecha <- sitios.pacifico %>% 
    dplyr::select(`CLAVE SITIO DESEMBARQUE`,`NOMBRE SITIO DESEMBARQUE`, `NOMBRE ESTADO`,`NOMBRE OFICINA`) %>% 
    dplyr::rename(no_sitio = `CLAVE SITIO DESEMBARQUE`, nombre_sitio = "NOMBRE SITIO DESEMBARQUE", entidad = "NOMBRE ESTADO", oficina = "NOMBRE OFICINA") %>% #cambiar nombre variables
    dplyr::distinct(no_sitio, nombre_sitio, entidad, oficina) %>% # sitios unicos
    dplyr::filter(!nombre_sitio=="") %>%  #quitar filas sin nombre de sitio
    dplyr::mutate(yr = estearchivo)
  
  readr::write_csv(sitios.pacifico, paste0("/home/atlantis/mount-folder/CONAPESCA/nombres_localidades_",estearchivo,".csv")) #crea archivo con localidades
  
  
  print(paste(estearchivo,"completado")) #imprimir mensaje archivo analizado
}

estados.pacifico<-c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "SONORA", "SINALOA", "NAYARIT", "JALISCO")

#asi se aplicaria la funcion a un solo año
#
#estearchivo <- "2018"
#avisos_cosecha(estearchivo = "2018", estados.pacifico, )


#correr funcion, la function lapply aplica una funcion de manera secuencial a multiples variables

#todos los años
archivos.periodo <- c("2018", "2019", "2020","2021", "2022")

lapply(archivos.periodo, avisos_cosecha, estados.pacifico)




#lee el archivo CONAPESCA_2000-2016.xlsx y filtra las localidades correspondientes
# funcion mas general, requiere el nombre de cada archivo

archivo.datos <- "/home/atlantis/mount-folder/CONAPESCA/CONAPESCA_IFAI_2000-2016.xlsx"
#hojas en el archivo de datos con captura de pesquerias artesanales
hojas.datos <- excel_sheets(archivo.datos) %>% 
  grep("MENORES",., value = TRUE)

lapply(hojas.datos, avisos_cosecha_gen, archivo.datos, estados.pacifico)

avisos_cosecha_gen <- function(estahoja, archivo.datos, estados.pacifico){ #argumentos que pasan a la funcion
  
 avisos.cosecha <- readxl::read_excel(archivo.datos, sheet = estahoja) %>% 
      dplyr::mutate(indice = 1:nrow(.))
 
 #filtrar estados del pacifico
  sitios.pacifico <- subset(avisos.cosecha,`ENTIDAD` %in% estados.pacifico)
  
  
  readr::write_csv(sitios.pacifico, paste0("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_",estearchivo,".csv")) #crea tablas captura_pacifico_año
  
  loc.cosecha <- sitios.pacifico %>% 
    dplyr::select(`CLAVE SITIO DESEMBARQUE`,`NOMBRE SITIO DESEMBARQUE`, `NOMBRE ESTADO`,`NOMBRE OFICINA`) %>% 
    dplyr::rename(no_sitio = `CLAVE SITIO DESEMBARQUE`, nombre_sitio = "NOMBRE SITIO DESEMBARQUE", entidad = "NOMBRE ESTADO", oficina = "NOMBRE OFICINA") %>% #cambiar nombre variables
    dplyr::distinct(no_sitio, nombre_sitio, entidad, oficina) %>% # sitios unicos
    dplyr::filter(!nombre_sitio=="") %>%  #quitar filas sin nombre de sitio
    dplyr::mutate(yr = estearchivo)
  
  readr::write_csv(sitios.pacifico, paste0("/home/atlantis/mount-folder/CONAPESCA/nombres_localidades_",estearchivo,".csv")) #crea archivo con localidades
  
  
  print(paste(estearchivo,"completado")) #imprimir mensaje archivo analizado
}



estearchivo <- "/home/atlantis/mount-folder/CONAPESCA/CONAPESCA_IFAI_2000-2016.xlsx"





estearchivo2000 = "2000-2016"
  avisos_cosecha <- function(estearchivo2000, estados.pacifico) #argumentos que pasan a la funcion
    archivo_path <- paste0("/home/atlantis/mount-folder/CONAPESCA/CONAPESCA_IFAI_2000-2016",  ".xlsx")  
  avisos.cosecha2000 <- readxl::read_excel(archivo_path)
  
   file.exists(archivo_path)
  
    #fread es mas eficiente para lectura de archivos grandes
    avisos.cosecha2000 <- readxl::read_excel(paste0("/home/atlantis/mount-folder/CONAPESCA/CONAPESCA_IFAI_2000-2016",".xlsx"))
  
  head(avisos.cosecha2000) #ver nombres de columnas
  
  #sitios_pacifico filtro Edos Pacifico
  sitios.pacifico2000 <-subset(avisos.cosecha2000,`ENTIDAD` %in% estados.pacifico)
  
  readr::write_csv(sitios.pacifico2000, paste0("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_",estearchivo2000,".csv"))
  
  print(paste(estearchivo2000,"completado")) #imprimir mensaje archivo analizado
  
  avisos2000.loc <- fread("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_2000-2016.csv") %>% 
    dplyr::distinct(`ENTIDAD`,`PUERTO DE ARRIBO`) %>% #sitios de arribo unicos
    dplyr::mutate(sitio_arribo = tolower(`PUERTO DE ARRIBO`)) %>% #sitios en minuscula
    dplyr::mutate(entidad = tolower(`ENTIDAD`)) #estados en minuscula 
  

  
#une las listas de localidades 2000-2022 y asegurate otra vez que son los sitios unicos 
archivos.cosecha.yrs2000_2017 <- c("2017","2000-2016")

archivos_combinados <- c(archivos.cosecha.yrs, archivos.cosecha.yrs2000_2017)
print(archivos_combinados)

#leer shapefile con sitios de arribo atlas localidades pesqueras

atlas.localidades <- sf::st_read("/home/atlantis/mount-folder/CONAPESCA/atlas_localidades_pesqueras.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::distinct(ESTADO, LOCALIDAD) %>% 
  subset(ESTADO %in% estados.pacifico)
  
