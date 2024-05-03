
library(tidyverse)
library(here)
library(data.table)
library(sf) # para localidades


#F i l t r a r AVISOS_MAYORES_MENORES_COSECHA_2018

avisos2018.cosecha <- fread("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2018.csv")
#cambio de titulos de columnas
avisos2018.cosecha<-dplyr::read_csv("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2018.csv",skip=2, header=TRUE)
head(avisos2018.cosecha) #ver nombres de titulos
#sitios_pacifico2018 filtro solo PACIFICO
sitios_pacifico2018<-subset(avisos2018.cosecha,LITORAL == "PACIFICO")
head(sitios_pacifico2018)
#sitios_pacifico2018 filtro Edos Pacifico y Jalisco
estados_pacifico<-c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "SONORA", "SINALOA", "NAYARIT", "JALISCO")
sitios_pacifico2018 <-subset(avisos2018.cosecha,NOMBRE.ESTADO %in% estados_pacifico)


#F i l t r a r AVISOS_MAYORES_MENORES_COSECHA_2019
avisos2019.cosecha <- fread("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2019.csv")
avisos2019.cosecha<-read.csv("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2019.csv",skip=2, header=TRUE)
#sitios_pacifico2019 filtro solo PACIFICO
sitios_pacifico2019<-subset(avisos2019.cosecha,LITORAL == "PACIFICO")
#sitios_pacifico2018 filtro Edos Pacifico y Jalisco
sitios_pacifico2019<-subset(avisos2019.cosecha,NOMBRE.ESTADO %in% estados_pacifico)

#F i l t r a r AVISOS_MAYORES_MENORES_COSECHA_2020
avisos2020.cosecha <- fread("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2020.csv")
avisos2020.cosecha<-read.csv("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2020.csv",skip=2, header=TRUE)
#sitios_pacifico2020 filtro solo PACIFICO
sitios_pacifico2020<-subset(avisos2020.cosecha,LITORAL == "PACIFICO")
#sitios_pacifico2020 filtro Edos Pacifico y Jalisco
sitios_pacifico2020<-subset(avisos2020.cosecha,NOMBRE.ESTADO %in% estados_pacifico)

#F i l t r a r AVISOS_MAYORES_MENORES_COSECHA_2021
avisos2021.cosecha <- fread("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2021.csv")
avisos2021.cosecha<-read.csv("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2021.csv",skip=2, header=TRUE)
#sitios_pacifico2021 filtro solo PACIFICO
sitios_pacifico2021<-subset(avisos2021.cosecha,LITORAL == "PACIFICO")
#sitios_pacifico2021 filtro Edos Pacifico y Jalisco
sitios_pacifico2021<-subset(avisos2021.cosecha,NOMBRE.ESTADO %in% estados_pacifico)

#F i l t r a r AVISOS_MAYORES_MENORES_COSECHA_2022
avisos2022.cosecha <- fread("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2022.csv")
avisos2022.cosecha<-read.csv("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_2022.csv",skip=2, header=TRUE)
#sitios_pacifico2022 filtro solo PACIFICO
sitios_pacifico2022<-subset(avisos2022.cosecha,LITORAL == "PACIFICO")
#sitios_pacifico2022 filtro Edos Pacifico y Jalisco
sitios_pacifico2022<-subset(avisos2022.cosecha,NOMBRE.ESTADO %in% estados_pacifico)



#nombres unicos de puerto de arribo

#sitios de arribo atlas localidades pesqueras

atlas.localidades <- sf::st_read("atlas_localidades_pesqueras.shp")

#----
library(tidyverse)
library(here)
library(data.table)
library(readr)
library(sf) # para localidades
library(readxl)

#F i l t r a r AVISOS_MAYORES_MENORES_COSECHA_2018 - 2022

estados.pacifico<-c("BAJA CALIFORNIA", "BAJA CALIFORNIA SUR", "SONORA", "SINALOA", "NAYARIT", "JALISCO")

estearchivo = "2018"

archivos.cosecha.yrs <- c("2018","2019","2020","2021","2022")

avisos_cosecha <- function(estearchivo, estados.pacifico){ #argumentos que pasan a la funcion
  
  #fread es mas eficiente para lectura de archivos grandes
  avisos.cosecha <- fread(paste0("/home/atlantis/mount-folder/CONAPESCA/AVISOS_ MAYORES_MENORES_COSECHA_",estearchivo,".csv"), skip = 2)
  
  head(avisos.cosecha) #ver nombres de columnas
  
  #sitios_pacifico filtro Edos Pacifico
  sitios.pacifico <-subset(avisos.cosecha,`NOMBRE ESTADO` %in% estados.pacifico)
  
  readr::write_csv(sitios.pacifico, paste0("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_",estearchivo,".csv"))
  
  print(paste(estearchivo,"completado")) #imprimir mensaje archivo analizado
}


#corre la funcion creada, avisos_cosecha, a todos los elementos de archivos.cosecha.yrs y usa estados.pacifico

lapply(archivos.cosecha.yrs, avisos_cosecha, estados.pacifico)


#leer los archivos guardados, solo con la captura del Golfo de California

#usa esta sintaxis para los otros archivos 2019-2022
avisos2018.loc <- fread("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_2018.csv") %>% 
  dplyr::distinct(`NOMBRE ESTADO`,`NOMBRE SITIO DESEMBARQUE`) %>% #sitios de arribo unicos
  dplyr::mutate(sitio_arribo = tolower(`NOMBRE SITIO DESEMBARQUE`)) %>% #sitios en minuscula
  dplyr::mutate(entidad = tolower(`NOMBRE ESTADO`)) #estados en minuscula

avisos2019.loc <- fread("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_2019.csv") %>% 
  dplyr::distinct(`NOMBRE ESTADO`,`NOMBRE SITIO DESEMBARQUE`) %>% 
  dplyr::mutate(sitio_arribo = tolower(`NOMBRE SITIO DESEMBARQUE`)) %>% 
  dplyr::mutate(entidad = tolower(`NOMBRE ESTADO`))

avisos2020.loc <- fread("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_2020.csv") %>% 
  dplyr::distinct(`NOMBRE ESTADO`,`NOMBRE SITIO DESEMBARQUE`) %>% 
  dplyr::mutate(sitio_arribo = tolower(`NOMBRE SITIO DESEMBARQUE`)) %>% 
  dplyr::mutate(entidad = tolower(`NOMBRE ESTADO`))

avisos2021.loc <- fread("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_2021.csv")%>% 
  dplyr::distinct(`NOMBRE ESTADO`,`NOMBRE SITIO DESEMBARQUE`) %>% 
  dplyr::mutate(sitio_arribo = tolower(`NOMBRE SITIO DESEMBARQUE`)) %>% 
  dplyr::mutate(entidad = tolower(`NOMBRE ESTADO`))

avisos2022.loc <- fread("/home/atlantis/mount-folder/CONAPESCA/captura_pacifico_2022.csv")%>% 
  dplyr::distinct(`NOMBRE ESTADO`,`NOMBRE SITIO DESEMBARQUE`) %>% 
  dplyr::mutate(sitio_arribo = tolower(`NOMBRE SITIO DESEMBARQUE`)) %>% 
  dplyr::mutate(entidad = tolower(`NOMBRE ESTADO`))

#lee el archivo 2017 y filtra las localidades correspondientes
ruta_xlsx <- "/home/atlantis/mount-folder/CONAPESCA/2017.xlsx"
datos <- read_excel(ruta_xlsx)

#lee el archivo 2000-2016 y filtra las localidades correspondientes

#une las listas de localidades 2000-2022 y asegurate otra vez que son los sitios unicos 

#leer shapefile con sitios de arribo atlas localidades pesqueras

atlas.localidades <- sf::st_read("/home/atlantis/mount-folder/CONAPESCA/atlas_localidades_pesqueras.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::distinct(ESTADO, LOCALIDAD) %>% 
  subset(ESTADO %in% estados.pacifico)


