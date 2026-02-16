#_______________________________________________________________________________

# Héctor Iván Soto Parra 
# 14 DE ENERO 2025
# Intento de código para web scrapping 

#_______________________________________________________________________________

library(httr)
library(readxl)
library(openxlsx)
library(dplyr) 
library(tidyverse)
library(haven)
library(rvest)
library(dplyr)
library(xml2)
library(stringr)
library(robotstxt)

#_______________________________________________________________________________


remove(list = ls())

setwd(
  "~/OneDrive - Esl Colegio de México A.C/CONASAMI/PROYECTOS/Informe mensual/huelgas arturo "
)

database <- "database/"

if (!dir.exists(database)) {
  dir.create(database, recursive = TRUE)
}

#_______________________________________________________________________________

### Esta sección del código hace webscrapping. Es decir, toma el archivo url y descarga el código de una 
### página de internet para obtener sus datos. 

url_1 <-
  "http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*273.*20Revisiones*20del*20salario*20contractual*27*5d*2freport*5b*40name*3d*273.2.1*20Resumen*20Jurisdicci*c3*b3n*20Federal*27*5dXSSEND&ui.name=XSSSTART3.2.1*20Resumen*20Jurisdicci*c3*b3n*20FederalXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3diAB3120B47B544DC8A38675A3C1DB8A9CXSSEND"

paths_allowed(url_1)

### Leer contenido de la pagina 

juris_federal <- read_html(url_1)
### convertir a txt 
tex_juris_federal <- html_text(juris_federal)
tex_juris_federal

#### Extraer tablas 
tablas <- juris_federal %>%
  html_table()

tabla_trabajadores <- tablas[[13]]

tabla_incremento_nominal <- tablas[[14]]

tabla_incremento_real <- tablas[[15]]


#_______________________________________________________________________________
### Esta sección del código procesa los dataframes 


### Revisiones y trabajadores 

tabla_trabajadores[1, 1] <- "año"
tabla_trabajadores[1, 2] <- "mes"
tabla_trabajadores[1, 3] <- "revisiones"
tabla_trabajadores[1, 4] <- "trabajadores"

colnames(tabla_trabajadores) <- tabla_trabajadores[1, ]
tabla_trabajadores <- tabla_trabajadores[-1,]

tabla_trabajadores <- tabla_trabajadores[tabla_trabajadores$mes != "Total", ]

tabla_trabajadores$revisiones <- gsub("\\.", "", tabla_trabajadores$revisiones)
tabla_trabajadores$trabajadores <- gsub("\\.", "", tabla_trabajadores$trabajadores)


tabla_trabajadores <- tabla_trabajadores %>%
  mutate(mes = month(parse_date_time(mes, "Y/b"))) %>%
  mutate(across(everything(), as.numeric))


### incremento real 

tabla_incremento_real[1, 1] <- "año"
tabla_incremento_real[1, 2] <- "mes"
tabla_incremento_real[1, 3] <- "real"

colnames(tabla_incremento_real) <- tabla_incremento_real[1, ]
tabla_incremento_real <- tabla_incremento_real[-1,]

tabla_incremento_real <- tabla_incremento_real[tabla_incremento_real$mes != "Total", ]

tabla_incremento_real <- tabla_incremento_real %>%
  mutate(mes = month(parse_date_time(mes, "Y/b"))) %>%
  mutate(across(everything(), as.numeric))

### incremento nominal 

tabla_incremento_nominal[1, 1] <- "año"
tabla_incremento_nominal[1, 2] <- "mes"
tabla_incremento_nominal[1, 3] <- "nominal"

colnames(tabla_incremento_nominal) <- tabla_incremento_nominal[1, ]
tabla_incremento_nominal <- tabla_incremento_nominal[-1,]

tabla_incremento_nominal <- tabla_incremento_nominal[tabla_incremento_nominal$mes != "Total", ]

tabla_incremento_nominal <- tabla_incremento_nominal %>%
  mutate(mes = month(parse_date_time(mes, "Y/b"))) %>%
  mutate(across(everything(), as.numeric))


### pegar base y exportar a excel  

lista_juris_federal <- list(tabla_trabajadores,tabla_incremento_real,tabla_incremento_nominal)
df_juris_federal <- reduce(lista_juris_federal, full_join, by = c("año", "mes"))

libro_trabajo <- createWorkbook()
addWorksheet(libro_trabajo,"Jurisdicción Federal")
writeData(libro_trabajo, "Jurisdicción Federal", df_juris_federal)
saveWorkbook(libro_trabajo,"excel/negociaciones_2.xlsx", overwrite = TRUE)






