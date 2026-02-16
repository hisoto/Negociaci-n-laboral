library(tidyverse)
library(rvest)
library(robotstxt)
library(openxlsx)
pacman::p_load(RSelenium)
pacman::p_load(chromote)

#_______________________________________________________________________________

#Héctor Iván Soto Parra 
#Wepscrapping para negocioaciones laborales 

# El objetivo del código es obtener la información necesaria para hacer la 
# seccion de negociaciones laborales del informe mensual de la conasami 


#_______________________________________________________________________________


remove(list = ls())

database <- "database/"

if (!dir.exists(database)) {
  dir.create(database, recursive = TRUE)
}

#_______________________________________________________________________________

##### 
#_________________Revisiones, trabajadores e incrementos de jurisdicción federal

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

lista_juris_federal <- list(tabla_trabajadores,tabla_incremento_real,tabla_incremento_nominal)
df_juris_federal <- reduce(lista_juris_federal, full_join, by = c("año", "mes"))

libro_trabajo <- createWorkbook()
addWorksheet(libro_trabajo,"Jurisdicción Federal")
writeData(libro_trabajo, "Jurisdicción Federal", df_juris_federal)
saveWorkbook(libro_trabajo,"excel/negociaciones_2.xlsx", overwrite = TRUE)

##### 
#______________________________________________________________Huelgas_por_Causa
url_2 <-
  "http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*275.*20Emplazamientos*20y*20Huelgas*27*5d*2ffolder*5b*40name*3d*27Emplazamientos*27*5d*2freport*5b*40name*3d*275.2.1*20Emplazamientos*20a*20Huelgas*20por*20Causa*27*5dXSSEND&ui.name=XSSSTART5.2.1*20Emplazamientos*20a*20Huelgas*20por*20CausaXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3di70D6953071C0436C96A5766BB7E26AAFXSSEND"

paths_allowed(url_2)

### Leer contenido de la pagina 

huelgas_causa <- read_html(url_2)
### convertir a txt 
tex_huelgas_causa <- html_text(huelgas_causa)
tex_huelgas_causa

#### Extraer tablas 

tablas_huelgas_causa <- huelgas_causa %>%
  html_table()

df_huelgas_causa <- tablas_huelgas_causa[[14]]

first_row <- df_huelgas_causa[1, ]
second_row <- df_huelgas_causa[2, ]

new_colnames <- paste(first_row, second_row, sep = "_")

colnames(df_huelgas_causa) <- new_colnames

df_huelgas_causa <- df_huelgas_causa[-c(1, 2), ]

col_names <- colnames(df_huelgas_causa)

index_to_rename <- which(col_names == "Número de Emplazamientos_Número de Emplazamientos")[2]  


col_names[index_to_rename] <- "fecha"

colnames(df_huelgas_causa) <- col_names

df_huelgas_causa_1 <- df_huelgas_causa%>%
  rename("año" = "Número de Emplazamientos_Número de Emplazamientos") %>% 
  filter(fecha != "Total") %>%
  mutate( fecha=make_date(fecha))

df_huelgas_causa_1 <- df_huelgas_causa %>%
  rename("año" = "Número de Emplazamientos_Número de Emplazamientos") %>%
  filter(fecha != "Total") %>%
  mutate(
    year = as.numeric(substr(fecha, 1, 4)),  # Extract year
    mes = match(substr(fecha, 6, 8), month.abb),  # Extract month number
    fecha = make_date(year, mes, 1)  # Create a date (day set to 1 by default)
  )%>% 
  select(-year) %>%
  select(año, mes, fecha, everything())%>%
  mutate(across(c(-fecha), as.numeric))


addWorksheet(libro_trabajo,"Huelgas por Causa")
writeData(libro_trabajo, "Huelgas por Causa", df_huelgas_causa_1)
saveWorkbook(libro_trabajo,"excel/negociaciones_2.xlsx", overwrite = TRUE)

##### 
#_________________Revisiones, trabajadores e incrementos de jurisdicción federal


rD <- rsDriver(browser = "chrome")
remDr <- rD$client

# Navigate to the page
remDr$navigate("http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*273.*20Revisiones*20del*20salario*20contractual*27*5d*2freport*5b*40name*3d*273.2.10*20Central*20Obrera*27*5dXSSEND&ui.name=XSSSTART3.2.10*20Central*20ObreraXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3diAB3120B47B544DC8A38675A3C1DB8A9CXSSEND#")


url_3 <-
  "http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*273.*20Revisiones*20del*20salario*20contractual*27*5d*2freport*5b*40name*3d*273.2.10*20Central*20Obrera*27*5dXSSEND&ui.name=XSSSTART3.2.10*20Central*20ObreraXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3diAB3120B47B544DC8A38675A3C1DB8A9CXSSEND#"

paths_allowed(url_3)

### Leer contenido de la pagina 

incremento_ <- read_html(url_3)
### convertir a txt 
tex_huelgas_causa <- html_text(huelgas_causa)
tex_huelgas_causa

#### Extraer tablas 

tablas_huelgas_causa <- huelgas_causa %>%
  html_table()



chrome_session <- ChromoteSession$new() 
chrome_session$view()
chrome_session$Page$navigate("http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*273.*20Revisiones*20del*20salario*20contractual*27*5d*2freport*5b*40name*3d*273.2.10*20Central*20Obrera*27*5dXSSEND&ui.name=XSSSTART3.2.10*20Central*20ObreraXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3diAB3120B47B544DC8A38675A3C1DB8A9CXSSEND#")
chrome_session$Runtime$evaluate("PRMT_SV_N185D41A0x1BD65C94_NS_")

