#_______________________________________________________________________________

  # Héctor Iván Soto Parra 
  # 14 de febrero de 2025
  # Comisiòn Nacional de los Salarios Mínimos
  # Coordinación para el análisis de la Economía Laboral 
  # El proposito de este código es hacer webscrapping de los datos de stps   
#_______________________________________________________________________________

### Esta sección del código hace webscrapping. Es decir, toma el archivo url y descarga el código de una 
### página de internet para obtener sus datos. 

url_emplazamientos_entidad  <- "http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*275.*20Emplazamientos*20y*20Huelgas*27*5d*2ffolder*5b*40name*3d*27Emplazamientos*27*5d*2freport*5b*40name*3d*275.2.5*20Emplazamientos*20a*20Huelgas*20por*20Entidad*20Federativa*27*5dXSSEND&ui.name=XSSSTART5.2.5*20Emplazamientos*20a*20Huelgas*20por*20Entidad*20FederativaXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3di70D6953071C0436C96A5766BB7E26AAFXSSEND"

paths_allowed(url_emplazamientos_entidad)

### Leer contenido de la pagina 

emplazamientos_entidad_html <- read_html(url_emplazamientos_entidad)
### convertir a txt 
emplazamientos_entidad_tex <- html_text(emplazamientos_entidad_html)
emplazamientos_entidad_tex

#### Extraer tablas 
tablas <- emplazamientos_entidad_html %>%
  html_table()

#Es necesario revisar la lista de tablas para identificar las correctas.

emplazamientos_entidad <- tablas[[17]]  

#______________________________________________________________Huelgas_por_Causa

url_2 <-"http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*275.*20Emplazamientos*20y*20Huelgas*27*5d*2ffolder*5b*40name*3d*27Huelgas*27*5d*2freport*5b*40name*3d*275.3.1*20Huelgas*20por*20Causa*27*5dXSSEND&ui.name=XSSSTART5.3.1*20Huelgas*20por*20CausaXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3diE3F867D94C284523A4365FB5AEED67E8XSSEND"
paths_allowed(url_2)

### Leer contenido de la pagina 

huelgas_causa <- read_html(url_2)
### convertir a txt 
tex_huelgas_causa <- html_text(huelgas_causa)
tex_huelgas_causa

#### Extraer tablas 

tablas_huelgas_causa <- huelgas_causa %>%
  html_table()

huelgas_causa <- tablas_huelgas_causa[[13]]

#___________________________________________________________emplazamientos_causa

url_3 <- "http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*275.*20Emplazamientos*20y*20Huelgas*27*5d*2ffolder*5b*40name*3d*27Emplazamientos*27*5d*2freport*5b*40name*3d*275.2.1*20Emplazamientos*20a*20Huelgas*20por*20Causa*27*5dXSSEND&ui.name=XSSSTART5.2.1*20Emplazamientos*20a*20Huelgas*20por*20CausaXSSEND&run.outputFormat=&run.prompt=true&ui.backURL=XSSSTART*2fibmcognos*2fcgi-bin*2fcognos.cgi*3fb_5faction*3dxts.run*26m*3dportal*2fcc.xts*26m_5ffolder*3di70D6953071C0436C96A5766BB7E26AAFXSSEND"
paths_allowed(url_3)

emplazamientos_causa <- read_html(url_3)

tablas_emplazamientos_causa <- emplazamientos_causa %>% 
  html_table()


emplazamientos_causa <- tablas_emplazamientos_causa[[15]]

#_______________________________________________________________________________

#Esta sección procesa los datos y los exporta a una excel. 
  #Creando el libro de trabajo 

#_________________________________________________________emplazamientos_entidad 

libro_trabajo <- createWorkbook()
addWorksheet(libro_trabajo,"emplazamientos_entidad")

nombres_entidad_emplazamiento <- emplazamientos_entidad %>% 
  slice(1) %>% 
  unlist()

emplazamientos_entidad_df <- emplazamientos_entidad %>% 
  slice(-1) %>% 
  setNames(nombres_entidad_emplazamiento) %>% 
  rename(year = 1,
         fecha = 2) %>% 
  filter(fecha != "Total") %>% 
  mutate(year = as.numeric(year),
         mes =  match(substr(fecha, 6, 8), month.abb),
         fecha = make_date(year, mes)) %>% 
  mutate(across(-c(fecha), as.numeric)) %>% 
  select(fecha, year, mes, everything()) %>% 
  arrange(desc(fecha)) %>% 
  rename("Ciudad de México" = `Distrito Federal 1`)

glimpse(emplazamientos_entidad_df)
  
writeData(libro_trabajo, "emplazamientos_entidad", emplazamientos_entidad_df)
saveWorkbook(libro_trabajo,"excels/negociaciones_scrapp.xlsx", overwrite = TRUE)

#__________________________________________________________________huelgas_causa

addWorksheet(libro_trabajo,"huelgas_causa")

first_row <- huelgas_causa[1, ]
second_row <- huelgas_causa[2, ]

new_colnames <- paste(first_row, second_row, sep = "_")

df_huelgas_causa <- huelgas_causa %>% 
  slice(-c(1,2)) %>% 
  setNames(new_colnames) %>% 
  rename(year=1,
         fecha=2) %>% 
  filter(fecha != "Total") %>% 
  mutate(year = as.numeric(year),
         mes =  match(substr(fecha, 6, 8), month.abb),
         fecha = make_date(year, mes)) %>% 
  mutate(across(-c(fecha), as.numeric)) %>% 
  select(fecha, year, mes, everything()) %>% 
  arrange(desc(fecha))

glimpse(df_huelgas_causa)

writeData(libro_trabajo, "huelgas_causa", df_huelgas_causa)
saveWorkbook(libro_trabajo,"excels/negociaciones_scrapp.xlsx", overwrite = TRUE)


#__________________________________________________________________Huelgas_causa

addWorksheet(libro_trabajo,"emplazamientos_causa") 

first_row <- emplazamientos_causa[1, ]
second_row <- emplazamientos_causa[2, ]

new_colnames <- paste(first_row, second_row, sep = "_")

emplazamientos_causa_1 <- emplazamientos_causa %>% 
  slice(-c(1,2)) %>% 
  setNames(new_colnames) %>% 
  rename(year=1,
         fecha=2) %>% 
  filter(fecha != "Total") %>% 
  mutate(year = as.numeric(year),
         mes =  match(substr(fecha, 6, 8), month.abb),
         fecha = make_date(year, mes)) %>% 
  mutate(across(-c(fecha), as.numeric)) %>% 
  select(fecha, year, mes, everything()) %>% 
  arrange(desc(fecha))

glimpse(emplazamientos_causa_1)



writeData(libro_trabajo, "emplazamientos_causa", emplazamientos_causa_1)
saveWorkbook(libro_trabajo,"excels/negociaciones_scrapp.xlsx", overwrite = TRUE)


rm(list = ls())




