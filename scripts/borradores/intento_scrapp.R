#_______________________________________________________________________________

# Prueba de scrapping 

rm(list = ls()); gc()

library(pacman)

p_load(
  tidyverse,
  data.table,
  rvest,
  chromote,
  RSelenium
)

url <- "http://siel.stps.gob.mx:303/ibmcognos/cgi-bin/cognos.cgi?b_action=cognosViewer&ui.action=run&ui.object=XSSSTART*2fcontent*2ffolder*5b*40name*3d*27Sitio*20STPS*27*5d*2ffolder*5b*40name*3d*273.*20Revisiones*20del*20salario*20contractual*27*5d*2freport*5b*40name*3d*273.2.7.2*20Sector*20de*20Actividad*20SCIAN*20(Incrementos)*27*5dXSSEND&ui.name=XSSSTART3.2.7.2*20Sector*20de*20Actividad*20SCIAN*20(Incrementos)XSSEND&run.outputFormat=&run.prompt=true"

# ----------------

dynamic <- read_html_live(url)

str(dynamic)

dynamic$click(css = "div > a:nth-of-type(1)")

dynamic$view()


tabla <- dynamic %>% 
  html_table()

dynamic <- 



# ----------------

b <- ChromoteSession$new()

b$Page$navigate(url)

anios <- b$Runtime$evaluate('
    Array.from(document.querySelectorAll("input.dijitCheckBoxInput"))
         .map(x => ({id: x.id, dv: x.getAttribute("dv")}))
')$result$value

Sys.sleep(6) 

# --------------------------------------------

rD <- rsDriver(browser = "chrome", 
               chromever = "latest",
               verbose = FALSE)

remDr <- rD$client



