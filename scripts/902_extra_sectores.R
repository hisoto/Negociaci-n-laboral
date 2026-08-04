#_______________________________________________________________________________

# Objetivo: Ranking de incrementos reales por sector de actividad (SCIAN)
#
# EXTRA (prefijo 9xx): no forma parte del informe mensual de la DT — el informe
# lleva la TABLA de sectores (011_tabla_sectores.R), no esta gráfica. Lo corre
# Master_negociaciones.R (flujo local completo), NO
# Master_informe_negociaciones.R, y no viaja en la copia de la carpeta
# compartida. Sigue escribiendo en graphs/10_sectores/ para no partir el
# histórico.
#
# Ojo: lee incrementos_sector.xlsx (serie mensual por sector), que es un insumo
# distinto del SCIAN del mes que consume 011.
#_______________________________________________________________________________

rm(list = ls());gc()

source(here::here("scripts", "theme_conasami_dt2026.R"))

# Configuración (Master_negociaciones.R la define; fallback si se corre suelto)
if (is.null(getOption("negociaciones"))) source(here::here("scripts", "000_config.R"))
cfg <- getOption("negociaciones")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  lubridate
)


# Data 

nominal <- read_excel(file.path(cfg$ruta_poligonos, "incrementos_sector.xlsx"), skip = 8) |> 
  slice(1:497) |> 
  select(-c(`...1`, `...2`, `...3`)) |> 
  rename(
    fecha = 1,
    Nacional = Total
  ) |> 
  filter(fecha != "Total") |> 
  mutate(
    across(.cols = -fecha, .fns = as.numeric),
    fecha = paste0(fecha, "-01"),
    fecha = str_replace(fecha, "/", "-"),
    fecha = lubridate::parse_date_time(fecha, orders = "Y-b-d") |> lubridate::as_date(),
    tipo = "Nominal"
  ) 


real <- read_excel(file.path(cfg$ruta_poligonos, "incrementos_sector.xlsx"), skip = 8) |> 
  slice(499:994) |> 
   select(-c(`...1`, `...2`, `...3`)) |> 
  rename(
    fecha = 1,
    Nacional = Total
  ) |> 
  filter(fecha != "Total") |> 
   mutate(
    across(.cols = -fecha, .fns = as.numeric),
    fecha = paste0(fecha, "-01"),
    fecha = str_replace(fecha, "/", "-"),
    fecha = lubridate::parse_date_time(fecha, orders = "Y-b-d") |> lubridate::as_date(),
    tipo = "Real" 
  ) 

sectores <- rbind(
  nominal,
  real
) |> 
  pivot_longer(
    cols = -c(fecha, tipo), 
    names_to = "sector",
    values_to = "valor"
  )

rm(real, nominal)

# Grafica de incrementos reales por sector 

sectores_fecha_interes <- sectores |> 
  filter(fecha == cfg$fecha_interes)

g_sectores <- ggplot(sectores_fecha_interes |> filter(tipo == "Real")) +
  geom_col(
    mapping = aes(
      x = reorder(sector, valor),
      y = valor
    ),
    fill = conasami_colores[["guinda"]],
    width = 0.68
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_conasami() +
  coord_flip()

g_sectores

# Ranking con 20 categorías: no cabe en los 8 cm de alto del tamaño "ancho",
# por eso va en tamaño libre (Manual DT 2026 §11, ancho recomendado 17.5 cm).
guardar_grafica_conasami(
  g_sectores,
  archivo = paste0("barra_sectores_", format(cfg$fecha_interes, "%Ym%m")),
  dir     = here::here("graphs", "10_sectores"),
  tamano  = "libre",
  width   = 17.5,
  height  = 14
)

