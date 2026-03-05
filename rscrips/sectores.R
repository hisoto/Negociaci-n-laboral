#_______________________________________________________________________________

# Objetivo: Hacer gráficas de revisiones salariales jurisdicción federal

#_______________________________________________________________________________

rm(list = ls());gc()

source("rscrips/theme_conasami.R")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  lubridate
)

fecha_inicio <- as.Date("2021-01-01")

fecha_interes <- as.Date("2026-01-01")

# Data 

nominal <- read_excel("excels/incrementos_sector.xlsx", skip = 8) |> 
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


real <- read_excel("excels/incrementos_sector.xlsx", skip = 8) |> 
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
  filter(fecha == fecha_interes)

ggplot(sectores_fecha_interes |> filter(tipo == "Real" | valor != NA)) +
  geom_col(
    mapping = aes(
      x = reorder(sector, valor), 
      y = valor
    )
  ) + 
  theme_conasami() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 1, angle = 45), 
    axis.text.y = element_text(size = 4)
  ) +
  coord_flip()

name <- paste0("graphs/sectores/barra_sectores_", fecha_interes |> format("%Ym%m"), ".png")

ggsave(name, plot = last_plot(), 
       width = 50, height = 20, units = "cm")

ggplot(
  sectores |> 
    filter(tipo != "Real")
) +
  geom_boxplot(
    mapping = aes(
      x = sector,
      y = valor
    )
  ) +
  coord_flip()

