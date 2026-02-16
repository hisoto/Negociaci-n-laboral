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

# Datos ------------------------------------------------------------------------

federal <- read_excel("excels/negociaciones_stata.xlsx", sheet = "j_federal") |>
  clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    direc = as.factor(ifelse(real >= 0, "Positivo", "Negativo"))
  ) |>
  filter(fecha >= fecha_inicio & fecha <= fecha_interes)

local <- read_excel("excels/negociaciones_stata.xlsx", sheet = "j_local") |>
  clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    direc = as.factor(ifelse(real >= 0, "Positivo", "Negativo"))
  ) |>
  filter(fecha >= fecha_inicio & fecha <= fecha_interes)

revisiones <- rbind(
  federal |> mutate(juris = "Federal"),
  local |> mutate(juris = "Local")
) |> 
  mutate(
    year = as.integer(year), 
    revisiones = as.integer(revisiones),
    trabajadores = as.integer(trabajadores)
  )

rm(federal, local)

attach(revisiones)

# serie -------------------------------------------------------------------------

# Federal ______________________________________________________________________

ggplot(revisiones |> filter(juris == "Federal")) +
  geom_point(
    mapping = aes(x = fecha, y = real, color = direc),
    shape = 1,
    show.legend = FALSE
  ) +
  geom_line(mapping = aes(x = fecha, y = real),
            color = "#611232") +
  geom_bar(
    data = revisiones |> filter(juris == "Federal"),
    mapping = aes(x = fecha, y = real, fill = direc),
    stat = "identity",
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Positivo" = "#1e5b4f",
    "Negativo" = "#611232"
  )) +
  scale_color_manual(values = c(
    "Positivo" = "#1e5b4f",
    "Negativo" = "#611232"
  ))+ 
  geom_text(
    data = revisiones |> filter(juris == "Federal" &
                                   fecha == fecha_interes),
    mapping = aes(
      x = fecha,
      y = real,
      label = sprintf("%.2f", real),
      color = direc
    ),
    vjust = -.5,
    hjust = -.5,
    size = 6, 
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_abline(
    slope = 0,
    intercept = 0,
    color = "black",
    size = 0.5,
    linetype = "dotted"
  ) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date(min(revisiones$fecha)), as.Date(max(revisiones$fecha)), by = "year")) +
  labs(x = "", y = "Variación Salarial Real (%)", color = "") +
  theme_conasami() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

name <- paste0("graphs/juris/ts_juris_federal_", fecha_interes |> format("%Ym%m"), ".png")

ggsave(name, plot = last_plot(), 
       width = 50, height = 20, units = "cm")

# Local ________________________________________________________________________

ggplot(revisiones |> filter(juris == "Local")) +
  geom_point(
    mapping = aes(x = fecha, y = real, color = direc),
    shape = 1,
    show.legend = FALSE
  ) +
  geom_line(mapping = aes(x = fecha, y = real),
            color = "#611232") +
  geom_bar(
    data = revisiones |> filter(juris == "Local"),
    mapping = aes(x = fecha, y = real, fill = direc),
    stat = "identity",
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Positivo" = "#1e5b4f",
    "Negativo" = "#611232"
  )) +
  scale_color_manual(values = c(
    "Positivo" = "#1e5b4f",
    "Negativo" = "#611232"
  )) +
  geom_text(
    data = revisiones |> filter(juris == "Local") |> filter(fecha == ifelse(
      fecha < fecha_interes, max(fecha), fecha_interes
    )),
    mapping = aes(
      x = fecha,
      y = real,
      label = sprintf("%.2f", real),
      color = direc
    ),
    vjust = -.5,
    hjust = -.5,
    size = 6,
    fontface = "bold",
    show.legend = FALSE
  ) +
  geom_abline(
    slope = 0,
    intercept = 0,
    color = "black",
    size = 0.5,
    linetype = "dotted"
  ) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date(min(revisiones$fecha)), as.Date(max(revisiones$fecha)), by = "year")) +
  labs(x = "", y = "Variación Salarial Real (%)", color = "") +
  theme_conasami() +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20))

name <- paste0("graphs/juris/ts_juris_local_",
               fecha_interes |> format("%Ym%m"),
               ".png")

ggsave(
  name,
  plot = last_plot(),
  width = 50,
  height = 20,
  units = "cm"
)

# grafica de barras revisiones y número de trabajadores ------------------------

# Barras Federal _______________________________________________________________

barras <- revisiones |>
  select(fecha, year, mes, juris, direc, revisiones, trabajadores) |>
  pivot_longer(
    cols = c(revisiones, trabajadores),
    names_to = "variable",
    values_to = "valor"
  ) |>
  mutate(year = lubridate:::floor_date(fecha, unit = "year"))

ggplot(barras |> filter(juris == "Federal" &
                           mes == month(as.Date(fecha_interes)))) +
  geom_col(aes(x = year, y = valor), fill = "#98989A", color = "#161a1d") +
  geom_text(
    data = barras |> filter(juris == "Federal" &
                               mes == month(as.Date(fecha_interes)) & valor != 0),
    mapping = aes(
      x = year,
      y = valor,
      label = scales::label_number(scale = 1)(valor)
    ),
    vjust = -0.4,
    size = 5,
    fontface = "bold",
    show.legend = FALSE,
    color = "#161a1d"
  ) +
  facet_wrap( ~ variable, scales = "free_y", labeller = as_labeller(c(
    revisiones   = "Revisiones", trabajadores = "Personas trabajadoras"
  ))) +
  scale_y_continuous(labels = scales::label_number(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date(min(revisiones$fecha)), as.Date(max(revisiones$fecha)), by = "year")) +
  theme_conasami() +
  labs(x = "", y = "", title = "") + 
  theme(axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text = element_text(size = 20))

name <- paste0("graphs/juris/barras_federal_", fecha_interes |> format("%Ym%m"), ".png")

ggsave(name, plot = last_plot(), width = 50, height = 15, units = "cm")

# Barras Local _________________________________________________________________

ggplot(barras |> filter(juris == "Local") |> filter(mes == month(as.Date(max(fecha))))) +
  geom_col(aes(x = year, y = valor), fill = "#98989A", color = "#161a1d") +
  geom_text(
    data = barras |> filter(juris == "Local") |> 
                               filter(mes == month(as.Date(max(fecha))) & valor != 0),
    mapping = aes(
      x = year,
      y = valor,
      label = scales::label_number(scale = 1)(valor)
    ),
    vjust = -0.4,
    size = 5,
    fontface = "bold",
    show.legend = FALSE,
    color = "#161a1d"
  ) +
  facet_wrap( ~ variable, scales = "free_y", labeller = as_labeller(c(
    revisiones   = "Revisiones", trabajadores = "Personas trabajadoras"
  ))) +
  scale_y_continuous(labels = scales::label_number(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date(min(revisiones$fecha)), as.Date(max(revisiones$fecha)), by = "year")) +
  theme_conasami() +
  labs(x = "", y = "", title = "") + 
  theme(axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text = element_text(size = 20))


name <- paste0("graphs/juris/barras_local_", fecha_interes |> format("%Ym%m"), ".png")

ggsave(name, plot = last_plot(), width = 50, height = 15, units = "cm")

# tabla estadistica ------------------------------------------------------------ 


tabla_anual <- revisiones |>
  group_by(juris, year) |>
  summarise(
    revisiones = sum(revisiones),
    trabajadores = sum(trabajadores),
    real = mean(real),
    nominal = mean(nominal),
    .groups = "drop") |>
      arrange(desc(year)) |> 
  # agregamos formato 
  mutate(
    revisiones = scales::label_number(scale = 1)(revisiones),
    trabajadores = scales::label_number(scale = 1)(trabajadores),
    real = sprintf("%.2f%%", real),
    nominal = sprintf("%.2f%%", nominal)
  ) 

print(tabla_anual)  

print(revisiones |> filter(fecha == fecha_interes)) 

print(revisiones |> filter(juris == "Local") |> filter(fecha == max(fecha)))







