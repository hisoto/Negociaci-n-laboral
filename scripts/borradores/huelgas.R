#_______________________________________________________________________________

# Objetivo: Hacer gráficas de huelgas estalladas y vigentes. 

#_______________________________________________________________________________

rm(list = ls());gc()

source("rscrips/theme_conasami_dt2026.R")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  lubridate
)

fecha_inicio <- as.Date("2021-01-01")

fecha_interes <- as.Date("2026-06-01")

ruta_externa  <- ruta_dt_automatizacion()

# Datos ------------------------------------------------------------------------

huelgas <- read_excel("excels/HUELGAS.xlsx") |> 
  rename(
    fecha = 1, 
    Estalladas = 2, 
    Vigentes = 3
  ) |> 
  mutate(
    across(everything(), as.integer)
  ) |> 
  pivot_longer(
    cols = -fecha,
    names_to = "tipo",
    values_to = "valor"
  )

# Gráfica

g_huelgas <- ggplot(huelgas) +
  geom_col(
    mapping = aes(
      x = fecha,
      y = valor,
      fill = tipo
    ),
    position = position_dodge(width = 1),
  ) +
  geom_text(
    mapping = aes(
      x = fecha,
      y = valor,
      label = ifelse(valor == 0, NA, scales::label_number(scale = 1)(valor)),
      group = tipo
    ),
    position = position_dodge(width = 1),
    vjust = -0.5,
    size = 3,
    family = "Noto Sans",
    show.legend = FALSE,
    color = conasami_neutros[["texto_secundario"]]
  ) +
  scale_fill_conasami() +
  labs(fill = NULL) +
  scale_x_continuous(
    breaks = seq(min(huelgas$fecha),
                 max(huelgas$fecha),
                 by = 1)
   ) +
  scale_y_continuous(labels = scales::label_number(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  theme_conasami() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

g_huelgas

guardar_grafica_conasami(
  g_huelgas,
  archivo     = paste0("bar_huelgas_", format(fecha_interes, "%Ym%m")),
  dir         = "graphs/huelgas",
  tamano      = "ancho",
  dest        = file.path(ruta_externa, "graphs")
)

rm(huelgas)

# Gráfica de huelgas por tipo de conflicto

huelgas_causa <- read_excel("excels/Libro1.xlsx", sheet = "df") |>
  filter(if_all(everything(), ~ !is.na(.))) |>
  mutate(huelgas = 1) |>
  mutate(year = year(`Fecha de inicio`)) |>
  group_by(Causa, year) |>
  summarise(sum(huelgas))

glimpse(huelgas_causa)

labels_causa <- unique(huelgas_causa$Causa)

g_huelgas_causa <- ggplot(huelgas_causa) +
  geom_col(
    mapping = aes(x = factor(year), y = `sum(huelgas)` , fill = Causa),
    stat = "identity",
    position = "stack") +
  scale_fill_conasami(labels = labels_causa) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_conasami()

g_huelgas_causa

guardar_grafica_conasami(
  g_huelgas_causa,
  archivo     = paste0("bar_huelgas_causa_", format(fecha_interes, "%Ym%m")),
  dir         = "graphs/huelgas",
  tamano      = "ancho",
  dest        = file.path(ruta_externa, "graphs")
)

