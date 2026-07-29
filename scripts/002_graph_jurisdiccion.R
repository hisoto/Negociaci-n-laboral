#_______________________________________________________________________________

# Objetivo: Hacer gráficas de revisiones salariales jurisdicción federal

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

# Datos ------------------------------------------------------------------------

leer_sheet <- function(sheet, juris) {
  read_excel(file.path(cfg$ruta_poligonos, "negociaciones_stata.xlsx"), sheet = sheet) |>
    clean_names() |>
    mutate(
      fecha = as.Date(fecha),
      direc = as.factor(ifelse(real >= 0, "Positivo", "Negativo")),
      juris = juris
    )
}

revisiones_raw <- rbind(
  leer_sheet("j_federal", "Federal"),
  leer_sheet("j_local",   "Local")
) |>
  mutate(
    year         = as.integer(year),
    revisiones   = as.integer(revisiones),
    trabajadores = as.integer(trabajadores)
  )

# Series de tiempo (y tabla estadística): rango global
revisiones <- revisiones_raw |>
  filter(fecha >= cfg$fecha_inicio_series & fecha <= cfg$fecha_interes)

# Barras: rango propio
revisiones_barras <- revisiones_raw |>
  filter(fecha >= cfg$fecha_inicio_barras & fecha <= cfg$fecha_interes)

rm(revisiones_raw)

attach(revisiones)

# serie -------------------------------------------------------------------------

# Federal ______________________________________________________________________

g_ts_federal <- ggplot(revisiones |> filter(juris == "Federal")) +
  geom_point(
    mapping = aes(x = fecha, y = real, color = direc),
    shape = 1,
    show.legend = FALSE
  ) +
  geom_line(mapping = aes(x = fecha, y = real),
            color = conasami_colores[["guinda_profundo"]],
            linewidth = 0.75, lineend = "round", linejoin = "round") +
  geom_bar(
    data = revisiones |> filter(juris == "Federal"),
    mapping = aes(x = fecha, y = real, fill = direc),
    stat = "identity",
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_direccion() +
  scale_color_direccion() +
  geom_hline(
    yintercept = 0,
    color = conasami_neutros[["texto_secundario"]],
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date(min(revisiones$fecha)), as.Date(max(revisiones$fecha)), by = "year")) +
  theme_conasami()

g_ts_federal

guardar_grafica_conasami(
  g_ts_federal,
  archivo     = paste0("ts_juris_federal_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "02_jurisdiccion"),
  tamano      = "ancho"
)

# Local ________________________________________________________________________

g_ts_local <- ggplot(revisiones |> filter(juris == "Local")) +
  geom_point(
    mapping = aes(x = fecha, y = real, color = direc),
    shape = 1,
    show.legend = FALSE
  ) +
  geom_line(mapping = aes(x = fecha, y = real),
            color = conasami_colores[["guinda_profundo"]],
            linewidth = 0.75, lineend = "round", linejoin = "round") +
  geom_bar(
    data = revisiones |> filter(juris == "Local"),
    mapping = aes(x = fecha, y = real, fill = direc),
    stat = "identity",
    alpha = 0.5,
    show.legend = FALSE
  )  +
  scale_fill_direccion() +
  scale_color_direccion() +
  geom_hline(
    yintercept = 0,
    color = conasami_neutros[["texto_secundario"]],
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date(min(revisiones$fecha)), as.Date(max(revisiones$fecha)), by = "year")) +
  theme_conasami()

g_ts_local

guardar_grafica_conasami(
  g_ts_local,
  archivo     = paste0("ts_juris_local_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "02_jurisdiccion"),
  tamano      = "ancho"
)

# grafica de barras revisiones y número de trabajadores ------------------------

# Cada variable (revisiones / personas trabajadoras) se exporta por separado,
# sin título y en tamaño medio (8 x 8 cm). El título de cada gráfica vive en la
# tabla-envoltorio de Word.

barras <- revisiones_barras |>
  select(fecha, year, mes, juris, direc, revisiones, trabajadores) |>
  pivot_longer(
    cols = c(revisiones, trabajadores),
    names_to = "variable",
    values_to = "valor"
  ) |>
  mutate(year = lubridate:::floor_date(fecha, unit = "year"))

# ── helper local: una barra por (juris, variable) ────────────────────────────
g_barra_juris <- function(df) {
  ggplot(df) +
    geom_col(aes(x = year, y = valor), fill = conasami_colores[["gris"]]) +
    scale_y_continuous(labels = scales::label_number(scale = 1),
                       expand = expansion(mult = c(0, 0.05))) +
    scale_x_date(date_labels = "%Y",
                 breaks = seq(as.Date(min(revisiones_barras$fecha)),
                              as.Date(max(revisiones_barras$fecha)), by = "year")) +
    theme_conasami()
}

# Federal: mes de la fecha de interés ; Local: mes del último dato disponible
mes_federal <- month(cfg$fecha_interes)
mes_local   <- month(max(revisiones_barras$fecha[revisiones_barras$juris == "Local"]))

combos <- tribble(
  ~juris,     ~var,           ~mes,
  "Federal",  "revisiones",   mes_federal,
  "Federal",  "trabajadores", mes_federal,
  "Local",    "revisiones",   mes_local,
  "Local",    "trabajadores", mes_local
)

for (i in seq_len(nrow(combos))) {
  juris_i <- combos$juris[i]; var_i <- combos$var[i]; mes_i <- combos$mes[i]

  df_i <- barras |> filter(juris == juris_i, variable == var_i, mes == mes_i)
  g_i  <- g_barra_juris(df_i)

  print(g_i)

  guardar_grafica_conasami(
    g_i,
    archivo     = paste0("barras_", var_i, "_", tolower(juris_i), "_",
                         format(cfg$fecha_interes, "%Ym%m")),
    dir         = here::here("graphs", "02_jurisdiccion"),
    tamano      = "medio"
    )
}

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

print(revisiones |> filter(fecha == cfg$fecha_interes)) 

print(revisiones |> filter(juris == "Local") |> filter(fecha == max(fecha)))







