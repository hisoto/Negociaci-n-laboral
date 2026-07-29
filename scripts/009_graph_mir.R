#_______________________________________________________________________________
#
# Objetivo: Gráficas del Monto Independiente de Recuperación (MIR)
#
#   Muestran cómo las revisiones salariales en la jurisdicción federal siguen el
#   aumento POR FIJACIÓN del salario mínimo y no el aumento TOTAL (que incluye el
#   MIR). Incluye incremento nominal y la variación anual del INPC de la
#   jurisdicción federal.
#
# Autor:  Héctor Iván Soto Parra
# Área:   Coordinación para el Análisis de la Economía Laboral (CAEL), CONASAMI
#
#_______________________________________________________________________________

rm(list = ls()); gc()
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

# ── variables de control ──────────────────────────────────────────────────────


# ── escala de series MIR (opción narrativa DT2026) ────────────────────────────
# Verde = incremento; dorado = referencia que sí se sigue (fijación);
# gris = referencia atenuada que NO se sigue (total con MIR); guinda = nominal.
mir_colores <- c(
  "Incremento nominal"                      = unname(conasami_colores[["guinda"]]),
  "Incremento real"                         = unname(conasami_colores[["verde"]]),
  "Variación anual del INPC"                = unname(conasami_colores[["verde"]]),
  "Aumento por fijación del Salario mínimo" = unname(conasami_colores[["dorado"]]),
  "Aumento total Salario mínimo"            = unname(conasami_colores[["gris"]])
)

mir_lineas <- c(
  "Incremento nominal"                      = "solid",
  "Incremento real"                         = "dashed",
  "Variación anual del INPC"                = "dashed",
  "Aumento por fijación del Salario mínimo" = "solid",
  "Aumento total Salario mínimo"            = "dotted"
)

# ══════════════════════════════════════════════════════════════════════════════
# 1. Jurisdicción federal
# ══════════════════════════════════════════════════════════════════════════════

# ── objeto federal (idéntico al de 001, sin el filtro >= 2021) ────────────────
federal <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_stata.xlsx"), sheet = "j_federal") |>
  clean_names() |>
  mutate(
    fecha = as.Date(fecha),
    year  = as.integer(year)
  ) |>
  filter(fecha >= cfg$fecha_inicio_mir & fecha <= cfg$fecha_interes)

# ── variación anual del INPC (comportamiento_precios) ─────────────────────────
# Fuente: proyecto hermano; serie nacional general (api == "v_inpc"). El CSV trae
# el nivel del índice, así que la variación interanual se calcula aquí.
ruta_inpc <- "../comportamiento_precios/data/inpc.csv"

inpc_var <- read_csv(ruta_inpc, show_col_types = FALSE) |>
  filter(api == "v_inpc") |>
  arrange(date) |>
  mutate(
    fecha    = floor_date(as.Date(date), "month"),
    inpc_var = (valor / lag(valor, 12) - 1) * 100
  ) |>
  select(fecha, inpc_var)

# Aumentos anuales del salario mínimo (escalonados por año)
tabla_federal <- federal |>
  select(fecha, year, mes, nominal) |>
  mutate(
    aumento_fijacion = case_when(
      year == 2017 ~ 4.1,
      year == 2018 ~ 4.1,
      year == 2019 ~ 5.5,
      year == 2020 ~ 5.7,
      year == 2021 ~ 6.5,
      year == 2022 ~ 10.1,
      year == 2023 ~ 10.9,
      year == 2024 ~ 6.8,
      year == 2025 ~ 6.8,
      year == 2026 ~ 6.9,
      TRUE ~ NA_real_
    ),
    aumento_total_sm = case_when(
      year == 2017 ~ 9.6,
      year == 2018 ~ 10.4,
      year == 2019 ~ 16.2,
      year == 2020 ~ 20.0,
      year == 2021 ~ 15.0,
      year == 2022 ~ 22.0,
      year == 2023 ~ 20.0,
      year == 2024 ~ 20.0,
      year == 2025 ~ 12.0,
      year == 2026 ~ 13.0,
      TRUE ~ NA_real_
    )
  ) |>
  # Unir la variación anual del INPC por mes (protegido con floor_date por si las
  # fechas del Excel no caen en día 01).
  mutate(fecha_mes = floor_date(fecha, "month")) |>
  left_join(inpc_var, by = c("fecha_mes" = "fecha")) |>
  select(-fecha_mes)

# Formato largo: color y linetype se mapean a la MISMA serie (leyenda unificada).
niveles_federal <- c(
  "Aumento por fijación del Salario mínimo",
  "Aumento total Salario mínimo",
  "Incremento nominal",
  "Variación anual del INPC"
)

federal_long <- tabla_federal |>
  pivot_longer(
    cols      = c(nominal, inpc_var, aumento_fijacion, aumento_total_sm),
    names_to  = "serie",
    values_to = "valor"
  ) |>
  mutate(
    serie = recode(serie,
      nominal          = "Incremento nominal",
      inpc_var         = "Variación anual del INPC",
      aumento_fijacion = "Aumento por fijación del Salario mínimo",
      aumento_total_sm = "Aumento total Salario mínimo"
    ),
    serie = factor(serie, levels = niveles_federal)
  ) |>
  filter(!is.na(valor))

# Cortes del eje x anclados en el año de corte y hacia atrás cada 2 años, para
# que el año de cfg$fecha_interes (p. ej. 2026) siempre aparezca en el eje.
breaks_federal <- seq(from = floor_date(cfg$fecha_interes, "year"),
                      to   = cfg$fecha_inicio_mir, by = "-2 years")

# ── área de trazado (sin título/subtítulo; van en la tabla-envoltorio de Word) ─
g_mir_federal <- ggplot(federal_long,
                        aes(x = fecha, y = valor,
                            color = serie, linetype = serie)) +
  geom_hline(
    yintercept = 0,
    color = conasami_neutros[["texto_secundario"]],
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  geom_line(linewidth = 0.75, lineend = "round", linejoin = "round") +
  scale_color_manual(values = mir_colores, name = NULL) +
  scale_linetype_manual(values = mir_lineas, name = NULL) +
  scale_x_date(date_labels = "%Y", breaks = breaks_federal) +
  guides(color = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) +
  theme_conasami()

g_mir_federal

guardar_grafica_conasami(
  g_mir_federal,
  archivo = paste0("mir_federal_", format(cfg$fecha_interes, "%Ym%m")),
  dir     = here::here("graphs", "09_mir"),
  tamano  = "ancho"
)

# ══════════════════════════════════════════════════════════════════════════════
# 2. Central obrera (facetada)
# ══════════════════════════════════════════════════════════════════════════════

leer_central <- function(sheet, etiqueta) {
  read_excel(file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx"), sheet = sheet) |>
    mutate(fecha = as.Date(fecha), central = etiqueta) |>
    rename(
      REAL         = paste0("REAL_", sheet),
      TRABAJADORES = paste0("TRABAJADORES_", sheet),
      REVISIONES   = paste0("REVISIONES_", sheet),
      NOMINAL      = paste0("NOMINAL_", sheet)
    )
}

centrales <- bind_rows(
  leer_central("ctm",     "CTM"),
  leer_central("croc",    "CROC"),
  leer_central("crom",    "CROM"),
  leer_central("sna_asa", "SNA y ASA"),
  leer_central("ind_ct",  "CT"),
  leer_central("otras",   "OTRAS")
) |>
  clean_names() |>
  mutate(year = as.integer(year)) |>
  filter(fecha >= cfg$fecha_inicio_mir & fecha <= cfg$fecha_interes) |>
  mutate(
    aumento_fijacion = case_when(
      year == 2017 ~ 3.9,
      year == 2018 ~ 5.0,
      year == 2019 ~ 5.0,
      year == 2020 ~ 5.0,
      year == 2021 ~ 6.0,
      year == 2022 ~ 9.0,
      year == 2023 ~ 10.0,
      year == 2024 ~ 6.0,
      year == 2025 ~ 6.5,
      year == 2026 ~ 6.5,
      TRUE ~ NA_real_
    )
  )

niveles_central <- c(
  "Aumento por fijación del Salario mínimo",
  "Incremento nominal",
  "Incremento real"
)

centrales_long <- centrales |>
  select(fecha, central, nominal, real, aumento_fijacion) |>
  pivot_longer(
    cols      = c(nominal, real, aumento_fijacion),
    names_to  = "serie",
    values_to = "valor"
  ) |>
  mutate(
    serie = recode(serie,
      nominal          = "Incremento nominal",
      real             = "Incremento real",
      aumento_fijacion = "Aumento por fijación del Salario mínimo"
    ),
    serie = factor(serie, levels = niveles_central)
  ) |>
  filter(!is.na(valor))

g_mir_central <- ggplot(centrales_long,
                        aes(x = fecha, y = valor,
                            color = serie, linetype = serie)) +
  geom_hline(
    yintercept = 0,
    color = conasami_neutros[["texto_secundario"]],
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  geom_line(linewidth = 0.6, lineend = "round", linejoin = "round") +
  scale_color_manual(values = mir_colores[niveles_central], name = NULL) +
  scale_linetype_manual(values = mir_lineas[niveles_central], name = NULL) +
  scale_x_date(date_labels = "%Y", date_breaks = "3 years") +
  facet_wrap(~central) +
  theme_conasami()

g_mir_central

guardar_grafica_conasami(
  g_mir_central,
  archivo = paste0("mir_central_", format(cfg$fecha_interes, "%Ym%m")),
  dir     = here::here("graphs", "09_mir"),
  tamano  = "libre",
  width   = 17.5,
  height  = 11
)
