#_______________________________________________________________________________
#
# Objetivo: Gráficas del Monto Independiente de Recuperación (MIR)
#
#   Muestran cómo las revisiones salariales en la jurisdicción federal siguen el
#   aumento POR FIJACIÓN del salario mínimo y no el aumento TOTAL (que incluye el
#   MIR). Incluye incremento nominal y la variación anual del INPC de la
#   jurisdicción federal.
#
#   EXTRA (prefijo 9xx): no forma parte del informe mensual de la DT. Lo corre
#   Master_negociaciones.R (flujo local completo), NO
#   Master_informe_negociaciones.R, y no viaja en la copia de la carpeta
#   compartida. Sigue escribiendo en graphs/09_mir/ para no partir el histórico.
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

# ── aumentos anuales del salario mínimo ───────────────────────────────────────
# Vienen de inputs/salario_minimo_aumentos.csv, no de un case_when en el código:
# cada 1 de enero se agrega una fila al CSV y no se toca este script. Son tres
# series distintas — la fijación de la jurisdicción federal, la de las centrales
# obreras y el aumento total (fijación + MIR).
aumentos_sm <- read_csv(cfg$ruta_sm, show_col_types = FALSE)

# ── variación anual del INPC (comportamiento_precios) ─────────────────────────
# Serie nacional general (api == "v_inpc"). El CSV trae el nivel del índice, así
# que la variación interanual se calcula aquí. cfg$ruta_inpc apunta primero a
# bases/ de la DT y, si no está, al proyecto hermano (ver 000_config.R).
if (is.na(cfg$ruta_inpc)) {
  stop(
    "No se encontró inpc.csv.\n",
    "Corre antes el pipeline de comportamiento_precios (deja el archivo en\n",
    "bases/ de la DT), o coloca ese proyecto como carpeta hermana de este.",
    call. = FALSE
  )
}

inpc_var <- read_csv(cfg$ruta_inpc, show_col_types = FALSE) |>
  filter(api == "v_inpc") |>
  arrange(date) |>
  mutate(
    fecha    = floor_date(as.Date(date), "month"),
    inpc_var = (valor / lag(valor, 12) - 1) * 100
  ) |>
  select(fecha, inpc_var)

tabla_federal <- federal |>
  select(fecha, year, mes, nominal) |>
  left_join(
    aumentos_sm |>
      select(year = anio,
             aumento_fijacion = aumento_fijacion_federal,
             aumento_total_sm),
    by = "year"
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
  # Ojo: la fijación que se compara con las centrales NO es la misma serie que la
  # de la jurisdicción federal de arriba (p. ej. 2019: 5.0 vs 5.5).
  left_join(
    aumentos_sm |>
      select(year = anio, aumento_fijacion = aumento_fijacion_central),
    by = "year"
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
