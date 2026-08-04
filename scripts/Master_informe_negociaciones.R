#_______________________________________________________________________________
#
# Master_informe_negociaciones.R — Solo lo que va al informe mensual
#
#   Genera las 15 gráficas y las 2 tablas de la sección "Negociaciones
#   contractuales" del informe de la Dirección Técnica, y nada más.
#
#   Es el master que corre la copia de la carpeta compartida de la DT. La
#   diferencia con Master_negociaciones.R es que este NO ejecuta los scripts
#   9xx_extra_*, que producen material complementario fuera del informe
#   (mapa de huelgas vigentes, MIR, ranking de sectores).
#
#   Uso: editar el mes de interés abajo y ejecutar este archivo.
#
#     Rscript scripts/Master_informe_negociaciones.R
#     # o, desde Positron:
#     #   source(here::here("scripts", "Master_informe_negociaciones.R"))
#
# Autor: Héctor Iván Soto Parra
# Área:  Coordinación para el Análisis de la Economía Laboral (CAEL), CONASAMI
#
#_______________________________________________________________________________

rm(list = ls()); gc()

# ── parámetros del mes ────────────────────────────────────────────────────────
# ESTAS SON LAS ÚNICAS LÍNEAS QUE SE ACTUALIZAN CADA MES.

anio_interes <- 2026L
mes_interes  <- 6L                  # 1 = enero, ..., 12 = diciembre

correr_extraccion <- FALSE          # TRUE = reextraer las tarjetas CIS nuevas
                                    # antes de graficar (pasos 007 y 013).

# ── configuración central ─────────────────────────────────────────────────────

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "000_config.R"))

# ── preflight ─────────────────────────────────────────────────────────────────
# Antes de generar nada, revisar que los insumos cubran el mes objetivo.

verificar_insumos()

# ── extracción de PDFs CIS (paso 001, Python) ─────────────────────────────────

if (correr_extraccion) {
  source(here::here("scripts", "_correr_extraccion_cis.R"))
} else {
  message("· 001_extraccion_pdf_cis.py omitido ",
          "(correr_extraccion = FALSE). Si llegaron tarjetas CIS nuevas:\n",
          "    python scripts/001_extraccion_pdf_cis.py --incremental")
}

# ── pasos del informe ─────────────────────────────────────────────────────────
# Los 9xx_extra_* NO van aquí a propósito: ver Master_negociaciones.R para el
# flujo local completo.

pasos <- c(
  "002_graph_jurisdiccion.R",       # ts_juris_{federal,local} · barras_*_{federal,local}
  "003_graph_central.R",            # ts_centrales
  "004_graph_tipo_empresa.R",       # empresas
  "005_mapa_incrementos.R",         # mapa_incremento (+ mapa_solo_ y bar_)
  "006_graph_emplazamientos.R",     # mapa_emplazamientos (+ mapa_solo_)
  "007_graph_huelgas.R",            # bar_huelgas · bar_huelgas_causa
  "011_tabla_sectores.R",           # tabla SCIAN del mes
  "013_tabla_huelgas_vigentes.R"    # tabla del anexo estadístico
)

source(here::here("scripts", "_correr_pasos.R"))
