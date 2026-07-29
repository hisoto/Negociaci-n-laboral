#_______________________________________________________________________________
#
# Master_negociaciones.R — Orquestador del pipeline de Negociación laboral
#
#   Corre el flujo mensual completo desde un solo lugar: extracción de tarjetas
#   CIS (opcional) → preflight de insumos → las nueve gráficas del informe.
#
#   Uso: editar el mes de interés abajo y ejecutar este archivo.
#
#     Rscript scripts/Master_negociaciones.R
#     # o, desde Positron:  source(here::here("scripts", "Master_negociaciones.R"))
#
#   El único parámetro que cambia cada mes es anio_interes / mes_interes; se
#   propaga a todos los scripts vía 000_config.R.
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
                                    # antes de graficar (pasos 007 y 008).

# ── configuración central ─────────────────────────────────────────────────────
# Publica fecha_interes, las fechas de inicio por familia y las rutas de insumos
# en options("negociaciones"). Todos los pasos leen de ahí, y como options()
# sobrevive a rm(list = ls()), cada script conserva su limpieza inicial.

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "000_config.R"))

# ── preflight ─────────────────────────────────────────────────────────────────
# Antes de generar nada, revisar que los insumos cubran el mes objetivo.

verificar_insumos()

# ── extracción de PDFs CIS (paso 001, Python) ─────────────────────────────────

if (correr_extraccion) {
  message("▶ 001_extraccion_pdf_cis.py (--incremental)")
  system2("python", c(shQuote(here::here("scripts", "001_extraccion_pdf_cis.py")),
                      "--incremental"))
} else {
  message("· 001_extraccion_pdf_cis.py omitido ",
          "(correr_extraccion = FALSE). Si llegaron tarjetas CIS nuevas:\n",
          "    python scripts/001_extraccion_pdf_cis.py --incremental")
}

# ── pasos de gráficas ─────────────────────────────────────────────────────────

pasos <- c(
  "002_graph_jurisdiccion.R",
  "003_graph_central.R",
  "004_graph_tipo_empresa.R",
  "005_mapa_incrementos.R",
  "006_graph_emplazamientos.R",
  "007_graph_huelgas.R",
  "008_mapa_huelgas_vigentes.R",
  "009_graph_mir.R",
  "010_graph_sectores.R"
)

# Cada script corre en su propio entorno, de modo que su rm(list = ls()) no
# alcance la configuración del master (dentro de un entorno sourceado, ls()
# resuelve a ese entorno). La config viaja por options(), no por variables, así
# que sobrevive de todas formas.
correr_paso <- function(paso) {
  message("\n▶ ", paso)
  t0 <- Sys.time()
  error <- NULL
  tryCatch(
    source(here::here("scripts", paso), local = new.env(), echo = FALSE),
    error = function(e) error <<- conditionMessage(e)
  )
  segundos <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (is.null(error)) {
    message("  OK  ", paso, "  (", segundos, "s)")
  } else {
    message("  ERROR  ", paso, "  (", segundos, "s): ", error)
  }
  data.frame(
    paso     = paso,
    ok       = is.null(error),
    segundos = segundos,
    error    = if (is.null(error)) NA_character_ else error,
    stringsAsFactors = FALSE
  )
}

resultados <- do.call(rbind, lapply(pasos, correr_paso))

# ── resumen ───────────────────────────────────────────────────────────────────

cat("\n────────────────────────────────────────────────────────────\n")
cat("Resumen ·", format(getOption("negociaciones")$fecha_interes, "%B %Y"), "\n\n")
for (i in seq_len(nrow(resultados))) {
  cat(sprintf("  %-6s %-30s %5.1fs\n",
              if (resultados$ok[i]) "OK" else "ERROR",
              resultados$paso[i],
              resultados$segundos[i]))
}
fallidos <- sum(!resultados$ok)
cat("\n", nrow(resultados) - fallidos, "de", nrow(resultados), "pasos completados\n")
if (fallidos > 0) {
  cat("\nDetalle de los errores:\n")
  for (i in which(!resultados$ok))
    cat("  ", resultados$paso[i], ": ", resultados$error[i], "\n", sep = "")
}
cat("────────────────────────────────────────────────────────────\n")
