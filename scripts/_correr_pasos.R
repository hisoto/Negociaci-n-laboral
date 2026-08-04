#_______________________________________________________________________________
#
# _correr_pasos.R — Motor compartido de los masters
#
#   Ejecuta el vector `pasos` (definido por el master que sourcea este archivo),
#   aislando cada script y capturando sus errores, y cierra con un resumen
#   OK/ERROR. Lo usan Master_negociaciones.R (flujo local completo) y
#   Master_informe_negociaciones.R (solo lo que va al informe).
#
#   Vive aquí, y no duplicado en cada master, para que los dos flujos no se
#   separen con el tiempo: el subconjunto de pasos es lo único que cambia.
#
#   El guion inicial del nombre lo mantiene fuera de la secuencia numerada: no
#   es un paso del pipeline, es infraestructura.
#
#_______________________________________________________________________________

if (!exists("pasos")) {
  stop("_correr_pasos.R requiere un vector `pasos` definido por el master.",
       call. = FALSE)
}

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
  cat(sprintf("  %-6s %-34s %5.1fs\n",
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
