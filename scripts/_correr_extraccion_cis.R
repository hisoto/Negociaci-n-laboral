#_______________________________________________________________________________
#
# _correr_extraccion_cis.R — Lanza el paso 001 (Python) desde los masters
#
#   Extrae los cuadros 1–4 de las tarjetas semanales del CIS a los CSV de
#   inputs/cis_csv/. Lo sourcean los dos masters cuando correr_extraccion = TRUE.
#
#   Existe como archivo aparte porque encontrar el intérprete de Python no es
#   trivial en los equipos de la CAEL: en unos el ejecutable es `python`, en
#   otros solo está el lanzador `py` de Windows, y system2() no propaga el fallo
#   — devolvía 127 y el master lo daba por bueno. Aquí se busca un intérprete
#   utilizable y se revisa el código de salida.
#
#_______________________________________________________________________________

.script_py <- here::here("scripts", "001_extraccion_pdf_cis.py")

# Candidatos en orden de preferencia. El último es la ruta de la instalación de
# usuario que documenta el CLAUDE.md global, por si ninguno está en el PATH.
.candidatos_py <- c(
  Sys.getenv("CNSM_PYTHON"),
  "python",
  "py",
  file.path(Sys.getenv("LOCALAPPDATA"), "Programs", "Python", "Python312",
            "python.exe")
)
.candidatos_py <- .candidatos_py[nzchar(.candidatos_py)]

.usable <- function(exe) {
  salida <- suppressWarnings(
    tryCatch(
      system2(exe, "--version", stdout = TRUE, stderr = TRUE),
      error = function(e) NULL
    )
  )
  !is.null(salida) && !inherits(salida, "error") &&
    is.null(attr(salida, "status"))
}

.py <- NULL
for (cand in .candidatos_py) {
  if (.usable(cand)) { .py <- cand; break }
}

if (is.null(.py)) {
  message("· No se encontró un intérprete de Python utilizable. ",
          "Se omite la extracción de tarjetas CIS.\n",
          "    Probados: ", paste(.candidatos_py, collapse = ", "), "\n",
          "    Fija CNSM_PYTHON con la ruta del ejecutable, o corre a mano:\n",
          "      python scripts/001_extraccion_pdf_cis.py --incremental")
} else {
  message("▶ 001_extraccion_pdf_cis.py (--incremental) con ", .py)
  .codigo <- system2(.py, c(shQuote(.script_py), "--incremental"))
  if (identical(.codigo, 0L)) {
    message("  OK  001_extraccion_pdf_cis.py")
  } else {
    message("  ERROR  001_extraccion_pdf_cis.py terminó con código ", .codigo,
            ". Los CSV de inputs/cis_csv/ pueden haber quedado sin actualizar.")
  }
}

rm(.script_py, .candidatos_py, .usable, .py)
