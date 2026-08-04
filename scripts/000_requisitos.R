#_______________________________________________________________________________
#
# 000_requisitos.R — Preparar el equipo (correr UNA vez por computadora)
#
#   Instala los paquetes del pipeline y revisa las dos cosas que fallan en
#   silencio al cambiar de máquina: la tipografía institucional y la carpeta
#   compartida de la Dirección Técnica.
#
#     Rscript scripts/000_requisitos.R
#
#   No forma parte del flujo mensual: los masters no lo sourcean.
#
# Autor: Héctor Iván Soto Parra
# Área:  Coordinación para el Análisis de la Economía Laboral (CAEL), CONASAMI
#
#_______________________________________________________________________________

cat("\n== Requisitos · Negociación laboral ==\n\n")

ok    <- function(x) cat("  OK      ", x, "\n")
aviso <- function(x) cat("  AVISO   ", x, "\n")
falla <- function(x) cat("  FALTA   ", x, "\n")

# ── 1. R ──────────────────────────────────────────────────────────────────────

cat("1. Versión de R\n")
if (getRversion() >= "4.2.0") {
  ok(paste0("R ", getRversion(), " (el pipeline usa el pipe nativo |>)"))
} else {
  falla(paste0("R ", getRversion(),
               " — se necesita 4.2 o superior por el pipe nativo |>"))
}

# ── 2. Paquetes de CRAN ───────────────────────────────────────────────────────

cat("\n2. Paquetes de CRAN\n")
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

paquetes <- c(
  "here", "tidyverse", "readxl", "readr", "janitor", "lubridate", "scales",
  "ggrepel", "patchwork", "stringi", "sf", "rnaturalearth",
  "ragg", "systemfonts", "svglite"
)
pacman::p_load(char = paquetes)

faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) == 0) {
  ok(paste(length(paquetes), "paquetes disponibles"))
} else {
  falla(paste("no se pudieron instalar:", paste(faltan, collapse = ", ")))
}

# ── 3. rnaturalearthhires (NO está en CRAN) ───────────────────────────────────
# Lo usan 005, 006 y 900 para la geometría de entidades. pacman::p_load() falla
# con él porque no vive en CRAN, y ese fallo tumba los tres scripts de mapas.

cat("\n3. rnaturalearthhires (fuera de CRAN)\n")
if (requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  ok("instalado")
} else {
  aviso("no está; instalando desde r-universe (puede tardar)")
  try(install.packages("rnaturalearthhires",
                       repos = "https://ropensci.r-universe.dev",
                       type  = "source"), silent = TRUE)
  if (requireNamespace("rnaturalearthhires", quietly = TRUE)) ok("instalado")
  else falla(paste0("sigue sin instalarse. Los mapas (005, 006, 900) fallarán.\n",
                    "           install.packages('rnaturalearthhires', ",
                    "repos = 'https://ropensci.r-universe.dev', type = 'source')"))
}

# ── 4. Tipografía Noto Sans ───────────────────────────────────────────────────
# El theme la registra desde la carpeta de fuentes del usuario. Si no está,
# ggplot sustituye por otra y las gráficas salen fuera del canon DT 2026 con un
# simple warning que es fácil pasar por alto.

cat("\n4. Tipografía Noto Sans\n")
familias <- systemfonts::system_fonts()$family
if ("Noto Sans" %in% familias) {
  ok("Noto Sans disponible")
} else {
  falla(paste0("Noto Sans NO está instalada.\n",
               "           Las gráficas saldrán con otra tipografía y solo\n",
               "           avisan con un warning. Instalarla antes de publicar:\n",
               "           https://fonts.google.com/noto/specimen/Noto+Sans"))
}

# ── 5. Carpeta compartida de la DT ────────────────────────────────────────────

cat("\n5. Carpeta compartida de la Dirección Técnica\n")
source(here::here("scripts", "theme_conasami_dt2026.R"))
for (sub in c("graphs", "bases")) {
  ruta <- ruta_dt_automatizacion(sub)
  if (dir.exists(ruta)) ok(paste0(sub, "/  ", ruta))
  else aviso(paste0(sub, "/ no existe — se omitirán las copias externas\n",
                    "           ", ruta))
}

# ── 6. Python (solo para el paso 001, opcional) ───────────────────────────────

cat("\n6. Python (paso 001, extracción de tarjetas CIS)\n")
usable <- function(exe) {
  s <- suppressWarnings(tryCatch(
    system2(exe, "--version", stdout = TRUE, stderr = TRUE),
    error = function(e) NULL))
  !is.null(s) && is.null(attr(s, "status"))
}
py <- Filter(usable, c(Sys.getenv("CNSM_PYTHON"), "python", "py"))
if (length(py) > 0) {
  ok(paste0("intérprete disponible: ", py[1],
            "  (requiere pandas y pdfplumber)"))
} else {
  aviso(paste0("sin intérprete de Python en el PATH. Solo hace falta para\n",
               "           reextraer las tarjetas CIS; el resto del pipeline corre igual."))
}

cat("\n== Fin ==\n\n")
