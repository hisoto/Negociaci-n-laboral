#_______________________________________________________________________________
#
# 000_config.R — Configuración central del pipeline de Negociación laboral
#
#   Fuente única de la fecha de interés, las fechas de inicio por familia de
#   gráficas y las rutas de insumos. Todos los scripts numerados leen de aquí.
#
#   Mecanismo: los parámetros se publican con options(negociaciones = list(...)).
#   A diferencia de una variable ordinaria, options() NO se borra con
#   rm(list = ls()), así que cada script puede conservar su rm() al inicio y
#   seguir leyendo la config con getOption("negociaciones").
#
#   Dos formas de uso:
#     1. Desde Master_negociaciones.R → el Master define anio_interes/mes_interes
#        y luego sourcea este archivo; los valores del Master mandan.
#     2. Script suelto → si no encuentra la opción, sourcea este archivo, que cae
#        en los valores por defecto de abajo.
#
#   EL MES CANÓNICO SE EDITA EN Master_negociaciones.R, no aquí.
#
# Autor: Héctor Iván Soto Parra
# Área:  Coordinación para el Análisis de la Economía Laboral (CAEL), CONASAMI
#
#_______________________________________________________________________________

options(scipen = 999)

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(here, readxl, readr, dplyr, stringr, lubridate)

# ── verificación del ancla ────────────────────────────────────────────────────
# here::here() se ancla en el WORKING DIRECTORY, no en la ubicación del script:
# busca hacia arriba desde el wd hasta el primer .Rproj / .git. Correr desde
# cualquier subcarpeta del proyecto funciona; correr desde fuera, no.
#
# Si el wd está fuera, lo normal es que el propio source() de este archivo falle
# primero con "no fue posible abrir el archivo .../scripts/000_config.R", que ya
# nombra la ruta equivocada. Esta guarda cubre el caso en que sí se llega hasta
# aquí (por ejemplo, si el wd cae en otro proyecto que también tiene
# scripts/000_config.R) y evita que se escriban gráficas en el proyecto vecino.

if (!file.exists(here::here("Negociacion_laboral.Rproj"))) {
  stop(
    "here::here() resolvió a:\n    ", here::here(), "\n",
    "que no es la raíz de Negociación laboral.\n\n",
    "Correr desde la raíz del proyecto:\n",
    "    cd '<...>/Informes/Negociación laboral'\n",
    "    Rscript scripts/Master_negociaciones.R\n",
    "o abrir Negociacion_laboral.Rproj en Positron/RStudio.",
    call. = FALSE
  )
}

# El theme trae theme_conasami(), las escalas institucionales,
# guardar_grafica_conasami() y ruta_dt_automatizacion().
source(here::here("scripts", "theme_conasami_dt2026.R"))

# ── mes de interés ────────────────────────────────────────────────────────────
# El mes canónico se edita en Master_negociaciones.R. Estos defaults solo aplican
# cuando se corre un script suelto sin pasar antes por el Master.


# Un orquestador global (Master_CAEL_informe.R, en la raíz de CAEL) fija el mes
# para los cuatro proyectos del informe con estas variables de entorno. Tienen
# prioridad sobre lo escrito en el master: si están, es que alguien está corriendo
# los cuatro proyectos juntos y el periodo debe ser el mismo en todos.
if (nzchar(Sys.getenv("CNSM_ANIO"))) anio_interes <- as.integer(Sys.getenv("CNSM_ANIO"))
if (nzchar(Sys.getenv("CNSM_MES")))  mes_interes  <- as.integer(Sys.getenv("CNSM_MES"))
if (!exists("anio_interes")) anio_interes <- 2026L
if (!exists("mes_interes"))  mes_interes  <- 6L

fecha_interes <- as.Date(sprintf("%04d-%02d-01", anio_interes, mes_interes))

# ── inicio de la serie por familia de gráficas ────────────────────────────────
# Cada familia tiene su propio arranque porque el Manual DT 2026 pide ~6 años de
# datos en las gráficas de ancho completo y ~3 en las de medio ancho.

fecha_inicio_series   <- as.Date("2021-01-01")  # 002 series de jurisdicción
fecha_inicio_barras   <- as.Date("2023-01-01")  # 002 barras anuales
fecha_inicio_central  <- as.Date("2023-01-01")  # 003 centrales obreras
fecha_inicio_empresas <- as.Date("2023-01-01")  # 004 tipo de empresa
fecha_inicio_mapas    <- as.Date("2017-01-01")  # 005 entidades · 006 emplazamientos
fecha_inicio_huelgas  <- as.Date("2007-01-01")  # 007 serie anual de huelgas
fecha_inicio_mir      <- as.Date("2012-01-01")  # 901 MIR (historia larga)
fecha_inicio_sectores <- as.Date("2021-01-01")  # 902 sectores

# ── rutas ─────────────────────────────────────────────────────────────────────
# ruta_dt_automatizacion() resuelve la raíz de OneDrive desde el entorno, así que
# no se toca al cambiar de equipo.

dest_graphs <- ruta_dt_automatizacion("graphs")
dest_bases  <- ruta_dt_automatizacion("bases")

# ── interruptor de copia a la carpeta compartida ──────────────────────────────
# proyectosDT es carpeta compartida de la Dirección Técnica. Para pruebas, para
# correr un mes viejo o para verificar portabilidad fuera del árbol CAEL, se
# apaga la copia externa sin tocar el código:
#
#   options(cnsm_copiar_dt = FALSE)   # antes de sourcear 000_config.R
#   Sys.setenv(CNSM_COPIAR_DT = "false")
#
# Por defecto está encendida: el flujo mensual normal sí vuelca a la DT.
copiar_dt <- getOption(
  "cnsm_copiar_dt",
  !identical(tolower(Sys.getenv("CNSM_COPIAR_DT", "true")), "false")
)

if (!isTRUE(copiar_dt)) {
  message("· Copia a proyectosDT DESACTIVADA (cnsm_copiar_dt = FALSE). ",
          "Las salidas se quedan en el proyecto.")
  dest_graphs <- NULL
  dest_bases  <- NA_character_
}

# Fijar la opción evita repetir `dest = ...` en cada llamada a
# guardar_grafica_conasami().
options(cnsm_dest_graphs = dest_graphs)

# ── INPC (insumo del proyecto hermano comportamiento_precios) ─────────────────
# Lo consume 901_extra_mir.R para calcular la variación anual.
#
# Orden de búsqueda:
#   1. bases/ de la DT — comportamiento_precios deja ahí inpc.csv cada mes. Es la
#      fuente preferida porque no depende de dónde viva este proyecto.
#   2. Proyecto hermano en el árbol CAEL, para quien trabaja en local sin haber
#      corrido el pipeline de precios.
#
# Antes era la ruta relativa "../comportamiento_precios/data/inpc.csv", que se
# resolvía contra el WORKING DIRECTORY (no contra el ancla del .Rproj) y obligaba
# a que el proyecto hermano estuviera al lado. Devuelve NA_character_ si no
# aparece en ningún lado; el script que la use decide qué hacer.
ruta_inpc <- local({
  candidatas <- c(
    ruta_dt_automatizacion("bases", "inpc.csv"),
    here::here("..", "comportamiento_precios", "data", "inpc.csv")
  )
  encontrada <- candidatas[file.exists(candidatas)]
  if (length(encontrada) == 0) NA_character_ else encontrada[1]
})

# ── dispositivo gráfico en sesiones no interactivas ───────────────────────────
# Bajo Rscript el dispositivo por defecto es pdf(), que no conoce "Noto Sans" y
# aborta con "invalid font type" en cuanto un script hace print() de una gráfica
# (además de dejar un Rplots.pdf en la raíz). ragg resuelve fuentes vía
# systemfonts, igual que guardar_grafica_conasami(), así que los print() de
# revisión funcionan igual desde el master que desde Positron.

if (!interactive()) {
  options(device = function(...) {
    ragg::agg_png(filename = file.path(tempdir(), "preview_%03d.png"), ...)
  })
}

# ── publicar configuración ────────────────────────────────────────────────────

options(negociaciones = list(
  anio          = as.integer(anio_interes),
  mes           = as.integer(mes_interes),
  fecha_interes = fecha_interes,

  fecha_inicio_series   = fecha_inicio_series,
  fecha_inicio_barras   = fecha_inicio_barras,
  fecha_inicio_central  = fecha_inicio_central,
  fecha_inicio_empresas = fecha_inicio_empresas,
  fecha_inicio_mapas    = fecha_inicio_mapas,
  fecha_inicio_huelgas  = fecha_inicio_huelgas,
  fecha_inicio_mir      = fecha_inicio_mir,
  fecha_inicio_sectores = fecha_inicio_sectores,

  ruta_poligonos = here::here("inputs", "stps_poligonos"),
  ruta_scian     = here::here("inputs", "stps_scian"),
  ruta_cis       = here::here("inputs", "cis_csv"),
  ruta_catalogos = here::here("inputs", "catalogos"),
  ruta_outputs   = here::here("outputs"),
  ruta_tablas    = here::here("outputs", "tablas"),
  ruta_sm        = here::here("inputs", "salario_minimo_aumentos.csv"),
  ruta_inpc      = ruta_inpc,

  dest_graphs = dest_graphs,
  dest_bases  = dest_bases
))

message("Config del pipeline lista — mes de interés: ",
        format(fecha_interes, "%Y-%m"))

# ── meses en español ──────────────────────────────────────────────────────────
# Los archivos SCIAN vienen nombrados con el mes en palabra ("junio 2026"), y el
# encabezado interno del Excel no es confiable: el de junio 2026 dice "MARZO
# 2024". El nombre del archivo es la única fuente para fechar ese insumo.

meses_es <- c(enero = 1, febrero = 2, marzo = 3, abril = 4, mayo = 5, junio = 6,
              julio = 7, agosto = 8, septiembre = 9, octubre = 10,
              noviembre = 11, diciembre = 12)

#' Ruta del archivo SCIAN correspondiente a un mes
#'
#' Tolera el espaciado inconsistente de los nombres ("junio  2025" con dos
#' espacios convive con "junio 2026" con uno).
ruta_scian_mes <- function(fecha = getOption("negociaciones")$fecha_interes) {
  mes_palabra <- names(meses_es)[month(fecha)]
  patron      <- paste0("negociaciones_scian_", mes_palabra, "\\s+", year(fecha), "\\.xlsx$")
  archivos    <- list.files(getOption("negociaciones")$ruta_scian,
                            pattern = patron, full.names = TRUE)
  if (length(archivos) == 0) return(NA_character_)
  archivos[1]
}

# ── preflight de insumos ──────────────────────────────────────────────────────

#' Última fecha disponible en cada insumo del pipeline
#'
#' Imprime una tabla con la cobertura de cada archivo frente a `fecha_interes`.
#' Correrlo ANTES de generar gráficas evita el caso silencioso de publicar un mes
#' con el Excel del mes anterior.
#'
#' El rezago de la jurisdicción local y del SCIAN es estructural (la STPS los
#' publica con meses de retraso), así que se marcan como "rezago normal" y no
#' como error.
verificar_insumos <- function(fecha = getOption("negociaciones")$fecha_interes) {

  cfg    <- getOption("negociaciones")
  seguro <- function(expr) tryCatch(suppressWarnings(expr), error = function(e) NA)

  f_poligonos <- file.path(cfg$ruta_poligonos, "negociaciones_stata.xlsx")
  f_central   <- file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx")
  f_emplaza   <- file.path(cfg$ruta_poligonos,
                           "5.2.5 Emplazamientos a Huelgas por Entidad Federativa.xlsx")
  f_cis       <- file.path(cfg$ruta_cis, "cuadro4_huelgas_vigentes_acumulado.csv")
  f_scian     <- ruta_scian_mes(fecha)

  max_hoja <- function(archivo, hoja) {
    seguro(max(as.Date(read_excel(archivo, sheet = hoja)$fecha), na.rm = TRUE))
  }

  filas <- list(
    list("negociaciones_stata / j_federal", "002 · 901",
         max_hoja(f_poligonos, "j_federal"), "estricto"),
    list("negociaciones_stata / j_local",   "002",
         max_hoja(f_poligonos, "j_local"), "rezago"),
    list("negociaciones_stata / entidad",   "005",
         max_hoja(f_poligonos, "entidad"), "estricto"),
    list("negociaciones_stata / empresas",  "004",
         max_hoja(f_poligonos, "empresas"), "estricto"),
    list("negociaciones_stata / huelgas",   "007",
         seguro(as.Date(paste0(max(read_excel(f_poligonos, sheet = "huelgas")$fecha,
                                   na.rm = TRUE), "-12-01"))), "anual"),
    list("negociaciones_central / ctm",     "003 · 901",
         max_hoja(f_central, "ctm"), "estricto"),
    list("5.2.5 Emplazamientos",            "006",
         seguro(as.Date(file.info(f_emplaza)$mtime)), "archivo"),
    list("cuadro4 huelgas vigentes (CIS)",  "007 · 013 · 900",
         seguro(max(as.Date(read_csv(f_cis, show_col_types = FALSE)$fecha_reporte),
                    na.rm = TRUE)), "estricto"),
    list("SCIAN del mes",                   "011 · 902",
         if (is.na(f_scian)) NA else seguro(as.Date(file.info(f_scian)$mtime)), "rezago"),
    # Aumentos del salario mínimo: se agrega una fila cada 1 de enero. La regla
    # "anual" basta — lo que importa es que el año en curso ya esté capturado.
    list("salario_minimo_aumentos.csv",     "901",
         seguro(as.Date(sprintf("%d-12-01",
                                max(read_csv(cfg$ruta_sm,
                                             show_col_types = FALSE)$anio,
                                    na.rm = TRUE)))), "anual"),
    list("inpc.csv (comportamiento_precios)", "901",
         if (is.na(cfg$ruta_inpc)) NA
         else seguro(max(as.Date(read_csv(cfg$ruta_inpc,
                                          show_col_types = FALSE)$date),
                         na.rm = TRUE)), "estricto")
  )

  cat("\n── Cobertura de insumos · mes objetivo:",
      format(fecha, "%B %Y"), "──\n\n")

  for (f in filas) {
    nombre <- f[[1]]; scripts <- f[[2]]; ultima <- f[[3]]; regla <- f[[4]]

    estado <- if (is.na(ultima)) {
      "FALTA / ilegible"
    } else if (regla == "archivo") {
      paste("archivo del", format(ultima, "%d-%m-%Y"))
    } else if (regla == "anual") {
      if (year(ultima) >= year(fecha)) "OK" else "DESACTUALIZADO"
    } else if (ultima >= fecha) {
      "OK"
    } else if (regla == "rezago") {
      paste0("rezago normal (", format(ultima, "%m-%Y"), ")")
    } else {
      paste0("DESACTUALIZADO (", format(ultima, "%m-%Y"), ")")
    }

    cat(sprintf("  %-36s %-16s %s\n", nombre, scripts, estado))
  }

  if (is.na(f_scian))
    cat("\n  Nota: no hay archivo SCIAN para", format(fecha, "%B %Y"),
        "en inputs/stps_scian/\n")

  if (is.na(cfg$ruta_inpc))
    cat("\n  Nota: no se encontró inpc.csv. 901_extra_mir.R lo necesita.\n",
        "        Corre antes el pipeline de comportamiento_precios (deja el\n",
        "        archivo en bases/ de la DT), o coloca ese proyecto como\n",
        "        carpeta hermana de este.\n", sep = "")

  cat("\n")
  invisible(NULL)
}
