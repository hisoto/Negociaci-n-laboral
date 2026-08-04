#_______________________________________________________________________________
#
# 011_tabla_sectores.R — Tabla de negociaciones por sector de actividad (SCIAN)
#
#   Produce la tabla "Revisiones salariales · Incremento real y negociaciones por
#   sector de actividad económica" de la sección de Negociaciones contractuales
#   del informe mensual. Hasta ahora se armaba a mano desde el Excel.
#
#   Insumo:  inputs/stps_scian/negociaciones_scian_{mes} {año}.xlsx, hoja
#            "Cuadro (2)". La ruta la resuelve ruta_scian_mes() de 000_config.R,
#            que fecha el archivo por su NOMBRE: el título interno del Excel no
#            es confiable (el de junio 2026 dice "MARZO 2024").
#
#   Salida:  outputs/tablas/tabla_sectores_{YYYYmMM}.csv
#            + copia a bases/ de la carpeta compartida de la DT
#
# Autor: Héctor Iván Soto Parra
# Área:  Coordinación para el Análisis de la Economía Laboral (CAEL), CONASAMI
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
  readr
)

# ── localizar el archivo del mes ──────────────────────────────────────────────

archivo_scian <- ruta_scian_mes(cfg$fecha_interes)

if (is.na(archivo_scian)) {
  stop(
    "No hay archivo SCIAN para ", format(cfg$fecha_interes, "%B %Y"), " en\n",
    "    ", cfg$ruta_scian, "\n\n",
    "El nombre esperado es 'negociaciones_scian_{mes} {año}.xlsx' (mes en\n",
    "palabra, minúsculas). Ver inputs/README.md para la descarga.",
    call. = FALSE
  )
}

message("· SCIAN del mes: ", basename(archivo_scian))

# ── lectura ───────────────────────────────────────────────────────────────────
# Estructura de la hoja: fila 1 título, fila 2 encabezados, fila 3 Total,
# filas 4+ los sectores (ya vienen ordenados de mayor a menor incremento real).
#
# Se leen sin encabezado y se nombran las columnas a mano: los dos "%" repetidos
# hacen que el repair automático de readxl genere nombres distintos según la
# versión, y no conviene depender de eso.

columnas <- c("sector", "incremento_real", "personas", "personas_pct",
              "negociaciones", "negociaciones_pct")

crudo <- read_excel(
  archivo_scian,
  sheet      = "Cuadro (2)",
  skip       = 2,
  col_names  = columnas,
  col_types  = "text",
  .name_repair = "minimal"
) |>
  filter(!is.na(sector), sector != "")

# ── limpieza ──────────────────────────────────────────────────────────────────
# El Excel de la STPS a veces trae texto donde debería haber un número: en junio
# 2026 el "Número de negociaciones" del primer sector venía como "Sa" en vez de 2.
# Cuando eso pasa, el conteo se reconstruye desde el porcentaje, que sí es
# numérico, usando el total de la primera fila. Si no se puede, queda NA — nunca
# se inventa un valor ni se arrastra el de la fila vecina.

a_numero <- function(x) suppressWarnings(as.numeric(x))

total <- crudo |> slice(1)
total_negociaciones <- a_numero(total$negociaciones)
total_personas      <- a_numero(total$personas)

tabla <- crudo |>
  mutate(
    across(c(incremento_real, personas, personas_pct, negociaciones_pct), a_numero),
    negociaciones_txt = negociaciones,
    negociaciones     = a_numero(negociaciones),
    # Reconstrucción desde el porcentaje cuando el conteo no es numérico.
    negociaciones_reconstruido = is.na(negociaciones) &
      !is.na(negociaciones_pct) & !is.na(total_negociaciones),
    negociaciones = if_else(
      negociaciones_reconstruido,
      round(negociaciones_pct / 100 * total_negociaciones),
      negociaciones
    ),
    es_total = row_number() == 1
  ) |>
  select(sector, incremento_real, personas, personas_pct,
         negociaciones, negociaciones_pct,
         es_total, negociaciones_reconstruido, negociaciones_txt)

reconstruidas <- tabla |> filter(negociaciones_reconstruido)
if (nrow(reconstruidas) > 0) {
  message("· Conteos no numéricos en el Excel, reconstruidos desde el %:")
  for (i in seq_len(nrow(reconstruidas))) {
    message("    '", reconstruidas$negociaciones_txt[i], "' -> ",
            reconstruidas$negociaciones[i], "  (",
            substr(reconstruidas$sector[i], 1, 45), ")")
  }
}

# ── control de integridad ─────────────────────────────────────────────────────
# La suma de los sectores debe cuadrar con la fila Total. Si no cuadra, es que la
# hoja cambió de estructura (filas de más, columnas corridas) y hay que mirarla
# antes de publicar nada.

sectores <- tabla |> filter(!es_total)
suma_personas      <- sum(sectores$personas, na.rm = TRUE)
suma_negociaciones <- sum(sectores$negociaciones, na.rm = TRUE)

cuadra <- function(a, b) !is.na(a) && !is.na(b) && abs(a - b) <= 1

if (!cuadra(suma_personas, total_personas) ||
    !cuadra(suma_negociaciones, total_negociaciones)) {
  warning(
    "La suma de los sectores no cuadra con el Total del Excel:\n",
    "  personas:      ", suma_personas,      " vs ", total_personas,      "\n",
    "  negociaciones: ", suma_negociaciones, " vs ", total_negociaciones, "\n",
    "Revisar la hoja 'Cuadro (2)' antes de usar la tabla.",
    call. = FALSE
  )
} else {
  message("· Integridad OK — los sectores suman el Total (",
          total_negociaciones, " negociaciones · ",
          format(total_personas, big.mark = ","), " personas)")
}

# ── salida ────────────────────────────────────────────────────────────────────

salida <- tabla |>
  mutate(
    periodo = format(cfg$fecha_interes, "%Y-%m"),
    orden   = if_else(es_total, "Total", "Sector")
  ) |>
  select(periodo, orden, sector, incremento_real, personas, personas_pct,
         negociaciones, negociaciones_pct)

# La fila Total va al final, como en el informe.
salida <- bind_rows(
  salida |> filter(orden == "Sector"),
  salida |> filter(orden == "Total")
)

print(as.data.frame(salida), row.names = FALSE)

if (!dir.exists(cfg$ruta_tablas)) dir.create(cfg$ruta_tablas, recursive = TRUE)

archivo <- paste0("tabla_sectores_", format(cfg$fecha_interes, "%Ym%m"), ".csv")
ruta_local <- file.path(cfg$ruta_tablas, archivo)

write_excel_csv(salida, ruta_local)
message("· Escrito: ", ruta_local)

# Copia a la carpeta compartida de la DT. Mismo criterio que el resto del
# pipeline: si la carpeta no existe se avisa y se sigue, nunca en silencio.
# cfg$dest_bases vale NA cuando se apagó la copia (options(cnsm_copiar_dt=FALSE)).
if (is.na(cfg$dest_bases)) {
  message("· Copia a la DT desactivada; la tabla se queda en outputs/tablas/")
} else if (dir.exists(cfg$dest_bases)) {
  if (file.copy(ruta_local, file.path(cfg$dest_bases, archivo), overwrite = TRUE)) {
    message("· Copiado a la DT: ", file.path(cfg$dest_bases, archivo))
  } else {
    message("· No se pudo copiar a la DT: ", file.path(cfg$dest_bases, archivo))
  }
} else {
  message("· Carpeta destino no encontrada, se omite la copia: ", cfg$dest_bases)
}
