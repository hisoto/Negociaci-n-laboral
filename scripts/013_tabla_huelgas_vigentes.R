#_______________________________________________________________________________
#
# 013_tabla_huelgas_vigentes.R — Tabla del anexo estadístico "Huelgas vigentes"
#
#   Produce el listado de huelgas federales vigentes que va en el anexo del
#   informe mensual: orden cronológico, empresa, entidad y municipio, sindicato,
#   central obrera, causa, fecha de inicio y personas trabajadoras. Hasta ahora
#   se transcribía a mano desde las tarjetas CIS.
#
#   Insumo:  inputs/cis_csv/cuadro4_huelgas_vigentes_acumulado.csv, que produce
#            001_extraccion_pdf_cis.py a partir de los PDFs semanales del CIS.
#            Se toma el ÚLTIMO fecha_reporte dentro del mes de cfg$fecha_interes
#            (mismo criterio que 900_extra_mapa_huelgas_vigentes.R), o el último
#            disponible si el corte cae ya en el mes siguiente — que es lo normal:
#            el informe de junio cita cifras "preliminares al 09 de julio".
#
#   Salida:  outputs/tablas/tabla_huelgas_vigentes_{YYYYmMM}.csv
#            + copia a bases/ de la carpeta compartida de la DT
#
#   Orden cronológico: numeración ascendente por fecha de inicio, igual que en el
#   informe. Las huelgas que abarcan más de una entidad no se numeran (N/A),
#   porque el anexo las deja fuera de la secuencia.
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
  readr,
  lubridate
)

# ── lectura ───────────────────────────────────────────────────────────────────

archivo_cis <- file.path(cfg$ruta_cis, "cuadro4_huelgas_vigentes_acumulado.csv")

if (!file.exists(archivo_cis)) {
  stop(
    "No existe ", archivo_cis, "\n\n",
    "Lo genera el paso 001. Si llegaron tarjetas CIS nuevas:\n",
    "    python scripts/001_extraccion_pdf_cis.py --incremental",
    call. = FALSE
  )
}

crudo <- read_csv(archivo_cis, show_col_types = FALSE) |>
  mutate(fecha_reporte = as.Date(fecha_reporte))

# ── elegir el corte ───────────────────────────────────────────────────────────
# Preferencia: el último reporte del mes de interés. Si no hay ninguno (porque el
# corte del mes se publica ya entrado el mes siguiente), se toma el último
# reporte disponible que no sea posterior al cierre del mes siguiente.

fin_mes      <- ceiling_date(cfg$fecha_interes, "month") - days(1)
limite_corte <- ceiling_date(cfg$fecha_interes %m+% months(1), "month") - days(1)

candidatos <- crudo$fecha_reporte[crudo$fecha_reporte <= limite_corte]

if (length(candidatos) == 0) {
  stop("No hay reportes CIS anteriores a ", format(limite_corte, "%d-%m-%Y"),
       ". El último disponible es ", format(max(crudo$fecha_reporte), "%d-%m-%Y"),
       ".", call. = FALSE)
}

corte <- max(candidatos)

if (corte <= fin_mes) {
  message("· Corte CIS: ", format(corte, "%d-%m-%Y"), " (dentro del mes)")
} else {
  message("· Corte CIS: ", format(corte, "%d-%m-%Y"),
          " (preliminar, ya en el mes siguiente — es lo habitual)")
}

# ── armado de la tabla ────────────────────────────────────────────────────────

vigentes <- crudo |>
  filter(fecha_reporte == corte) |>
  # Filas de control del extractor: sin empresa no hay registro que reportar.
  filter(!is.na(empresa), empresa != "", !is.na(fecha_inicio)) |>
  mutate(
    fecha_inicio = dmy(fecha_inicio),
    trabajadores = suppressWarnings(as.numeric(
      str_remove_all(as.character(trabajadores), "[^0-9]")
    )),
    entidad   = str_squish(coalesce(entidad, "")),
    municipio = str_squish(coalesce(municipio, "")),
    # La central viene con el marcador de nota al pie pegado y en posición
    # inconsistente ("1 Otras" y "Otras 1" conviven en el mismo corte). Se
    # separan para que la columna quede limpia y la nota se conserve aparte.
    central_nota = str_squish(str_extract(coalesce(central, ""), "\\d+")),
    central      = str_squish(str_remove_all(coalesce(central, ""), "\\d+")),
    # El anexo junta entidad y municipio en una sola columna.
    entidad_municipio = if_else(
      municipio == "" | is.na(municipio),
      entidad,
      paste0(entidad, ", ", municipio)
    ),
    multi_entidad = str_detect(str_to_lower(entidad), "más de una entidad")
  )

if (nrow(vigentes) == 0) {
  stop("El corte ", format(corte, "%d-%m-%Y"),
       " no dejó ninguna huelga vigente después de limpiar. Revisar el CSV.",
       call. = FALSE)
}

# Orden cronológico: 1 = la más antigua. Las de más de una entidad quedan fuera
# de la numeración, como en el informe.
# El desempate por empresa y causa hace la numeración reproducible: hay cortes
# con dos huelgas de la misma empresa iniciadas el mismo día (dos causas
# distintas), y sin desempate el folio dependía del orden de lectura del CSV.
tabla <- vigentes |>
  arrange(fecha_inicio, empresa, causa) |>
  mutate(
    orden_cronologico = if_else(multi_entidad, NA_integer_,
                                cumsum(!multi_entidad))
  ) |>
  arrange(desc(fecha_inicio), empresa, causa) |>
  transmute(
    periodo           = format(cfg$fecha_interes, "%Y-%m"),
    fecha_corte       = corte,
    orden_cronologico,
    empresa,
    entidad_municipio,
    sindicato,
    central,
    central_nota,
    causa,
    fecha_inicio,
    trabajadores
  )

message("· Huelgas vigentes al corte: ", nrow(tabla),
        "  ·  personas trabajadoras: ",
        format(sum(tabla$trabajadores, na.rm = TRUE), big.mark = ","))

sin_dato <- tabla |> filter(is.na(trabajadores))
if (nrow(sin_dato) > 0) {
  message("· ", nrow(sin_dato),
          " registro(s) sin número de personas trabajadoras legible:")
  for (e in sin_dato$empresa) message("    ", substr(e, 1, 60))
}

print(as.data.frame(
  tabla |> select(orden_cronologico, empresa, entidad_municipio,
                  central, causa, fecha_inicio, trabajadores)
), row.names = FALSE)

# ── salida ────────────────────────────────────────────────────────────────────

if (!dir.exists(cfg$ruta_tablas)) dir.create(cfg$ruta_tablas, recursive = TRUE)

archivo <- paste0("tabla_huelgas_vigentes_",
                  format(cfg$fecha_interes, "%Ym%m"), ".csv")
ruta_local <- file.path(cfg$ruta_tablas, archivo)

write_excel_csv(tabla, ruta_local)
message("· Escrito: ", ruta_local)

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
