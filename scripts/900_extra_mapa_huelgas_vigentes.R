#_______________________________________________________________________________
# Héctor Iván Soto Parra
# Comisión Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral
# Mapa de huelgas federales vigentes (Cuadro 4 de las Tarjetas CIS)
#   - Snapshot del último fecha_reporte dentro del mes de cfg$fecha_interes
#   - Tamaño del punto = trabajadores
#   - Color del punto  = causa
#   - Etiqueta         = N. central obrera · trabajadores  (N por fecha_inicio asc)
#
# EXTRA (prefijo 9xx): no forma parte del informe mensual de la DT — el informe
# lleva la TABLA del anexo (013_tabla_huelgas_vigentes.R), no este mapa. Lo corre
# Master_negociaciones.R (flujo local completo), NO
# Master_informe_negociaciones.R, y no viaja en la copia de la carpeta
# compartida. Sigue escribiendo en graphs/08_huelgas_vigentes/ para no partir el
# histórico.
#_______________________________________________________________________________

rm(list = ls()); gc()
source(here::here("scripts", "theme_conasami_dt2026.R"))

# Configuración (Master_negociaciones.R la define; fallback si se corre suelto)
if (is.null(getOption("negociaciones"))) source(here::here("scripts", "000_config.R"))
cfg <- getOption("negociaciones")

pacman::p_load(
  tidyverse,
  lubridate,
  readr,
  janitor,
  stringi,
  ggrepel,
  sf,
  rnaturalearth,
  rnaturalearthhires,
  patchwork
)


# Tamaño de etiqueta de datos: 8 pt (manual §11.1) → mm, igual que la referencia.
lab_size <- 8 / .pt

# ── lectura de datos ───────────────────────────────────────────────────────────

huelgas_raw <- read_csv(
  file.path(cfg$ruta_cis, "cuadro4_huelgas_vigentes_acumulado.csv"),
  show_col_types = FALSE
) |>
  mutate(
    fecha_reporte = as.Date(fecha_reporte),
    trabajadores  = suppressWarnings(as.integer(trabajadores)),
    duracion_dias = suppressWarnings(as.integer(duracion_dias))
  )

centroides <- read_csv(
  file.path(cfg$ruta_catalogos, "municipios_centroides.csv"),
  show_col_types = FALSE,
  col_types = cols(cvegeo = "c", .default = col_guess())
)

catalogo_inegi <- read_csv(
  file.path(cfg$ruta_catalogos, "catun_municipio", "AGEEML_202631880673_utf8.csv"),
  show_col_types = FALSE,
  col_types = cols(.default = "c")
) |>
  transmute(
    cvegeo  = CVEGEO,
    nom_ent = NOM_ENT,
    nom_mun = NOM_MUN
  )

# ── snapshot: último fecha_reporte del mes objetivo ───────────────────────────

mes_target <- floor_date(cfg$fecha_interes, "month")

reportes_mes <- huelgas_raw |>
  filter(floor_date(fecha_reporte, "month") == mes_target) |>
  pull(fecha_reporte) |>
  unique() |>
  sort()

if (length(reportes_mes) == 0) {
  stop(
    "No hay fecha_reporte en el mes de ", format(mes_target, "%Y-%m"),
    ". Revisa cfg$fecha_interes o el CSV de huelgas vigentes."
  )
}

fecha_snapshot <- max(reportes_mes)
message("Snapshot tomado al fecha_reporte: ", fecha_snapshot,
        " (de ", length(reportes_mes), " corte(s) disponibles en el mes).")

snapshot <- huelgas_raw |>
  filter(fecha_reporte == fecha_snapshot) |>
  filter(!is.na(empresa), str_detect(empresa, "[A-Za-z]"))

# ── separar multi-entidad para nota al pie ─────────────────────────────────────

multi_entidad <- snapshot |>
  filter(entidad %in% c("Más de una entidad", "Más de una"))

snapshot_geo <- snapshot |>
  filter(!entidad %in% c("Más de una entidad", "Más de una"),
         !is.na(municipio), str_squish(municipio) != "")

# ── resolución por CVEGEO: huelgas → catálogo INEGI → centroides ──────────────

normalize_txt <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_replace_all("(?<=\\b[A-Za-z]) (?=[A-Za-z]\\b)", "") |>
    str_to_lower() |>
    str_squish()
}

estado_aliases <- c(
  "Coahuila"  = "Coahuila de Zaragoza",
  "Michoacán" = "Michoacán de Ocampo",
  "Veracruz"  = "Veracruz de Ignacio de la Llave"
)

municipio_aliases <- c(
  "Campeche|Ciudad del Carmen"       = "Carmen",
  "Chiapas|Ocozocuautla de Espinoza" = "Ocozocoautla de Espinosa",
  "Jalisco|Tlaquepaque"              = "San Pedro Tlaquepaque",
  "Quintana Roo|Cancún"              = "Benito Juárez",
  "Quintana Roo|Solidaridad"         = "Playa del Carmen",
  "Tabasco|Villahermosa"             = "Centro"
)

apply_aliases <- function(df) {
  df |>
    mutate(
      entidad_cat   = coalesce(estado_aliases[entidad], entidad),
      municipio_cat = coalesce(
        municipio_aliases[paste(entidad, municipio, sep = "|")],
        municipio
      )
    )
}

catalogo_lookup <- catalogo_inegi |>
  mutate(
    entidad_norm   = normalize_txt(nom_ent),
    municipio_norm = normalize_txt(nom_mun)
  ) |>
  select(cvegeo, entidad_norm, municipio_norm)

snapshot_geo <- snapshot_geo |>
  apply_aliases() |>
  mutate(
    entidad_norm   = normalize_txt(entidad_cat),
    municipio_norm = normalize_txt(municipio_cat)
  ) |>
  left_join(catalogo_lookup, by = c("entidad_norm", "municipio_norm")) |>
  left_join(centroides |> select(cvegeo, lon, lat), by = "cvegeo")

sin_cvegeo    <- snapshot_geo |> filter(is.na(cvegeo))
sin_centroide <- snapshot_geo |> filter(!is.na(cvegeo), is.na(lon) | is.na(lat))
puntos        <- snapshot_geo |> filter(!is.na(lon), !is.na(lat))

if (nrow(sin_cvegeo) > 0) {
  message("Huelgas sin CVEGEO (nombre no resuelve en catálogo INEGI):")
  sin_cvegeo |>
    distinct(entidad, municipio) |>
    pmap(\(entidad, municipio) message("  · ", entidad, " — ", municipio))
}

if (nrow(sin_centroide) > 0) {
  message("Huelgas con CVEGEO pero sin centroide ",
          "(agregar a inputs/municipios_centroides.csv):")
  sin_centroide |>
    distinct(cvegeo, entidad, municipio) |>
    pmap(\(cvegeo, entidad, municipio)
         message("  · ", cvegeo, "  ", entidad, " — ", municipio))
}

# ── etiquetas: central obrera numerada por fecha_inicio ──────────────────────

puntos <- puntos |>
  mutate(fecha_inicio_d = dmy(fecha_inicio)) |>
  arrange(fecha_inicio_d) |>
  mutate(
    n_label      = row_number(),
    trabajadores = replace_na(trabajadores, 0L),
    central_lbl  = central |>
      str_squish() |>
      str_remove("^\\d+\\s+") |>
      str_remove("\\s+\\d+$"),
    central_lbl  = if_else(is.na(central_lbl) | central_lbl == "",
                           "Otras", central_lbl),
    label        = paste0(
      'bold("', n_label, '.")*" ',
      str_replace_all(central_lbl, '"', "'"),
      ' · ', trabajadores, '"'
    ),
    causa_grp    = case_when(
      str_detect(causa, regex("contrato ley",          ignore_case = TRUE)) ~ "Violación de contrato ley",
      str_detect(causa, regex("firma",                 ignore_case = TRUE)) ~ "Firma de contrato",
      str_detect(causa, regex("revisi.n salarial",     ignore_case = TRUE)) ~ "Revisión salarial",
      str_detect(causa, regex("revisi.n de contrato",  ignore_case = TRUE)) ~ "Revisión de contrato",
      str_detect(causa, regex("violaci.n de contrato", ignore_case = TRUE)) ~ "Violación de contrato",
      str_detect(causa, regex("reparto",               ignore_case = TRUE)) ~ "Reparto de utilidades",
      TRUE                                                                   ~ NA_character_
    )
  )

# ── geometría base: México por entidad ────────────────────────────────────────

mexico <- ne_states(country = "Mexico", returnclass = "sf")
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)

# ── paleta CONASAMI para causa ────────────────────────────────────────────────

causa_palette <- c(
  "Firma de contrato"          = "#611232",  # guinda profundo
  "Revisión de contrato"       = "#98989A",  # gris
  "Violación de contrato"      = "#E6D194",  # arena
  "Violación de contrato ley"  = "#A57F2C",  # dorado
  "Revisión salarial"          = "#9B2247",  # guinda institucional
  "Reparto de utilidades"      = "#161A1D"   # tinta
)

# ── mapa ──────────────────────────────────────────────────────────────────────

mapa <- ggplot() +
  geom_sf(data = mexico, fill = "white", color = conasami_neutros[["eje_base"]], linewidth = 0.25) +
  geom_point(
    data = puntos,
    aes(x = lon, y = lat, size = trabajadores, color = causa_grp),
    alpha = 0.85
  ) +
  geom_text_repel(
    data = puntos,
    aes(x = lon, y = lat, label = label),
    parse              = TRUE,
    size               = lab_size,
    family             = "Noto Sans",
    color              = "#161a1d",
    bg.color           = "white",
    bg.r               = 0.12,
    min.segment.length = 0,
    segment.color      = "#161a1d",
    segment.size       = 0.25,
    box.padding        = 0.55,
    point.padding      = 0.35,
    force              = 4,
    force_pull         = 0.5,
    max.overlaps       = Inf,
    seed               = 1
  ) +
  scale_color_manual(values = causa_palette, name = "Causa") +
  scale_size_continuous(
    name   = "Trabajadores",
    range  = c(1.5, 6),
    breaks = c(50, 250, 500, 1000, 2000)
  ) +
  coord_sf(datum = NA) +
  labs(
    title    = NULL,
    subtitle = NULL,
    caption  = NULL,
    x = NULL, y = NULL
  ) +
  theme_void() +
  theme(
    text                  = element_text(family = "Noto Sans", color = "#161a1d"),
    plot.background       = element_rect(fill = "transparent", color = NA),
    panel.background      = element_rect(fill = "transparent", color = NA),
    legend.background     = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    legend.key            = element_rect(fill = "transparent", color = NA),
    legend.position       = "bottom",
    legend.box            = "horizontal",
    legend.box.just       = "center",
    legend.title          = element_text(family = "Noto Sans", size = 9, face = "bold", color = "#161a1d"),
    legend.text           = element_text(family = "Noto Sans", size = 9, color = "#161a1d"),
    plot.margin           = margin(0.3, 0.3, 0.3, 0.3, "cm")
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      nrow           = 1,
      override.aes   = list(size = 4)
    ),
    size  = guide_legend(
      title.position = "top",
      nrow           = 1
    )
  )

print(mapa)

# ── guardar ───────────────────────────────────────────────────────────────────

ruta_guardada <- guardar_grafica_conasami(
  mapa,
  archivo     = paste0("mapa_huelgas_vigentes_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "08_huelgas_vigentes"),
  tamano      = "ancho"
)

message("Mapa guardado en: ", ruta_guardada)
