#_______________________________________________________________________________
# Héctor Iván Soto Parra
# Comisión Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral
# Mapa de huelgas federales vigentes (Cuadro 4 de las Tarjetas CIS)
#   - Snapshot del último fecha_reporte dentro del mes de fecha_interes
#   - Tamaño del punto = trabajadores
#   - Color del punto  = causa
#   - Etiqueta         = N. central obrera · trabajadores  (N por fecha_inicio asc)
#_______________________________________________________________________________

rm(list = ls()); gc()
options(scipen = 999)

source("rscrips/theme_conasami.R")

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

fecha_interes <- as.Date("2026-05-01")

ruta_externa  <- "C:/Users/ivan_/OneDrive - Comision Nacional de los Salarios Minimos/proyectosDT/informes/automatizacion"

# ── lectura de datos ───────────────────────────────────────────────────────────

huelgas_raw <- read_csv(
  "csvs/cuadro4_huelgas_vigentes_acumulado.csv",
  show_col_types = FALSE
) |>
  mutate(
    fecha_reporte = as.Date(fecha_reporte),
    trabajadores  = suppressWarnings(as.integer(trabajadores)),
    duracion_dias = suppressWarnings(as.integer(duracion_dias))
  )

centroides <- read_csv(
  "inputs/municipios_centroides.csv",
  show_col_types = FALSE,
  col_types = cols(cvegeo = "c", .default = col_guess())
)

catalogo_inegi <- read_csv(
  "catun_municipio/catun_municipio/AGEEML_202631880673_utf8.csv",
  show_col_types = FALSE,
  col_types = cols(.default = "c")
) |>
  transmute(
    cvegeo  = CVEGEO,
    nom_ent = NOM_ENT,
    nom_mun = NOM_MUN
  )

# ── snapshot: último fecha_reporte del mes objetivo ───────────────────────────

mes_target <- floor_date(fecha_interes, "month")

reportes_mes <- huelgas_raw |>
  filter(floor_date(fecha_reporte, "month") == mes_target) |>
  pull(fecha_reporte) |>
  unique() |>
  sort()

if (length(reportes_mes) == 0) {
  stop(
    "No hay fecha_reporte en el mes de ", format(mes_target, "%Y-%m"),
    ". Revisa fecha_interes o el CSV de huelgas vigentes."
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
  "Firma de contrato"          = "#611232",
  "Revisión de contrato"       = "#98989A",
  "Violación de contrato"      = "#e6d194",
  "Violación de contrato ley"  = "#a57f2c",
  "Revisión salarial"          = "#9b2247",
  "Reparto de utilidades"      = "#161a1d"
)

# ── mapa ──────────────────────────────────────────────────────────────────────

mapa <- ggplot() +
  geom_sf(data = mexico, fill = "white", color = "#222831", linewidth = 0.25) +
  geom_point(
    data = puntos,
    aes(x = lon, y = lat, size = trabajadores, color = causa_grp),
    alpha = 0.85
  ) +
  geom_text_repel(
    data = puntos,
    aes(x = lon, y = lat, label = label),
    parse              = TRUE,
    size               = 3.5,
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
    range  = c(2, 10),
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
    legend.title          = element_text(size = 12, face = "bold", color = "#161a1d"),
    legend.text           = element_text(size = 10, color = "#161a1d"),
    plot.margin           = margin(0.6, 0.6, 0.6, 0.6, "cm")
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

name <- paste0("graphs/huelgas/mapa_huelgas_vigentes_",
               format(fecha_interes, "%Ym%m"), ".png")

ggsave(name, plot = mapa, width = 30, height = 22, units = "cm", dpi = 300)
if (dir.exists(ruta_externa)) {
  file.copy(name, file.path(ruta_externa, "graphs", basename(name)), overwrite = TRUE)
}
ggsave(sub("\\.png$", ".svg", name), plot = mapa, width = 30, height = 22, units = "cm")

message("Mapa guardado en: ", name)
