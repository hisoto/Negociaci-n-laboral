#_______________________________________________________________________________
#
# Objetivo: las dos gráficas de huelgas de la sección "Huelgas" del informe.
#
#   1. bar_huelgas_*        "Huelgas · Por año de inicio"
#                           Estalladas vs. vigentes, serie anual desde 2007.
#   2. bar_huelgas_causa_*  "Huelgas · Vigentes por causa"
#                           Apiladas por causa y año de inicio.
#
#   Fusiona los dos scripts que antes competían: `huelgas.R` (que leía el
#   HUELGAS.xlsx de llenado manual) y `graph_huelgas_causa_cis.R`. Ambos escribían
#   `bar_huelgas_causa_{fecha}`, así que el resultado dependía de cuál se corriera
#   al final. Ahora hay un solo productor de cada archivo.
#
#   La serie anual ya no sale del llenado manual: la hoja `huelgas` del archivo
#   maestro de Polígonos trae exactamente las mismas cifras 2007-2026, más el
#   desglose por causa. La gráfica por causa sale del acumulado de las Tarjetas
#   CIS, que es el insumo correcto (registro nominal caso por caso).
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
  janitor,
  lubridate
)

# ══════════════════════════════════════════════════════════════════════════════
# 1. Huelgas por año de inicio: estalladas vs. vigentes
# ══════════════════════════════════════════════════════════════════════════════

# La hoja trae `fecha` como año entero (no como fecha), más el desglose por causa
# que aquí no se usa.
huelgas <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_stata.xlsx"),
                      sheet = "huelgas") |>
  select(
    fecha,
    Estalladas = huelgas,
    Vigentes   = huelgas_vigentes
  ) |>
  mutate(across(everything(), as.integer)) |>
  filter(fecha >= year(cfg$fecha_inicio_huelgas)) |>
  pivot_longer(
    cols      = -fecha,
    names_to  = "tipo",
    values_to = "valor"
  )

g_huelgas <- ggplot(huelgas) +
  geom_col(
    mapping = aes(
      x    = fecha,
      y    = valor,
      fill = tipo
    ),
    position = position_dodge(width = 1)
  ) +
  geom_text(
    mapping = aes(
      x     = fecha,
      y     = valor,
      label = ifelse(valor == 0, NA, scales::label_number(scale = 1)(valor)),
      group = tipo
    ),
    position    = position_dodge(width = 1),
    vjust       = -0.5,
    size        = 3,
    family      = "Noto Sans",
    show.legend = FALSE,
    color       = conasami_neutros[["texto_secundario"]]
  ) +
  scale_fill_conasami() +
  labs(fill = NULL) +
  scale_x_continuous(
    breaks = seq(min(huelgas$fecha), max(huelgas$fecha), by = 1)
  ) +
  scale_y_continuous(
    labels = scales::label_number(scale = 1),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_conasami() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

g_huelgas

guardar_grafica_conasami(
  g_huelgas,
  archivo = paste0("bar_huelgas_", format(cfg$fecha_interes, "%Ym%m")),
  dir     = here::here("graphs", "07_huelgas"),
  tamano  = "ancho"
)

# ══════════════════════════════════════════════════════════════════════════════
# 2. Huelgas vigentes por causa
# ══════════════════════════════════════════════════════════════════════════════

# Snapshot del último corte disponible del Cuadro 4 de las Tarjetas CIS.
huelgas_raw <- read_csv(
  file.path(cfg$ruta_cis, "cuadro4_huelgas_vigentes_acumulado.csv"),
  col_types = cols(.default = "c")
) |>
  filter(fecha_reporte == max(fecha_reporte)) |>
  filter(!is.na(fecha_inicio) & fecha_inicio != "") |>
  mutate(fecha_inicio_d = dmy(fecha_inicio)) |>
  filter(!is.na(fecha_inicio_d)) |>
  mutate(year = year(fecha_inicio_d))

# ── normalizar causas ─────────────────────────────────────────────────────────
# El texto de la causa varía entre tarjetas ("Revisión Salarial", "revisión
# salarial", "Rev. salarial"), así que se colapsa a las seis categorías del
# informe antes de contar.

huelgas_clean <- huelgas_raw |>
  mutate(causa_norm = case_when(
    str_detect(causa, regex("contrato ley", ignore_case = TRUE))         ~ "Violación de contrato ley",
    str_detect(causa, regex("firma", ignore_case = TRUE))                ~ "Firma de contrato",
    str_detect(causa, regex("revisi.n salarial", ignore_case = TRUE))    ~ "Revisión salarial",
    str_detect(causa, regex("revisi.n de contrato", ignore_case = TRUE)) ~ "Revisión de contrato",
    str_detect(causa, regex("violaci.n de contrato", ignore_case = TRUE))~ "Violación de contrato",
    str_detect(causa, regex("reparto", ignore_case = TRUE))              ~ "Reparto de utilidades",
    TRUE ~ causa
  ))

# ── agrupar por causa y año ───────────────────────────────────────────────────
# Orden de causas por frecuencia total descendente: fija los niveles del factor
# para que scale_fill_conasami() asigne un color estable a cada causa mes a mes.

orden_causas <- huelgas_clean |>
  count(causa_norm, sort = TRUE) |>
  pull(causa_norm)

huelgas_causa <- huelgas_clean |>
  group_by(causa_norm, year) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(causa_norm = factor(causa_norm, levels = orden_causas))

# ── gráfica ───────────────────────────────────────────────────────────────────
# Área de trazado únicamente (Manual DT 2026, §11): el título, los detalles y la
# fuente se arman en la tabla-envoltorio de Word.

g_huelgas_causa <- ggplot(huelgas_causa) +
  geom_col(
    aes(x = factor(year), y = n, fill = causa_norm),
    position = "stack"
  ) +
  scale_fill_conasami() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_conasami() +
  guides(fill = guide_legend(nrow = 2, ncol = 3)) +
  labs(fill = "")

g_huelgas_causa

guardar_grafica_conasami(
  g_huelgas_causa,
  archivo = paste0("bar_huelgas_causa_", format(cfg$fecha_interes, "%Ym%m")),
  dir     = here::here("graphs", "07_huelgas"),
  tamano  = "ancho"
)
