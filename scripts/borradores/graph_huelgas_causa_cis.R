rm(list = ls()); gc()
options(scipen = 999)
source("rscrips/theme_conasami_dt2026.R")
pacman::p_load(tidyverse, lubridate)

fecha_interes <- as.Date("2026-06-01")
ruta_externa  <- ruta_dt_automatizacion()

# ── Leer datos extraídos de PDFs CIS ─────────────────────────────────────────

huelgas_raw <- read_csv("csvs/cuadro4_huelgas_vigentes_acumulado.csv",
                        col_types = cols(.default = "c")) |>
  filter(fecha_reporte == max(fecha_reporte)) |>
  filter(!is.na(fecha_inicio) & fecha_inicio != "") |>
  mutate(fecha_inicio_d = dmy(fecha_inicio)) |>
  filter(!is.na(fecha_inicio_d)) |>
  mutate(year = year(fecha_inicio_d))

# ── Normalizar causas ────────────────────────────────────────────────────────

huelgas_clean <- huelgas_raw |>
  mutate(causa_norm = case_when(
    str_detect(causa, regex("contrato ley", ignore_case = TRUE))  ~ "Violación de contrato ley",
    str_detect(causa, regex("firma", ignore_case = TRUE))         ~ "Firma de contrato",
    str_detect(causa, regex("revisi.n salarial", ignore_case = TRUE)) ~ "Revisión salarial",
    str_detect(causa, regex("revisi.n de contrato", ignore_case = TRUE)) ~ "Revisión de contrato",
    str_detect(causa, regex("violaci.n de contrato", ignore_case = TRUE)) ~ "Violación de contrato",
    str_detect(causa, regex("reparto", ignore_case = TRUE))       ~ "Reparto de utilidades",
    TRUE ~ causa
  ))

# ── Agrupar por causa y año ──────────────────────────────────────────────────
# Orden de causas por frecuencia total descendente: fija los niveles del factor
# para que scale_fill_conasami() asigne un color estable a cada causa mes a mes.

orden_causas <- huelgas_clean |>
  count(causa_norm, sort = TRUE) |>
  pull(causa_norm)

huelgas_causa <- huelgas_clean |>
  group_by(causa_norm, year) |>
  summarise(n = n(), .groups = "drop") |>
  mutate(causa_norm = factor(causa_norm, levels = orden_causas))

# ── Gráfica ──────────────────────────────────────────────────────────────────
# Área de trazado únicamente (Manual DT 2026, §11): el título/detalles/fuente se
# arman en Word. Paleta categórica institucional (orden fijo del Manual) vía
# scale_fill_conasami().

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

# ── Guardar ──────────────────────────────────────────────────────────────────

guardar_grafica_conasami(
  g_huelgas_causa,
  archivo     = paste0("bar_huelgas_causa_", format(fecha_interes, "%Ym%m")),
  dir         = "graphs/huelgas",
  tamano      = "ancho",
  dest        = file.path(ruta_externa, "graphs")
)
