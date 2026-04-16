rm(list = ls()); gc()
options(scipen = 999)
source("rscrips/theme_conasami.R")
pacman::p_load(tidyverse, lubridate)

fecha_interes <- as.Date("2026-03-01")
ruta_externa  <- "C:/Users/ivan_/OneDrive - Comision Nacional de los Salarios Minimos/proyectosDT/informes/automatizacion"

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

huelgas_causa <- huelgas_clean |>
  group_by(causa_norm, year) |>
  summarise(n = n(), .groups = "drop")

# ── Orden y colores ──────────────────────────────────────────────────────────

colores_causa <- c(
  "Firma de contrato"          = "#611232",
  "Revisión de contrato"       = "#98989A",
  "Violación de contrato"      = "#e6d194",
  "Violación de contrato ley"  = "#a57f2c",
  "Revisión salarial"          = "#9b2247",
  "Reparto de utilidades"      = "#161a1d"
)

# ── Gráfica ──────────────────────────────────────────────────────────────────

ggplot(huelgas_causa) +
  geom_bar(
    aes(x = factor(year), y = n, fill = causa_norm),
    stat = "identity",
    position = "stack"
  ) +
  scale_fill_manual(values = colores_causa) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_conasami() +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(hjust = 0.5, size = 20),
    plot.subtitle    = element_text(hjust = 0.5),
    axis.title.y     = element_text(size = 20),
    legend.text      = element_text(size = 15),
    axis.text.x      = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y      = element_text(angle = 0, hjust = 1, size = 15)
  ) +
  guides(fill = guide_legend(nrow = 2, ncol = 3)) +
  labs(
    title    = NULL,
    subtitle = NULL,
    x        = NULL,
    y        = NULL,
    fill     = "",
    color    = ""
  )

# ── Guardar ──────────────────────────────────────────────────────────────────

name_out <- paste0("graphs/huelgas/bar_huelgas_causa_",
                   fecha_interes |> format("%Ym%m"), ".png")

ggsave(name_out, plot = last_plot(),
       width = 30, height = 15, units = "cm")

file.copy(name_out, file.path(ruta_externa, "graphs", basename(name_out)), overwrite = TRUE)

cat("Guardado:", name_out, "\n")
