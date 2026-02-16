#_______________________________________________________________________________
# Héctor Iván Soto Parra
# 08 de febrero de 2025
# Comisiòn Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral
# Gráficas de línea y de barras para la evolución de las negociaciones salariales por central obrera
#_______________________________________________________________________________

rm(list = ls());gc()

source("rscrips/theme_conasami.R")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  lubridate,
  janitor
)

fecha_inicio <- as.Date("2021-01-01")

fecha_interes <- as.Date("2026-01-01")

#_______________________________________________________________________________

# Iniciamos importando los datos de excel y creando un df en formato long para las centrales obreras.

central_ctm <- read_excel("excels/negociaciones_central.xlsx", sheet = "ctm") %>%
  mutate(fecha = as.Date(fecha), central = "CTM") %>%
  rename(
    REAL = REAL_ctm,
    TRABAJADORES = TRABAJADORES_ctm,
    REVISIONES = REVISIONES_ctm,
    NOMINAL = NOMINAL_ctm
  )

central_croc <- read_excel("excels/negociaciones_central.xlsx", sheet = "croc") %>%
  mutate(fecha = as.Date(fecha), central = "CROC") %>%
  rename(
    REAL = REAL_croc,
    TRABAJADORES = TRABAJADORES_croc,
    REVISIONES = REVISIONES_croc,
    NOMINAL = NOMINAL_croc
  )

central_crom <- read_excel("excels/negociaciones_central.xlsx", sheet = "crom") %>%
  mutate(fecha = as.Date(fecha), central = "CROM") %>%
  rename(
    REAL = REAL_crom,
    TRABAJADORES = TRABAJADORES_crom,
    REVISIONES = REVISIONES_crom,
    NOMINAL = NOMINAL_crom
  )

central_sna_asa <- read_excel("excels/negociaciones_central.xlsx", sheet = "sna_asa") %>%
  mutate(fecha = as.Date(fecha), central = "SNA y ASA") %>%
  rename(
    REAL = REAL_sna_asa,
    TRABAJADORES = TRABAJADORES_sna_asa,
    REVISIONES = REVISIONES_sna_asa,
    NOMINAL = NOMINAL_sna_asa
  )

central_otras <- read_excel("excels/negociaciones_central.xlsx", sheet = "otras") %>%
  mutate(fecha = as.Date(fecha), central = "OTRAS") %>%
  rename(
    REAL = REAL_otras,
    TRABAJADORES = TRABAJADORES_otras,
    REVISIONES = REVISIONES_otras,
    NOMINAL = NOMINAL_otras
  )

central_ind_ct <- read_excel("excels/negociaciones_central.xlsx", sheet = "ind_ct") %>%
  mutate(fecha = as.Date(fecha), central = "CT") %>%
  rename(
    REAL = REAL_ind_ct,
    TRABAJADORES = TRABAJADORES_ind_ct,
    REVISIONES = REVISIONES_ind_ct,
    NOMINAL = NOMINAL_ind_ct
  )


centrales <- bind_rows(
  central_ctm,
  central_croc,
  central_crom,
  central_sna_asa,
  central_ind_ct,
  central_otras
)

rm(
  central_ctm,
  central_croc,
  central_crom,
  central_sna_asa,
  central_ind_ct,
  central_otras
)

centrales <- centrales %>%
  filter(fecha >= fecha_inicio & fecha <= fecha_interes) %>% 
  clean_names() %>% 
  mutate(
    direc = ifelse(real >= 0, "Positivo", "Negativo")
  )

#_______________________________________________________________________________

etiquetas <- centrales %>%
  group_by(central, fecha) %>%
  summarise(
    x = min(fecha, na.rm = TRUE),
    etiqueta = paste0(
      "Promedio: ",
      round(real,2),
      "%"
    ),
    .groups = "drop"
  ) |> 
  filter(
    fecha == fecha_interes
  ) 

ggplot(centrales) +
  geom_point(
    mapping = aes(x = fecha, y = real, color = direc),
    show.legend = FALSE
  ) + 
  geom_line(mapping = aes(x = fecha, y = real),
            color = "#611232") +
  geom_bar(
    mapping = aes(x = fecha, y = real, fill = direc),
    stat = "identity",
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c(
    "Positivo" = "#1e5b4f",
    "Negativo" = "#611232"
  )) +
  scale_color_manual(values = c(
    "Positivo" = "#1e5b4f",
    "Negativo" = "#611232"
  )) + 
  geom_abline(
    slope = 0,
    intercept = 0,
    color = "black",
    size = 0.5,
    linetype = "dotted"
  ) +
  geom_text(
    data = etiquetas,
    aes(
      x = x,
      y = Inf,
      label = etiqueta
    ),
    hjust = 2.8,
    vjust = 1.5,
    size = 6,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_x_date(date_labels = "%Y",
                 breaks = seq(as.Date(min(centrales$fecha)),
                              as.Date(max(centrales$fecha)),
                              by = "year")) +
  facet_wrap(~central) +
  theme_conasami() + 
  labs(x = "", y = "Variación Salarial Real (%)", color = "") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text = element_text(size = 20))

name_centrales <- paste0("graphs/central_obrera/ts_centrales_",
                         fecha_interes %>% format("%Ym%m"),
                         ".png")

ggsave(
  name_centrales,
  plot = last_plot(),
  width = 50,
  height = 20,
  units = "cm"
)
# tabla ------------------------------------------------------------------------

tabla <- centrales %>% 
  group_by(year, central) %>% 
  summarise(
    revisiones = sum(revisiones),
    trabajadores = sum(trabajadores),
    real = mean(real),
    nominal = mean(nominal)
  ) %>% 
  arrange(desc(year))

print(tabla)

print(centrales %>% filter(fecha == fecha_interes) %>% relocate(fecha, central, real))

