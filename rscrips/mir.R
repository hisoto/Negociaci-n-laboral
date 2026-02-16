#_______________________________________________________________________________

# Objetivo: Gráficas sobre el MIR

# Autor: Héctor Iván Soto Parra 

# Fecha: enero 2026

#_______________________________________________________________________________

rm(list = ls());gc()

source("rscrips/theme_conasami.R")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor
)


# Incrementos nominales y Monto Independiente de Recuperación ------------------

tabla <- federal %>% 
  select(fecha, year, mes, nominal, real) %>% 
  mutate(
    aumento_fijacion = case_when(
      year == 2017 ~ 3.9, 
      year == 2018 ~ 5.0,
      year == 2019 ~ 5.0,
      year == 2020 ~ 5.0,
      year == 2021 ~ 6.0,
      year == 2022 ~ 9.0,
      year == 2023 ~ 10.0,
      year == 2024 ~ 6.0,
      year == 2025 ~ 6.5,
      TRUE ~ NA 
    ), 
    aumento_total_sm = case_when(
      year == 2017 ~ 9.6,
      year == 2018 ~ 10.4,
      year == 2019 ~ 16.2,
      year == 2020 ~ 20.0,
      year == 2021 ~ 15.0,
      year == 2022 ~ 22.0,
      year == 2023 ~ 20.0,
      year == 2024 ~ 20.0,
      year == 2025 ~ 12.0,
    )
  )

ggplot(tabla %>% filter(year >= 2006)) + 
  geom_line(aes(x = fecha, y = nominal, color = "Incremento nominal"), size = 1) + 
  geom_line(aes(x = fecha, y = aumento_fijacion, color = "Aumento por fijación del Salario mínimo"), size = 1) +
  geom_line(aes(x = fecha, y = real, color = "Incremento real"), linetype = "dashed", size = 1) +
  geom_line(aes(x = fecha, y = aumento_total_sm, color = "Aumento total Salario mínimo"), linetype = "dotted", size = 1) +
  geom_abline(slope = 0, intercept = 0, color = "black", size = 0.5, linetype = "dotted") +
  scale_color_manual(
    values = c(
      "Incremento nominal" = "blue",
      "Aumento por fijación del Salario mínimo" = "red",
      "Incremento real" = "green",
      "Aumento total Salario mínimo" = "purple"
    )
  ) +
  theme_conasami() +
  labs(
    title = "Incrementos nominales y aumentos por fijación del Salario mínimo",
    subtitle = "Revisiones salariales en la jurisdicción federal",
    x = "",
    y = "Variación (%)",
    color = ""
  ) +
  theme(legend.position = "bottom")

ggsave("graphs/grafica_revisiones_salariales_federal.png", width = 10, height = 6)

rm(tabla)

# central obrera


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

rm(central_ctm,
   central_croc,
   central_crom,
   central_sna_asa,
   central_otras,
   central_ind_ct)

centrales <- centrales %>% 
  mutate(
    aumento_fijacion = case_when(
      year == 2017 ~ 3.9, 
      year == 2018 ~ 5.0,
      year == 2019 ~ 5.0,
      year == 2020 ~ 5.0,
      year == 2021 ~ 6.0,
      year == 2022 ~ 9.0,
      year == 2023 ~ 10.0,
      year == 2024 ~ 6.0,
      year == 2025 ~ 6.5,
      TRUE ~ NA 
    ), 
    aumento_total_sm = case_when(
      year == 2017 ~ 9.6,
      year == 2018 ~ 10.4,
      year == 2019 ~ 16.2,
      year == 2020 ~ 20.0,
      year == 2021 ~ 15.0,
      year == 2022 ~ 22.0,
      year == 2023 ~ 20.0,
      year == 2024 ~ 20.0,
      year == 2025 ~ 12.0,
    )
  )

ggplot(centrales %>% filter(year >= 2012)) + 
  geom_line(aes(x = fecha, y = NOMINAL, color = "Incremento nominal"), size = 1) + 
  geom_line(aes(x = fecha, y = aumento_fijacion, color = "Aumento por fijación del Salario mínimo"), size = 1) +
  geom_line(aes(x = fecha, y = REAL, color = "Incremento real"), linetype = "dashed", size = 1) +
  geom_abline(slope = 0, intercept = 0, color = "black", size = 0.5, linetype = "dotted") +
  scale_color_manual(
    values = c(
      "Incremento nominal" = "blue",
      "Aumento por fijación del Salario mínimo" = "red",
      "Incremento real" = "green"
    )
  ) +
  facet_wrap(~central) + 
  theme_conasami() +
  labs(
    title = "Incrementos nominales y aumentos por fijación del Salario mínimo",
    subtitle = "Revisiones salariales en la jurisdicción federal por central obrera",
    x = "",
    y = "Porcentaje de aumento (%)",
    color = ""
  ) +
  theme(legend.position = "bottom")

ggsave("graphs/grafica_revisiones_salariales_federal_central.png", width = 12, height = 8)



