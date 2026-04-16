#_______________________________________________________________________________

# Objetivo: Hacer gráficas de revisiones salariales por tipo de empresa

# Autor: Héctor Iván Soto Parra 

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

# Datos ------------------------------------------------------------------------


revisiones <- read_excel("excels/negociaciones_stata.xlsx", sheet = "empresas") %>%
  clean_names() %>% 
  mutate(fecha = as.Date(fecha)) 

revisiones <- revisiones %>% 
  pivot_longer(
    cols = starts_with("real_")| starts_with("nominal_") | 
      starts_with("revisiones_") | starts_with("trabajadores_"),
    names_to = c("variable", "clase"),
    names_sep = "_"
  ) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  filter(fecha >= fecha_inicio & fecha <= fecha_interes) %>% 
  mutate(
    clase = case_when(
      clase == "priv" ~ "Privada",
      clase == "pub" ~ "Pública",
      TRUE ~ clase
    ),
    direc = ifelse(real >= 0, "Positivo", "Negativo")
  )

# serie de tiempo --------------------------------------------------------------

ggplot(revisiones) + 
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
  ))+ 
  geom_abline(
    slope = 0,
    intercept = 0,
    color = "black",
    size = 0.5,
    linetype = "dotted"
  ) +
  scale_x_date(date_labels = "%Y", 
               breaks = seq(as.Date(min(revisiones$fecha)), 
                            as.Date(max(revisiones$fecha)), 
                            by = "year")) +
  facet_wrap(~clase) + 
  geom_text(
    data = revisiones %>% filter(fecha == fecha_interes),
    mapping = aes(
      x = fecha,
      y = real,
      label = sprintf("%.2f", real),
      color = direc
    ),
    vjust = -3.5,
    hjust = 0.15,
    size = 6, 
    fontface = "bold",
    show.legend = FALSE
  ) +
  theme_conasami() + 
  labs(x = "", y = "Variación Salarial Real (%)", color = "") +
  theme(legend.position = "bottom",
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        strip.text = element_text(size = 20))


name <- paste0("graphs/tipo_empresa/empresas_", fecha_interes %>%  format("%Ym%m"), ".png")

ggsave(name,  plot = last_plot(), 
       width = 50, height = 20, units = "cm")

# tabla ------------------------------------------------------------------------

tabla_anual <- revisiones %>% 
  group_by(year, clase) %>% 
  summarise(
    revisiones = sum(revisiones),
    trabajadores = sum(trabajadores),
    real = mean(real),
    nominal = mean(nominal)
  )

print(tabla_anual)

print(revisiones %>% filter(fecha == fecha_interes))







