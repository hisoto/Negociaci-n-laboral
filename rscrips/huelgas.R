#_______________________________________________________________________________

# Objetivo: Hacer gráficas de huelgas estalladas y vigentes. 

#_______________________________________________________________________________

rm(list = ls());gc()

source("rscrips/theme_conasami.R")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  lubridate
)

fecha_inicio <- as.Date("2021-01-01")

fecha_interes <- as.Date("2026-01-01")

# Datos ------------------------------------------------------------------------

huelgas <- read_excel("excels/HUELGAS.xlsx") |> 
  rename(
    fecha = 1, 
    Estalladas = 2, 
    Vigentes = 3
  ) |> 
  mutate(
    across(everything(), as.integer)
  ) |> 
  pivot_longer(
    cols = -fecha,
    names_to = "tipo",
    values_to = "valor"
  )

# Gráfica 
   
ggplot(huelgas) +
  geom_col(
    mapping = aes(
      x = fecha, 
      y = valor, 
      color = tipo,
      fill = tipo
    ), 
    position = position_dodge(width = 1),
  ) +
  geom_text(
    mapping = aes(
      x = fecha, 
      y = valor,
      label = ifelse(valor == 0, NA, scales::label_number(scale = 1)(valor)),
      hjust = if_else(tipo == "Estalladas", 1.5, -1)
    ),
    position = position_dodge(width = 1),
    vjust = -0.5,
    size = 5,
    fontface = "bold",
    show.legend = FALSE,
    color = "#161a1d"
  ) + 
  scale_fill_manual(values = c(
    "Estalladas" = "#1e5b4f",
    "Vigentes" = "#611232"
  )) +
  scale_color_manual(values = c(
    "Estalladas" = "#1e5b4f",
    "Vigentes" = "#611232"
  )) +
  scale_x_continuous(
    breaks = seq(min(huelgas$fecha),
                 max(huelgas$fecha),
                 by = 1)
   ) +
  scale_y_continuous(labels = scales::label_number(scale = 1),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    color = "",
    fill = "",
    x = "",
    y = ""
  ) + 
  theme_conasami() +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 20, angle = 90),
    axis.text.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    strip.text = element_text(size = 20)
  )

name <- paste0("graphs/huelgas/bar_huelgas_",
               fecha_interes |> format("%Ym%m"),
               ".png")

ggsave(
  name,
  plot = last_plot(),
  width = 50,
  height = 20,
  units = "cm"
)

rm(huelgas)

# Gráfica de huelgas por tipo de conflicto

huelgas_causa <- read_excel("excels/Libro1.xlsx", sheet = "df") %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  mutate(huelgas = 1) %>% 
  mutate(year = year(`Fecha de inicio`)) %>% 
  group_by(Causa, year) %>% 
  summarise(sum(huelgas))

glimpse(huelgas_causa) 

labels_causa <- unique(huelgas_causa$Causa)

ggplot(huelgas_causa) + 
  geom_col(
    mapping = aes(x = factor(year), y = `sum(huelgas)` , fill = Causa),
    stat = "identity", 
    position = "stack") + 
  scale_fill_manual(
    values = c("#611232", "#98989A","#e6d194", "#a57f2c", "#9b2247",  "#161a1d"), labels = labels_causa) + 
  theme_conasami() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 15), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 15)) + 
#  guides(fill = guide_legend(nrow = 2, col = 3)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    fill = "", 
    color = ""
  ) 


name_h_2 <- paste0("graphs/huelgas/bar_huelgas_causa_", fecha_interes %>%  format("%Ym%m"), ".png")

ggsave(
  name_h_2,
  plot = last_plot(), 
  width = 30, 
  height = 15, 
  units = "cm")

