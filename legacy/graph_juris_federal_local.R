#_______________________________________________________________________________
# Héctor Iván Soto Parra 
# 08 de febrero de 2025
# Comisiòn Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral 
# Gráficas de línea y de barras para la evolución de las negociaciones salariales en las jurisdicciones federal y local. 
#_______________________________________________________________________________

# procesamiento de datos -------------------------------------------------------

print("Gráficando jurisdicción federal y local")

juris_federal <- read_excel("excels/negociaciones_stata.xlsx", sheet = "j_federal") %>%
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(juris = "federal")


juris_local <- read_excel("excels/negociaciones_stata.xlsx", sheet = "j_local") %>%
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(juris = "local") 

# ------------------------------------------------------------------------------

# SERIE DE TIEMPO JURISDICCIÓN LOCAL 

juris_local <- juris_local %>% 
  filter(fecha <= fecha_interes & fecha >= fecha_inicio) 

puntos_juris_local <- juris_local %>% 
  group_by(juris) %>% 
  filter(fecha == max(fecha))

y_tick <- seq(as.Date(min(juris_local$fecha)), 
              as.Date(max(juris_local$fecha)), 
              by = "year")

colores_institucionales <- c("federal" = "#a57f2c", 
                             "local" = "#98989A")

ggplot(juris_local) + 
  geom_line(mapping = aes(x = fecha, y = REAL, color = juris), show.legend = FALSE) + 
  geom_point(mapping = aes(x = fecha, y = REAL, color = juris), size = 1, show.legend = FALSE) +
  geom_text(data = puntos_juris_local, aes(x = fecha, y = REAL, label = round(REAL, 2), color = juris, vjust = -1, hjust = 0, fontface = "bold"), size = 5, show.legend = FALSE) + 
  scale_color_manual(values = colores_institucionales, labels = c("Federal", "Local")) +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(limits = c(min(juris_local$REAL), max(juris_local$REAL) * 1.1)) +
  theme_conasami() +
  labs(x = "", y = "Variación Salarial Real (%)", color = "") + 
  theme(legend.position = "bottom", 
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 20),
        axis.text.y = element_text(size=20))

name_1 <- paste0("graphs/juris/ts_juris_local_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(name_1, plot = last_plot(), 
       width = 30, height = 15, units = "cm")

rm(name_1, y_tick, puntos_juris_local) 

#_______________________________________________________________________________

# GRÁFICA DE SERIES DE TIEMPO JURISDICCIÓN FEDERAL 

juris_federal <- juris_federal %>% 
  filter(fecha <= fecha_interes & fecha >= fecha_inicio)

puntos_juris_federal <- juris_federal %>% 
  group_by(juris) %>% 
  filter(fecha == max(fecha))


y_tick <- seq(as.Date(min(juris_federal$fecha)), 
              as.Date(max(juris_federal$fecha)), 
              by = "year")


ggplot(juris_federal) + 
  geom_line(mapping = aes(x = fecha, y = REAL, color = juris), show.legend = FALSE) + 
  geom_point(mapping = aes(x = fecha, y = REAL, color = juris), size = 1, show.legend = FALSE) +
  geom_text(data = puntos_juris_federal, aes(x = fecha, y = REAL, label = round(REAL, 2), color = juris, vjust = 1, hjust = -.1, fontface = "bold"), size = 10, show.legend = FALSE) + 
  scale_color_manual(values = colores_institucionales, labels = c("Federal", "Local")) +
  # theme_bw(base_size = 13, base_family = "Noto Sans") +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(limits = c(min(juris_federal$REAL), max(juris_federal$REAL) * 1.1)) +
  theme_conasami() +
  labs(x = "", y = "Variación Salarial Real (%)", color = "") + 
  theme(legend.position = "bottom", 
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 20),
        axis.text.y = element_text(size=20))


name_2 <- paste0("graphs/juris/ts_juris_federal_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(name_2, plot = last_plot(), 
       width = 50, height = 20, units = "cm")

rm(name_2, y_tick, puntos_juris_federal)


#_______________________________________________________________________________

# GRAFICA DE BARRAS JURISDICCIÓN FEDERAL

mes_interes <- month(as.Date(fecha_interes))

j_fed_bar_filtered <- juris_federal %>% 
  filter(mes == mes_interes) %>% 
  mutate(TRABAJADORES = TRABAJADORES/1000)

y_tick <- seq(as.Date(min(j_fed_bar_filtered$fecha)), 
              as.Date(max(j_fed_bar_filtered$fecha)), 
              by = "year")

# Le incluimos una linea horizontal que muestre el promedio 

trabajadores_fed_plot <- ggplot(data = j_fed_bar_filtered) + 
  geom_bar(mapping = aes(fecha, TRABAJADORES, fill = juris) , stat = 
             "identity", position = "dodge", show.legend = FALSE) + 
  geom_text(aes(fecha, TRABAJADORES, label = scales::label_number(scale = 1)(TRABAJADORES) , color = juris, fontface = "bold"),
            position = position_dodge2(width = 300),
            vjust = -0.4, size = 4.0,
            show.legend = FALSE) + 
  scale_color_manual(values = c("federal" = "#611232", "local" = "#98989A")) +
  scale_fill_manual(values = c("federal" = "#611232", "local" = "#98989A"), labels = c("Federal", "Local")) +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(limits = c(0, max(j_fed_bar_filtered$TRABAJADORES) * 1.2)) +
  labs(
    title = "Personas trabajadoras",
    subtitle = "",
    x = "",
    y = "Personas trabajadoras (miles)",
    fill = "", 
    color = ""
  ) + 
  theme_conasami() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size=20))

trabajadores_fed_plot

# Gráfica de barras del número de revisiones salariales en el último mes en cada jurisdicción 


revisiones_fed_plot <- ggplot(j_fed_bar_filtered) + 
  geom_bar(mapping = aes(fecha, REVISIONES, fill = juris) , stat = 
             "identity", position = "dodge", show.legend = FALSE) + 
  geom_text(aes(fecha, REVISIONES, label = REVISIONES, color = juris, fontface = "bold"),
            position = position_dodge2(width = 300),
            vjust = -0.4, size = 4.0,
            show.legend = FALSE) + 
  scale_color_manual(values = c("federal" = "#611232", "local" = "#98989A")) +
  scale_fill_manual(values = c("federal" = "#611232", "local" = "#98989A"), labels = c("Federal", "Local")) +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(limits = c(0, max(j_fed_bar_filtered$REVISIONES) * 1.1)) +
  labs(
    title = "Revisiones",
    subtitle = "",
    x = "",
    y = "Número de revisiones salariales",
    fill = "", 
    color = ""
  ) + 
  theme_conasami() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size=20))

revisiones_fed_plot

revisiones_trabajadores_fed_plot <- trabajadores_fed_plot + revisiones_fed_plot + 
  plot_layout(ncol = 2, widths = c(1.5, 1.5))


print(revisiones_trabajadores_fed_plot)

name_3 <- paste0("graphs/juris/barras_federal_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(name_3, revisiones_trabajadores_fed_plot, 
       width = 35, height = 15, units = "cm")

rm(name_3, y_tick, j_fed_bar_filtered)

#_______________________________________________________________________________

#Gráfica de barras para trabajadores del último mes en local

mes_loc <- month(as.Date(max(juris_local$fecha)-1)) # obtenemos el mes de la fecha máxima

j_loc_bar_filtered <- juris_local %>% 
  filter(mes == mes_loc) %>% 
  mutate(TRABAJADORES = TRABAJADORES/1000)
  

y_tick <- seq(as.Date(min(j_loc_bar_filtered$fecha)), 
              as.Date(max(j_loc_bar_filtered$fecha)), 
              by = "year")

# Le incluimos una linea horizontal que muestre el promedio 

trabajadores_loc_plot <- ggplot(data = j_loc_bar_filtered) + 
  geom_bar(mapping = aes(fecha, TRABAJADORES, fill = juris) , stat = 
             "identity", position = "dodge", show.legend =  FALSE) + 
  geom_text(aes(fecha, TRABAJADORES, label = scales::label_number(scale = 1)(TRABAJADORES) , color = juris, fontface = "bold"),
            position = position_dodge2(width = 300),
            vjust = -0.4, size = 4.0,
            show.legend = FALSE) + 
  #geom_hline(yintercept = mean(j_loc_bar_filtered$TRABAJADORES), linetype = "dashed", color = "black") +
  scale_color_manual(values = c("federal" = "#611232", "local" = "#98989A")) +
  scale_fill_manual(values = c("federal" = "#611232", "local" = "#98989A"), labels = c("Federal", "Local")) +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(limits = c(0, max(j_loc_bar_filtered$TRABAJADORES) * 1.1)) +
  labs(
    title = "Personas trabajadoras",
    subtitle = "",
    x = "",
    y = "Personas trabajadoras (miles)",
    fill = "", 
    color = ""
  ) + 
  theme_conasami() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size=20))

trabajadores_loc_plot

# Gráfica de barras del número de revisiones salariales en el último mes en cada jurisdicción 


revisiones_loc_plot <- ggplot(j_loc_bar_filtered) + 
  geom_bar(mapping = aes(fecha, REVISIONES, fill = juris) , stat = 
             "identity", position = "dodge",
           show.legend = FALSE) + 
  geom_text(aes(fecha, REVISIONES, label = REVISIONES, color = juris, fontface = "bold"),
            position = position_dodge2(width = 300),
            vjust = -0.4, size = 4.0,
            show.legend = FALSE) + 
  scale_color_manual(values = c("federal" = "#611232", "local" = "#98989A")) +
  scale_fill_manual(values = c("federal" = "#611232", "local" = "#98989A"), labels = c("Federal", "Local")) +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(limits = c(0, max(j_loc_bar_filtered$REVISIONES) * 1.1)) +
  labs(
    title = "Revisiones",
    subtitle = "",
    x = "",
    y = "Número de revisiones salariales",
    fill = "", 
    color = ""
  ) + 
  theme_conasami() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size=20))

revisiones_loc_plot

revisiones_trabajadores_loc_plot <- trabajadores_loc_plot + revisiones_loc_plot + 
  plot_layout(ncol = 2, widths = c(1.5, 1.5))


print(revisiones_trabajadores_loc_plot)

name_4 <- paste0("graphs/juris/barras_local_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(name_4, revisiones_trabajadores_loc_plot, 
       width = 35, height = 15, units = "cm")

rm(name_4, y_tick, j_loc_bar_filtered, revisiones_fed_plot, revisiones_trabajadores_loc_plot, trabajadores_fed_plot, trabajadores_loc_plot, revisiones_trabajadores_fed_plot)

# grafica de barras personas y revisiones anual --------------------------------

federal_anual <- juris_federal %>% 
  group_by(year) %>% 
  summarise(
    revisiones = sum(REVISIONES),
    trabajadores = sum(TRABAJADORES),
    nominal = mean(NOMINAL),
    real = mean(REAL)
  ) %>% 
  filter(year >= 2018)

bar_trabajadores_anual_fed <- ggplot(federal_anual) +
  geom_bar(
    mapping = aes(x = factor(year), y = trabajadores / 1000),
    stat = "identity",
    fill = "#611232"
  ) + 
  theme_conasami() +
  geom_text(
    aes(x = factor(year), 
        y = round(trabajadores / 1000, 3), 
        label = scales::label_number(scale = 1)(trabajadores / 1000)), 
    vjust = -0.4, 
    size = 4.0, 
    fontface = "bold"
    ) +
  labs(
    title = "Personas trabajadoras",
    subtitle = "",
    x = "",
    y = "Personas trabajadoras (miles)"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size=18))

bar_trabajadores_anual_fed

bar_revisiones_anual_fed <- ggplot(federal_anual) +
  geom_bar(
    mapping = aes(x = factor(year), y = revisiones),
    stat = "identity",
    fill = "#611232"
  ) + 
  theme_conasami() +
  geom_text(
    aes(x = factor(year), 
        y = revisiones, 
        label = revisiones), 
    vjust = -0.4, 
    size = 4.0, 
    fontface = "bold"
  ) +
  labs(
    title = "Revisiones salariales",
    subtitle = "",
    x = "",
    y = "Número de revisiones salariales"
  ) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size= 18))

bar_revisiones_anual_fed

revisiones_trabajadores_anual_fed_plot <- bar_trabajadores_anual_fed + 
  bar_revisiones_anual_fed + 
  plot_layout(
    ncol = 2, 
    widths = c(1.5, 1.5)
              )

revisiones_trabajadores_anual_fed_plot

ggsave(
  filename = paste0("graphs/juris/barra_federal_anual_", fecha_interes %>% format("%Ym%m"), ".png"),
  plot = revisiones_trabajadores_anual_fed_plot,
  width = 40, 
  height = 15, 
  units = "cm"
)

ggplot(federal_anual) + 
  geom_line(
    mapping = aes(x = year, y = real),
    color = "#611232",
    size = 1.2
  ) +
  geom_bar(
    mapping = aes(x = year, y = real),
    stat = "identity",
    fill = "#FDE9EF",
    alpha = 0.8
  ) + 
  geom_line(
    mapping = aes(x = year, y = real),
    color = "#611232",
    size = 1.2
  ) +
  geom_text(
    data = federal_anual %>% filter(year != 2025), 
    aes(x = year, y = real, label = round(real, 2), vjust = ifelse(real >= 0, -0.8, 1.5), hjust = ifelse(year == 2018, 1, 0.5)),
    size = 5,
    fontface = "bold",
    color = "grey50"
  ) +
  geom_text(
    data = federal_anual %>% filter(year == 2025), 
    aes(x = year, y = real, label = round(real, 2), vjust = -2, hjust = -0.15),
    size = 5,
    fontface = "bold",
    color = "#611232"
  ) +
  scale_x_continuous(breaks = seq(min(federal_anual$year), max(federal_anual$year), by = 1)) +
  scale_y_continuous(limits = c(min(federal_anual$real) * 1.2, max(federal_anual$real) * 1.2),
                     breaks = seq(-2, 4, by = 1)) +
  theme_conasami() +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "Variación salarial real (%)"
  ) + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 0),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size= 18))

ggsave(
  filename = paste0("graphs/juris/linea_federal_anual_", fecha_interes %>% format("%Ym%m"), ".png"),
  plot = last_plot(),
  width = 35, 
  height = 20, 
  units = "cm"
)

# local ------------------------------------------------------------------------

local_anual <- juris_local %>% 
  group_by(year) %>% 
  summarise(
    revisiones = sum(REVISIONES),
    trabajadores = sum(TRABAJADORES),
    nominal = mean(NOMINAL),
    real = mean(REAL)
  ) %>% 
  filter(year >= 2012)

bar_trabajadores_anual_loc <- ggplot(local_anual) +
  geom_bar(
    mapping = aes(x = factor(year), y = trabajadores / 1000),
    stat = "identity",
    fill = "#98989A"
  ) + 
  theme_conasami() +
  geom_text(
    aes(x = factor(year), 
        y = round(trabajadores / 1000, 3), 
        label = scales::label_number(scale = 1)(trabajadores / 1000)), 
    vjust = -0.4, 
    size = 4.0, 
    fontface = "bold"
  ) +
  labs(
    title = "Personas trabajadoras",
    subtitle = "",
    x = "",
    y = "Personas trabajadoras (miles)"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size=18))

bar_trabajadores_anual_loc

bar_revisiones_anual_loc <- ggplot(local_anual) +
  geom_bar(
    mapping = aes(x = factor(year), y = revisiones),
    stat = "identity",
    fill = "#98989A"
  ) + 
  theme_conasami() +
  geom_text(
    aes(x = factor(year), 
        y = revisiones, 
        label = revisiones), 
    vjust = -0.4, 
    size = 4.0, 
    fontface = "bold"
  ) +
  labs(
    title = "Revisiones salariales",
    subtitle = "",
    x = "",
    y = "Número de revisiones salariales"
  ) + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(size= 18))

bar_revisiones_anual_loc

revisiones_trabajadores_anual_loc_plot <- bar_trabajadores_anual_loc + 
  bar_revisiones_anual_loc + 
  plot_layout(
    ncol = 2, 
    widths = c(1.5, 1.5)
  )

revisiones_trabajadores_anual_loc_plot

ggsave(
  filename = paste0("graphs/juris/barra_local_anual_", fecha_interes %>% format("%Ym%m"), ".png"),
  plot = revisiones_trabajadores_anual_loc_plot,
  width = 40, 
  height = 15, 
  units = "cm"
)

ggplot(local_anual) + 
  geom_smooth(
    mapping = aes(x = year, y = real),
    method = "loess",
    span = .5,
    se = FALSE,
    linewidth = 1,
    color = "#98989A"
  ) + 
  geom_bar(
    mapping = aes(x = year, y = real),
    stat = "identity",
    fill = "#F0F0F0",
    alpha = 0.8
  ) +
  geom_text(
    data = local_anual %>% filter(year != 2025), 
    aes(x = year, y = real, label = round(real, 2), vjust = ifelse(real >= 0, -0.8, 1.5), hjust = 0.5),
    size = 5,
    fontface = "bold",
    color = "grey50"
  ) +
  geom_text(
    data = local_anual %>% filter(year == 2025), 
    aes(x = year, y = real, label = round(real, 2), vjust = -2, hjust = -0.15),
    size = 5,
    fontface = "bold",
    color = "#98989A"
  ) +
  scale_x_continuous(breaks = seq(min(local_anual$year), max(local_anual$year), by = 1)) +
  scale_y_continuous(limits = c(min(local_anual$real) * 1.2, max(local_anual$real) * 1.2),
                     breaks = seq(-2, 4, by = 1)) +
  theme_conasami() +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "Variación salarial real (%)"
  ) + 
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 0),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 18),
    legend.text = element_text(size = 18),
    axis













