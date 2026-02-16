#_______________________________________________________________________________
# Héctor Iván Soto Parra 
# 08 de febrero de 2025
# Comisiòn Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral 
# Gráficas de línea y de barras para la evolución de las negociaciones salariales por tamaño de empresa  
#_______________________________________________________________________________


#_______________________________________________________________________________

#Lectura de datos y transformación de la base de datos

empresas <- read_excel("excels/negociaciones_stata.xlsx", sheet = "empresas") %>%
  mutate(fecha = as.Date(fecha))

empresas_long <- empresas %>% 
  pivot_longer(
    cols = starts_with("REAL_")| starts_with("nominal_") | 
      starts_with("revisiones_") | starts_with("trabajadores_"),
    names_to = c("variable", "clase"),
    names_sep = "_"
  ) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  filter(fecha >= fecha_inicio & fecha <= fecha_interes)
#_______________________________________________________________________________


#Gráfica de incrementos por tipo de empresa 

y_tick <- seq(as.Date(min(empresas_long$fecha)), 
              as.Date(max(empresas_long$fecha)), 
              by = "year")

texto_empresas <- empresas_long %>% 
  group_by(clase) %>% 
  filter(fecha == max(fecha)) %>% 
  mutate(ajuste = case_when(
    clase == "pub" ~ REAL - 0.2,
    clase == "priv" ~ REAL + 0.2,
    TRUE ~ REAL
  ) )


ggplot(empresas_long) + geom_line(mapping = aes(x = fecha, y = REAL, color = clase)) + 
  geom_point(mapping = aes(x = fecha, y = REAL, color = clase), size = 1, show.legend = FALSE) + 
  #geom_text(data = texto_empresas, aes(x = fecha, y = ajuste, label = sprintf("%.2f", REAL), color = clase, vjust = -.1, hjust = -.2), size = 5, show.legend = FALSE) +
  geom_text(texto_empresas %>% filter( clase == "priv"), mapping = aes(x = fecha, y = ajuste, label = sprintf("%.2f", REAL), color = clase, vjust = 1, hjust = -.1), size = 5, show.legend = FALSE) +
  geom_text(texto_empresas %>% filter( clase == "pub"), mapping = aes(x = fecha, y = ajuste, label = sprintf("%.2f", REAL), color = clase, vjust = 0, hjust = -.1), size = 5, show.legend = FALSE) +
  scale_color_manual(values = c("micro" = "#611232", "priv" = "#98989A", "pub" = "#a57f2c", "grande" = "#9b2247"), label = c("Privado", "Público")) + 
  theme_conasami() + 
  labs(x = "", y = "Variación Salarial Real (%)", color = "") + 
  theme(legend.position = "bottom", 
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 20), 
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 20),
        axis.text.y = element_text(size=20)) + 
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(limits = c(min(empresas_long$REAL), max(empresas_long$REAL) * 1.1)) 



name_empresas <- paste0("graphs/tipo_empresa/empresas_", fecha_interes %>%  format("%Ym%m"), ".png")

ggsave(name_empresas,  plot = last_plot(), 
       width = 30, height = 15, units = "cm")

#_______________________________________________________________________________