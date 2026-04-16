#_______________________________________________________________________________

# Autor: Héctor Iván Soto Parra 

# Objetivo: Automatizar gráficas de negociaciones salariales 

#_______________________________________________________________________________

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

juris <- juris_federal %>% 
  rbind(
    juris_local
  )

rm(juris_local, juris_federal)

# Serie de tiempo --------------------------------------------------------------

juris %>%
  group_split(juris) %>%
  walk(function(df) {
    tipo <- unique(df$juris)
    
    puntos_finales <- df %>%
      filter(fecha == max(fecha))
    
    p <- ggplot(df) +
      geom_line(aes(x = fecha, y = REAL, color = juris), show.legend = FALSE) +
      geom_point(aes(x = fecha, y = REAL, color = juris), size = 1, show.legend = FALSE) +
      geom_text(
        data = puntos_finales,
        aes(x = fecha, y = REAL, label = round(REAL, 2), color = juris),
        vjust = -1,
        hjust = 0,
        fontface = "bold",
        size = 5
      ) +
      scale_color_manual(values = colores_institucionales) +
      scale_x_date(date_labels = "%Y", breaks = y_tick) +
      labs(
        x = "",
        y = "Variación Salarial Real (%)",
        title = paste("Evolución de la variación salarial real - Jurisdicción", tipo)
      ) +
      theme_conasami() +
      theme(
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18)
      )
    
    name_1 <- paste0(
      "graphs/juris/ts_juris_", tipo, "_",
      format(fecha_interes, "%Y%m"),
      ".png"
    )
    
    # Guardar gráfico
    ggsave(
      filename = name_1,
      plot = p,
      width = 30,
      height = 15,
      units = "cm"
    )
    
    message("Gráfico guardado: ", name_1)
    
  })