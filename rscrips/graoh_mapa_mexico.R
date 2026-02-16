#_______________________________________________________________________________
# Héctor Iván Soto Parra 
# 08 de febrero de 2025 
# Comisiòn Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral 
# Gráficas de línea y de barras para la evolución de las negociaciones salariales por entidad. 
#_______________________________________________________________________________

rm(list = ls());gc()

source("rscrips/theme_conasami.R")

pacman::p_load(
  tidyverse,
  dplyr,
  readxl,
  janitor,
  lubridate,
  janitor,
  rnaturalearth,
  rnaturalearthhires,
  patchwork
)

fecha_inicio <- as.Date("2017-01-01")

fecha_interes <- as.Date("2026-01-01")

#_______________________________________________________________________________

# Lectura de datos y transformación de la base de datos
  # Vamos a convertir el dataframe a formato long, con excepción de las variables fecha, año y mes. 

entidades <- read_excel("excels/negociaciones_stata.xlsx", sheet = "entidad")%>%
  mutate(fecha = as.Date(fecha))


entidades_long <- entidades %>% 
  pivot_longer(cols = -c(fecha, year, mes), 
               names_to = "entidad", 
               values_to = "REAL")


    # Vamos a cambiar los nombres de las entidades para que tengan espacios entre las palabras.

add_spaces <- function(state_name){
  state_name <- str_replace_all(state_name, "(?<!^)([A-Z])", " \\1")
  return(state_name)
}

entidades_long <- entidades_long %>% 
  mutate(entidad = add_spaces(entidad))

  # La función únicamente no pudo corregir Ciudadde México, es necesario cambiarlo 

entidades_long <- entidades_long %>%
  mutate(entidad = str_replace_all(entidad, "Ciudadde México", "Ciudad de México"))

    # Vamos a seleccionar los datos del último mes y año.

entidades_long_last <- entidades_long %>% 
  filter(fecha == fecha_interes) %>% 
  mutate(categoria = case_when(REAL < 0 ~ "Negativa",
                   REAL <= 5 ~ "Hasta 5%",
                   REAL <= 10 ~ "De 5 a 10%",
                   REAL > 10 ~ "Mayor a 10%"))


#_______________________________________________________________________________

# Datos para el mapa de México

#install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")


# Carga los datos geoespaciales de los estados de México

mexico <- ne_states(country = "Mexico", returnclass = "sf")

# Cambia "Distrito Federal" a "Ciudad de México" en la columna 'name'
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)

# Une los datos de los estados con los datos de los incrementos reales
  # Hacemos un casewhen para 

merged_mexico <- mexico %>%
  left_join(entidades_long_last, by = c("name" = "entidad")) 

#_______________________________________________________________________________

# Grafica de México con los incrementos reales por entidad federativa 


colores <- c("Negativa" = "#FDE9EF", "Hasta 5%" = "#c79dad",
             "De 5 a 10%" = "#955870", "Mayor a 10%" = "#621132")

#merged_mexico$categoria <- factor(merged_mexico$categoria, levels = c("Negativa", "Hasta 5%", "De 5 a 10%", "Mayor a 10%"))

imaief <- ggplot(data = merged_mexico) +
  geom_sf(aes(fill = categoria), color = "black") +
  labs(fill = "Variación anual") +
  scale_fill_manual(values = colores) +
  theme_void() +
  theme(legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"))
imaief

mapa_plot_real <- ggplot(merged_mexico) +
  geom_sf(aes(fill = REAL), color = "black") +
  scale_fill_gradient(low = "#FDE9EF", high = "#611232") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "", fill = "Variación Salarial Real (%)") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  coord_sf(datum = NA) +
  theme(
        legend.position = "none",
        #legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) + 
  labs(x = "", y = "", fill = "%")

mapa_plot_real

name <- paste0("graphs/entidades/mapa_solo_incremento_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(
  name,
  plot = mapa_plot_real, 
  width = 20, height = 15, units = "cm"
)

#_______________________________________________________________________________

# Gráfica de barras para los incrementos reales por entidad federativa del último mes.
  # queremos que la gráfica tenga un gradiente en el color 


entidades_long_last <- entidades_long_last %>% 
  mutate(REAL = replace_na(REAL,0)) %>% 
  filter(REAL != 0, entidad != "...36")

bar_entidad_real <- ggplot(entidades_long_last) + 
  geom_bar(mapping = aes(x = reorder(entidad, REAL), y = REAL, fill = REAL), stat = "identity") + 
  coord_flip() + 
  scale_y_continuous(limits = c(-.5, max(entidades_long_last$REAL)*1.1)) +
  scale_fill_gradient(low = "#FDE9EF", high = "#611232") +
  geom_text(mapping = aes(x = reorder(entidad, REAL), y = REAL, label = round(REAL, 2)), vjust = 0, hjust = -.2, size = 4) +
  theme_conasami() + 
  theme(legend.position = "", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = .4)) + 
  labs(x = "", y = "Incrementos reales (%)", fill = "")

bar_entidad_real

name <- paste0("graphs/entidades/bar_incremento_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(name, plot = bar_entidad_real, 
       width = 15, height = 20, units = "cm")

#_______________________________________________________________________________

# Combinamos las dos gráficas en un solo objeto.

combined_plot <- mapa_plot_real + bar_entidad_real + 
  theme_conasami() + 
  plot_layout(ncol = 2, widths = c(2, 1.0))


print(combined_plot)

name <- paste0("graphs/entidades/mapa_incremento_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(name, plot = last_plot(), 
       width = 35, height = 20, units = "cm")
