#_______________________________________________________________________________
# Héctor Iván Soto Parra 
# 08 de febrero de 2025 
# Comisiòn Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral 
# Gráficas de línea y de barras para la evolución de las negociaciones salariales por entidad. 
#_______________________________________________________________________________

rm(list = ls());gc()

source(here::here("scripts", "theme_conasami_dt2026.R"))

# Configuración (Master_negociaciones.R la define; fallback si se corre suelto)
if (is.null(getOption("negociaciones"))) source(here::here("scripts", "000_config.R"))
cfg <- getOption("negociaciones")

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


#_______________________________________________________________________________

# Lectura de datos y transformación de la base de datos
  # Vamos a convertir el dataframe a formato long, con excepción de las variables fecha, año y mes. 

entidades <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_stata.xlsx"), sheet = "entidad")%>%
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
  filter(fecha == cfg$fecha_interes) %>% 
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
  geom_sf(aes(fill = REAL), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low    = "#FDE9EF",
    high   = conasami_colores[["guinda_profundo"]],
    name   = "Variación anual real (%)",
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_number(accuracy = 1)
  ) +
  guides(fill = guide_colourbar(
    title.position = "top",
    barwidth       = unit(2.5, "cm"),
    barheight      = unit(0.4, "cm")
  )) +
  coord_sf(datum = NA) +
  theme_void(base_family = "Noto Sans") +
  theme(
    legend.position      = c(0.02, 0.18),
    legend.justification = c(0, 0),
    legend.direction     = "horizontal",
    legend.title         = element_text(family = "Noto Sans", size = 9,
                                         color = conasami_neutros[["texto_secundario"]]),
    legend.text          = element_text(family = "Noto Sans Tab", size = 8,
                                         color = conasami_neutros[["tinta"]]),
    text             = element_text(family = "Noto Sans", color = conasami_neutros[["tinta"]]),
    plot.background  = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA)
  )

mapa_plot_real

guardar_grafica_conasami(
  mapa_plot_real,
  archivo     = paste0("mapa_solo_incremento_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "05_entidades"),
  tamano      = "ancho"
)

#_______________________________________________________________________________

# Gráfica de barras para los incrementos reales por entidad federativa del último mes.
  # queremos que la gráfica tenga un gradiente en el color 


entidades_long_last <- entidades_long_last %>% 
  mutate(REAL = replace_na(REAL,0)) %>% 
  filter(REAL != 0, entidad != "...36")

bar_entidad_real <- ggplot(entidades_long_last) +
  geom_col(mapping = aes(x = reorder(entidad, REAL), y = REAL, fill = REAL), width = 0.68) +
  coord_flip() +
  scale_y_continuous(limits = c(-.5, max(entidades_long_last$REAL) * 1.1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_gradient(low = "#FDE9EF", high = conasami_colores[["guinda_profundo"]]) +
  theme_conasami() +
  theme(legend.position = "none")

bar_entidad_real

guardar_grafica_conasami(
  bar_entidad_real,
  archivo     = paste0("bar_incremento_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "05_entidades"),
  tamano      = "medio"
)

#_______________________________________________________________________________

# Combinamos las dos gráficas en un solo objeto.

combined_plot <- mapa_plot_real + bar_entidad_real +
  plot_layout(ncol = 2, widths = c(2, 1))

print(combined_plot)

guardar_grafica_conasami(
  combined_plot,
  archivo     = paste0("mapa_incremento_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "05_entidades"),
  tamano      = "ancho"
)
