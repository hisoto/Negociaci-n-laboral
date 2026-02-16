#_______________________________________________________________________________

# Coordinación de Analísis de la Economía Laboral 

# Mapa de coeficientes de variación por ocupación y estado 

#_______________________________________________________________________________


rm(list = ls())

if (!require(pacman)) install.packages("pacman") 
library(pacman)

p_load("tidyverse","stringr", "Hmisc", "EnvStats", "survey","srvyr", "data.table","bit64", "statar", "foreign", "openxlsx", "extrafont", "readxl",  "rnaturalearthhires", "rnaturalearth", "patchwork")

source("scrips/theme_conasami.R")

#_______________________________________________________________________________

alimentos_cv <- read_excel("data/ocupacion.xlsx", sheet = "alimentos entidad") %>% 
  filter(ocupacion_lider == 5312) %>% 
  select(nom_ent, alimentos_cv) %>% 
  mutate(rango = factor(case_when(
    alimentos_cv < .15 ~ "menor a 15%",
    alimentos_cv >= .15 & alimentos_cv < .30 ~ "entre 15% y 30%",
    alimentos_cv >= .30 ~ "Mayor a 30%"
  ), levels = c("menor a 15%", "entre 15% y 30%", "Mayor a 30%")))

rangos_cv <- c("menor a 15%" = "#BFE3A7", 
               "entre 15% y 30%" = "#955870", 
               "Mayor a 30%" = "#621132")

# Cargamos info de México 

mexico <- ne_states(country = "Mexico", returnclass = "sf")
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)

# Hacemos un merge 

alimentos_mexico <- mexico %>%
  left_join(alimentos_cv, by = c("name" = "nom_ent")) %>% 
  filter(!is.na(name)) 

# Mapa 

alimentos_rango <- ggplot(data = alimentos_mexico) +
  geom_sf(aes(fill = rango), color = "black") +
  labs(title = "alimentos", fill = "Variación anual") +
  scale_fill_manual(values = rangos_cv) +
  theme_void() +
  theme(legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

alimentos_rango

#_______________________________________________________________________________

variables <- c("alimentos", "vivienda", "nanv", "gasto total")

# Hacemos una función que haga el mismo mapa 

crear_mapa <- function(variable) {
  # Cargar datos
  datos <- read_excel("data/ocupacion.xlsx", sheet = paste0(variable, " entidad")) %>% 
    filter(ocupacion_lider == 5312) %>% 
    select(nom_ent, all_of(paste0(variable, "_cv"))) %>% 
    mutate(rango = case_when(
      get(paste0(variable, "_cv")) < 0.15 ~ "menor a 15%",
      get(paste0(variable, "_cv")) >= 0.15 & get(paste0(variable, "_cv")) < 0.30 ~ "entre 15% y 30%",
      get(paste0(variable, "_cv")) >= 0.30 ~ "Mayor a 30%"
    ))
  
  # Definir colores para los rangos
  rangos_cv <- c("menor a 15%" = "#BFE3A7", 
                 "entre 15% y 30%" = "#955870", 
                 "Mayor a 30%" = "#621132")
  
  # Cargar el mapa de México
  mexico <- ne_states(country = "Mexico", returnclass = "sf")
  mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)
  
  # Hacer merge con los datos
  datos_mexico <- mexico %>%
    left_join(datos, by = c("name" = "nom_ent")) %>% 
    filter(!is.na(name))
  
  # Crear el mapa
  mapa <- ggplot(data = datos_mexico) +
    geom_sf(aes(fill = rango), color = "black") +
    labs(title = variable, fill = "") +
    scale_fill_manual(values = rangos_cv) +
    theme_void() +
    theme(legend.position = "none",
          text = element_text(family = "Noto Sans"),
          legend.key.size = unit(0.4, "cm"),
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
  
  return(mapa)
}


mapas <- lapply(variables, crear_mapa)

combined_plot <- mapas[[3]] / mapas[[2]] /
  alimentos_rango / mapas[[4]] +
  plot_layout(ncol = 2, nrow = 2)

# Mostrar el gráfico combinado
combined_plot

ggsave("graphs/mapas_cv_policias.png", combined_plot, width = 20, height = 15, units = "cm", dpi = 300)








