#_______________________________________________________________________________

  # Héctor Iván Soto Parra 
  # Coordinación para analísis de la economia laboral 
  # 14 de febrero de 2025  
  # El objetivo es construir las gráficas para emplazamientos y huelgas 

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

emplazamientos <- read_excel("excels/5.2.5 Emplazamientos a Huelgas por Entidad Federativa.xlsx", skip = 9) |> 
  slice(1:483) |> 
  rename(
    fecha = 2, 
    Nacional = 3
  ) |> 
  select(-1) |> 
  filter(fecha != "Total") |> 
  mutate(
    across(.cols = -fecha, .fns = as.integer),
    fecha = paste0(fecha, "-01"),
    fecha = str_replace(fecha, "/", "-"),
    fecha = lubridate::parse_date_time(fecha, orders = "Y-b-d") |> lubridate::as_date(),
    mes = as.integer(lubridate::month(fecha)),
    year = as.integer(lubridate::year(fecha))
  ) |> 
  relocate(fecha, year, mes)

# Convertimos a formato long 

emplazamientos_long <- emplazamientos %>% 
  pivot_longer(cols = -c(fecha, year, mes), 
               names_to = "entidad", 
               values_to = "emplazamientos") %>% 
  filter(
    entidad != "Más de una entidad" & 
    entidad != "Nacional"
)
  

glimpse(emplazamientos_long)

emplazamientos_long_last <- emplazamientos_long %>% 
  filter(fecha == fecha_interes)%>% 
  mutate(emplazamientos = replace_na(emplazamientos, 0))


#_______________________________________________________________________________ 

# Carga los datos geoespaciales de los estados de México

mexico <- ne_states(country = "Mexico", returnclass = "sf")

# Cambia "Distrito Federal" a "Ciudad de México" en la columna 'name'
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)

# Une los datos de los estados con los datos de los incrementos reales
# Hacemos un casewhen para 

merged_mexico <- mexico %>%
  left_join(emplazamientos_long_last, by = c("name" = "entidad")) 
#Construimos las gráficas 

mapa_plot_real <- ggplot(merged_mexico) +
  geom_sf(aes(fill = emplazamientos), color = "black") +
  scale_fill_gradient(low = "#FDE9EF", high = "#611232") +
  theme_void() +
  theme(legend.position = "bottom") +
  labs(title = "", fill = "Variación Salarial Real (%)") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  coord_sf(datum = NA) + 
  theme(legend.position = c(0.2,0.2),
        text = element_text(family = "Noto Sans"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) + 
  labs(x = "", y = "", fill = "")

mapa_plot_real

#_______________________________________________________________________________

bar_text <- emplazamientos_long_last %>% 
  filter(emplazamientos != 0)

bar_entidad_real <- ggplot(emplazamientos_long_last %>% 
                             filter(emplazamientos != 0)) + 
  geom_bar(mapping = aes(x = reorder(entidad, emplazamientos), y = emplazamientos, fill = emplazamientos), stat = "identity") + 
  coord_flip() + 
  scale_y_continuous(limits = c(0, max(emplazamientos_long_last$emplazamientos) * 1.1)) +
  scale_fill_gradient(low = "#FDE9EF", high = "#611232") +
  geom_text(data = bar_text, mapping = aes(x = reorder(entidad, emplazamientos), y = emplazamientos, label = round(emplazamientos, 2)), vjust = 0, hjust = -.5, size = 4) +
  theme_conasami() + 
  theme(legend.position = "", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = .4)) + 
  labs(x = "", y = "Revisiones", fill = "")

bar_entidad_real

#_______________________________________________________________________________

combined_plot <- mapa_plot_real + bar_entidad_real + 
  plot_layout(ncol = 2, widths = c(2, 1))


print(combined_plot)

name_emp <- paste0("graphs/huelgas/mapa_emplazamientos_", fecha_interes %>% format("%Ym%m"), ".png")

ggsave(name_emp, combined_plot, 
       width = 35, height = 20, units = "cm")






