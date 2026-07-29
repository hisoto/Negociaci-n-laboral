#_______________________________________________________________________________

  # Héctor Iván Soto Parra 
  # Coordinación para analísis de la economia laboral 
  # 14 de febrero de 2025  
  # El objetivo es construir las gráficas para emplazamientos y huelgas 

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

emplazamientos <- read_excel(file.path(cfg$ruta_poligonos, "5.2.5 Emplazamientos a Huelgas por Entidad Federativa.xlsx"), skip = 9) |> 
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
  filter(fecha == cfg$fecha_interes)%>% 
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
  geom_sf(aes(fill = emplazamientos), color = "white", linewidth = 0.2) +
  scale_fill_gradient(
    low    = "#FDE9EF",
    high   = conasami_colores[["guinda_profundo"]],
    name   = "Emplazamientos",
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
  archivo     = paste0("mapa_solo_emplazamientos_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "06_emplazamientos"),
  tamano      = "ancho"
)

#_______________________________________________________________________________

bar_entidad_real <- ggplot(emplazamientos_long_last %>%
                             filter(emplazamientos != 0)) +
  geom_col(mapping = aes(x = reorder(entidad, emplazamientos), y = emplazamientos, fill = emplazamientos), width = 0.68) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(emplazamientos_long_last$emplazamientos) * 1.1),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_fill_gradient(low = "#FDE9EF", high = conasami_colores[["guinda_profundo"]]) +
  theme_conasami() +
  theme(legend.position = "none")

bar_entidad_real

#_______________________________________________________________________________

combined_plot <- mapa_plot_real + bar_entidad_real +
  plot_layout(ncol = 2, widths = c(2, 1))

print(combined_plot)

guardar_grafica_conasami(
  combined_plot,
  archivo     = paste0("mapa_emplazamientos_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "06_emplazamientos"),
  tamano      = "ancho"
)


