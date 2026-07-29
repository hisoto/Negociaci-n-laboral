#_______________________________________________________________________________

# Objetivo: Hacer gráficas de revisiones salariales por tipo de empresa

# Autor: Héctor Iván Soto Parra 

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
  janitor
)


# Datos ------------------------------------------------------------------------


revisiones <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_stata.xlsx"), sheet = "empresas") %>%
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
  filter(fecha >= cfg$fecha_inicio_empresas & fecha <= cfg$fecha_interes) %>% 
  mutate(
    clase = case_when(
      clase == "priv" ~ "Privada",
      clase == "pub" ~ "Pública",
      TRUE ~ clase
    ),
    direc = ifelse(real >= 0, "Positivo", "Negativo")
  )

# serie de tiempo --------------------------------------------------------------

g_empresas <- ggplot(revisiones) +
  geom_point(
    mapping = aes(x = fecha, y = real, color = direc),
    show.legend = FALSE
  ) +
  geom_line(mapping = aes(x = fecha, y = real),
            color = conasami_colores[["guinda_profundo"]],
            linewidth = 0.75, lineend = "round", linejoin = "round") +
  geom_bar(
    mapping = aes(x = fecha, y = real, fill = direc),
    stat = "identity",
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_fill_direccion() +
  scale_color_direccion() +
  geom_hline(
    yintercept = 0,
    color = conasami_neutros[["texto_secundario"]],
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  scale_x_date(date_labels = "%Y",
               breaks = seq(as.Date(min(revisiones$fecha)),
                            as.Date(max(revisiones$fecha)),
                            by = "year")) +
  facet_wrap(~clase) +
  theme_conasami()

g_empresas

guardar_grafica_conasami(
  g_empresas,
  archivo     = paste0("empresas_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "04_tipo_empresa"),
  tamano      = "ancho"
)

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

print(revisiones %>% filter(fecha == cfg$fecha_interes))


