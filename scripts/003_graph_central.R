#_______________________________________________________________________________
# Héctor Iván Soto Parra
# 08 de febrero de 2025
# Comisiòn Nacional de los Salarios Mínimos
# Coordinación para el análisis de la Economía Laboral
# Gráficas de línea y de barras para la evolución de las negociaciones salariales por central obrera
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


#_______________________________________________________________________________

# Iniciamos importando los datos de excel y creando un df en formato long para las centrales obreras.

central_ctm <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx"), sheet = "ctm") %>%
  mutate(fecha = as.Date(fecha), central = "CTM") %>%
  rename(
    REAL = REAL_ctm,
    TRABAJADORES = TRABAJADORES_ctm,
    REVISIONES = REVISIONES_ctm,
    NOMINAL = NOMINAL_ctm
  )

central_croc <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx"), sheet = "croc") %>%
  mutate(fecha = as.Date(fecha), central = "CROC") %>%
  rename(
    REAL = REAL_croc,
    TRABAJADORES = TRABAJADORES_croc,
    REVISIONES = REVISIONES_croc,
    NOMINAL = NOMINAL_croc
  )

central_crom <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx"), sheet = "crom") %>%
  mutate(fecha = as.Date(fecha), central = "CROM") %>%
  rename(
    REAL = REAL_crom,
    TRABAJADORES = TRABAJADORES_crom,
    REVISIONES = REVISIONES_crom,
    NOMINAL = NOMINAL_crom
  )

central_sna_asa <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx"), sheet = "sna_asa") %>%
  mutate(fecha = as.Date(fecha), central = "SNA y ASA") %>%
  rename(
    REAL = REAL_sna_asa,
    TRABAJADORES = TRABAJADORES_sna_asa,
    REVISIONES = REVISIONES_sna_asa,
    NOMINAL = NOMINAL_sna_asa
  )

central_otras <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx"), sheet = "otras") %>%
  mutate(fecha = as.Date(fecha), central = "OTRAS") %>%
  rename(
    REAL = REAL_otras,
    TRABAJADORES = TRABAJADORES_otras,
    REVISIONES = REVISIONES_otras,
    NOMINAL = NOMINAL_otras
  )

central_ind_ct <- read_excel(file.path(cfg$ruta_poligonos, "negociaciones_central.xlsx"), sheet = "ind_ct") %>%
  mutate(fecha = as.Date(fecha), central = "CT") %>%
  rename(
    REAL = REAL_ind_ct,
    TRABAJADORES = TRABAJADORES_ind_ct,
    REVISIONES = REVISIONES_ind_ct,
    NOMINAL = NOMINAL_ind_ct
  )


centrales <- bind_rows(
  central_ctm,
  central_croc,
  central_crom,
  central_sna_asa,
  central_ind_ct,
  central_otras
)

rm(
  central_ctm,
  central_croc,
  central_crom,
  central_sna_asa,
  central_ind_ct,
  central_otras
)

centrales <- centrales %>%
  filter(fecha >= cfg$fecha_inicio_central & fecha <= cfg$fecha_interes) %>% 
  clean_names() %>% 
  mutate(
    direc = ifelse(real >= 0, "Positivo", "Negativo")
  )

#_______________________________________________________________________________

# Área de trazado únicamente (Manual DT 2026, §11): sin título de eje ni
# etiquetas de valor; el antetítulo/título/detalles/fuente se arman en Word.

g_centrales <- ggplot(centrales) +
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
                 breaks = seq(as.Date(min(centrales$fecha)),
                              as.Date(max(centrales$fecha)),
                              by = "year")) +
  facet_wrap(~central) +
  theme_conasami()

g_centrales

guardar_grafica_conasami(
  g_centrales,
  archivo     = paste0("ts_centrales_", format(cfg$fecha_interes, "%Ym%m")),
  dir         = here::here("graphs", "03_central_obrera"),
  tamano      = "ancho"
)

# tabla ------------------------------------------------------------------------

tabla <- centrales %>% 
  group_by(year, central) %>% 
  summarise(
    revisiones = sum(revisiones),
    trabajadores = sum(trabajadores),
    real = mean(real),
    nominal = mean(nominal)
  ) %>% 
  arrange(desc(year))

print(tabla)

print(centrales %>% filter(fecha == cfg$fecha_interes) %>% relocate(fecha, central, real) |> arrange(real))

