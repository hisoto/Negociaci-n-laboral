#_______________________________________________________________________________

# Héctor Iván Soto Parra 
# Gráficas para informe mensual de negociaciones salariales
# 
#_______________________________________________________________________________

library(tidyverse)
library(readxl)
pacman::p_load(scales)
pacman::p_load(rnaturalearth)
pacman::p_load(rnaturalearthhires)
pacman::p_load(stringr)
pacman::p_load(patchwork)

rm(list = ls())

#_______________________________________________________________________________


if (!file.exists("graphs")) {
  dir.create("graphs")
  cat("Folder created successfully.\n")
} else {
  cat("The folder already exists.\n")
}

#_______________________________________________________________________________

juris_federal <- read_excel("excels/negociaciones_stata.xlsx", sheet = "j_federal") %>%
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(juris = "federal")

str(juris_federal)

juris_local <- read_excel("excels/negociaciones_stata.xlsx", sheet = "j_local") %>%
  mutate(fecha = as.Date(fecha)) %>% 
  mutate(juris = "local")

str(juris_federal)

juris_federal_local <- bind_rows(juris_federal, juris_local)

#_______________________________________________________________________________

# Gráfica de linea para incrementos reales en cada jurisdicción 

juris_federal_local_2019 <- juris_federal_local %>% 
  filter(year >= 2018)

puntos_juris <- juris_federal_local_2019 %>% 
  group_by(juris) %>% 
  filter(fecha == max(fecha))

colores_institucionales <- c("federal" = "#611232", 
                             "local" = "#98989A")

y_tick <- seq(as.Date(min(juris_federal_local_2019$fecha)), 
              as.Date(max(juris_federal_local_2019$fecha)), 
              by = "year")


ggplot(juris_federal_local_2019) + 
  geom_line(mapping = aes(x = fecha, y = REAL, color = juris)) + 
  geom_point(mapping = aes(x = fecha, y = REAL, color = juris), size = 1, show.legend = FALSE) +
  geom_text(data = puntos_juris, aes(x = fecha, y = REAL, label = round(REAL, 2), color = juris, vjust = -.5, hjust = -.5), show.legend = FALSE) + 
  scale_color_manual(values = colores_institucionales, labels = c("Federal", "Local")) +
  theme_bw(base_size = 13, base_family = "Noto Sans") +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  theme_minimal() +
  labs(x = "", y = "Variación Salarial Real (%)", color = "") + 
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black", linewidth = .05)) 

ggsave("graphs/ts_j_fed_loc/ts_juris.png")

trabajadores_ambas_juris <- juris_federal_local %>% 
  group_by(fecha) %>% 
  summarise(TRABAJADORES = sum(TRABAJADORES))

#_______________________________________________________________________________

#Gráfica de barras para trabajadores del último mes en cada jurisdicción. 

j_loc_fed_bar_filtered <- juris_federal_local_2019 %>% 
  filter(mes==12)

ggplot(data = j_loc_fed_bar_filtered) + 
  geom_bar(mapping = aes(fecha, TRABAJADORES, fill = juris) , stat = 
                                                   "identity", position = "dodge") + 
  geom_text(aes(fecha, TRABAJADORES, label = scales::label_number(scale = 0.001)(TRABAJADORES) , color = juris),
    position = position_dodge2(width = 300),
    vjust = -0.4, size = 3,
    show.legend = FALSE) + 
  scale_color_manual(values = c("federal" = "#611232", "local" = "#98989A")) +
  scale_fill_manual(values = c("federal" = "#611232", "local" = "#98989A")) +
  theme_bw(base_size = 13, base_family = "Noto Sans") +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  labs(
    title = "",
    subtitle = "Agosto 2024",
    x = "",
    y = "Trabajadoras(es) involucradas(es) (miles)",
    fill = "", 
    color = ""
  ) + theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.y.right = element_blank(),
    axis.line.x.top = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = .2))

ggsave("graphs/bar_j_fed_loc/bar_juris.png")


#_______________________________________________________________________________
#Gráfica de incrementos por tamaño de empresa 

empresas <- read_excel("negociaciones_stata.xlsx", sheet = "empresas") %>%
  mutate(fecha = as.Date(fecha))

empresas_long <- empresas %>% 
  pivot_longer(
    cols = starts_with("REAL_")| starts_with("nominal_") | 
      starts_with("revisiones_") | starts_with("trabajadores_"),
    names_to = c("variable", "clase"),
    names_sep = "_"
  ) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  filter(year >= 2018)

ggplot(empresas_long) + geom_line(mapping = aes(x = fecha, y = REAL, color = clase)) + 
  geom_point(mapping = aes(x = fecha, y = REAL, color = clase), size = 1, show.legend = FALSE) + 
  scale_color_manual(values = c("micro" = "#611232", "priv" = "#98989A", "pub" = "#a57f2c", "grande" = "#9b2247")) + 
  theme_minimal() + 
  labs(x = "", y = "Variación Salarial Real (%)", color = "") + 
  theme(legend.position = "bottom", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.line = element_line(color = "black")) + 
  scale_x_date(date_labels = "%Y", breaks = y_tick)

#_______________________________________________________________________________

# Gráfica de series de tiempo para incrementos reales por central obrera 
  # Iniciamos importando los datos de excel 

central_ctm <- read_excel("negociaciones_central.xlsx", sheet = "ctm") %>%
  mutate(fecha = as.Date(fecha),
         central = "CTM") %>% 
  rename(REAL = REAL_ctm, 
         TRABAJADORES = TRABAJADORES_ctm, 
         REVISIONES = REVISIONES_ctm, 
         NOMINAL = NOMINAL_ctm)

central_croc <- read_excel("negociaciones_central.xlsx", sheet = "croc") %>%
  mutate(fecha = as.Date(fecha),
         central = "CROC") %>% 
  rename(REAL = REAL_croc, 
         TRABAJADORES = TRABAJADORES_croc, 
         REVISIONES = REVISIONES_croc, 
         NOMINAL = NOMINAL_croc)

central_crom <- read_excel("negociaciones_central.xlsx", sheet = "crom") %>%
  mutate(fecha = as.Date(fecha), 
         central = "CROM") %>% 
  rename(REAL = REAL_crom, 
         TRABAJADORES = TRABAJADORES_crom, 
         REVISIONES = REVISIONES_crom, 
         NOMINAL = NOMINAL_crom)

central_sna_asa <- read_excel("negociaciones_central.xlsx", sheet = "sna_asa") %>%
  mutate(fecha = as.Date(fecha),
         central = "SNA_ASA") %>% 
  rename(REAL = REAL_sna_asa, 
         TRABAJADORES = TRABAJADORES_sna_asa, 
         REVISIONES = REVISIONES_sna_asa, 
         NOMINAL = NOMINAL_sna_asa)

central_otras <- read_excel("negociaciones_central.xlsx", sheet = "otras") %>%
  mutate(fecha = as.Date(fecha), 
         central = "OTRAS") %>% 
  rename(REAL = REAL_otras, 
         TRABAJADORES = TRABAJADORES_otras, 
         REVISIONES = REVISIONES_otras, 
         NOMINAL = NOMINAL_otras)

central_ind_ct <- read_excel("negociaciones_central.xlsx", sheet = "ind_ct") %>%
  mutate(fecha = as.Date(fecha), 
         central = "IND_CT") %>% 
  rename(REAL = REAL_ind_ct, 
         TRABAJADORES = TRABAJADORES_ind_ct, 
         REVISIONES = REVISIONES_ind_ct, 
         NOMINAL = NOMINAL_ind_ct)

centrales <- bind_rows(central_ctm, central_croc, central_crom, central_sna_asa, central_otras, central_ind_ct)

ggsave("graphs/ts_j_fed_loc/ts_juris.png")

# Nos gustatia mostrar las series de tiempo y la tendencia de los incrementos reales por central obrera. 

centrales_2018 <- centrales %>% 
  filter(year >= 2018)


# Gráfica de series de tiempo para incrementos reales por central obrera   
ggplot(centrales_2018) + 
  geom_line(mapping = aes(x = fecha, y = REAL),  alpha = .4, linewidth=1.2, colour = "#777777") +
  geom_smooth(mapping = aes(x = fecha, y = REAL, color = central), method = "loess", span = .4 ,se = FALSE, linewidth = 1) +
  facet_wrap(~central, scales = "fixed") +
  #geom_point(mapping = aes(x = fecha, y = REAL, color = central), size = 1, show.legend = FALSE) + 
  #geom_text(data = centrales %>% group_by(central) %>% filter(fecha == max(fecha)), aes(x = fecha, y = REAL, label = round(REAL, 2),  color = central, vjust = -.5, hjust = 1), show.legend = FALSE) + 
  scale_color_manual(values = c("#611232", "#98989A", "#a57f2c", "#9b2247", "#e6d194", "#161a1d"), labels = c("CTM", "CROC", "CROM", "SNA_ASA", "OTRAS", "IND")) + 
  scale_x_date(breaks = scales::date_breaks("2 year"), 
               labels = scales::date_format("%Y")) + 
  theme_minimal() + 
  labs(x = "", y = "Variación Salarial Real (%)", color = "") + 
  theme(legend.position = "none", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.line = element_line(color = "black"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2)) + 
  guides(color = guide_legend(override.aes = list(shape = NA)))

ggsave("graphs/ts_centrales/ts_centrales.png") 


#_______________________________________________________________________________

# Gráfica de barras para trabajadores del último mes en cada central obrera.
# Queremos agregar etiquetas con el último dato sobre cada barra 

centrales_trabajadores <- centrales %>% 
  filter(year >=2019) %>% 
  filter( mes == 8) 

ggplot(centrales_trabajadores) + 
  geom_bar(mapping = aes(x = year, y = TRABAJADORES, fill = central), stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("#611232", "#98989A", "#a57f2c", "#9b2247", "#e6d194", "#161a1d"), labels = c("CTM", "CROC", "CROM", "SNA_ASA", "OTRAS", "IND")) + 
  scale_y_continuous() + 
  theme_minimal() + 
  theme(legend.position = "bottom", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) + 
  labs(x = "", y = "Trabajadores involucrados", fill = "") 

#_______________________________________________________________________________

# Gráfica de incrementos reales por entidad federativa del último mes. Mapa de México.  

entidades <- read_excel("negociaciones_stata.xlsx", sheet = "entidad")%>%
  mutate(fecha = as.Date(fecha))

# Vamos a convertir el dataframe a formato long, con excepción de las variables fecha, año y mes. 

entidades_long <- entidades %>% 
  pivot_longer(cols = -c(fecha, year, mes), names_to = "entidad", values_to = "REAL")


add_spaces <- function(state_name){
  state_name <- str_replace_all(state_name, "(?<!^)([A-Z])", " \\1")
  return(state_name)
}

entidades_long <- entidades_long %>% 
  mutate(entidad = add_spaces(entidad))

entidades_long_last <- entidades_long %>% 
  filter(year == max(year)) %>%
  filter(mes == max(mes))

#nombres <- c("Aguascalientes","BajaCalifornia","BajaCaliforniaSur","Campeche","Coahuila","Colima","Chiapas","Chihuahua","CiudaddeMéxico","Durango","Guanajuato","Guerrero","Hidalgo","Jalisco","México","Michoacán","Morelos","Nayarit","NuevoLeón","Oaxaca","Puebla","Querétaro","QuintanaRoo","SanLuisPotosí","Sinaloa","Sonora","Tabasco","Tamaulipas","Tlaxcala","Veracruz","Yucatán","Zacatecas")
#ENT <- seq(1,32, by = 1)
#nombres_df <- data.frame(ENT, nombres)
#entidad_codigo <- left_join(entidades_long, nombres_df, by = c("entidad" = "nombres"))


##  install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")

# Carga los datos geoespaciales de los estados de México
mexico <- ne_states(country = "Mexico", returnclass = "sf")

# Cambia "Distrito Federal" a "Ciudad de México" en la columna 'name'
mexico$name <- ifelse(mexico$name == "Distrito Federal", "Ciudad de México", mexico$name)

# Une los datos de los estados con los datos de los incrementos reales

merged_mexico <- mexico %>%
  left_join(entidades_long_last, by = c("name" = "entidad"))

mapa_plot_real <- ggplot(merged_mexico) +
  geom_sf(aes(fill = REAL), color = "black") +
  scale_fill_gradient(low = "#98989A", high = "#611232") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(title = "", fill = "Variación Salarial Real (%)") +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  coord_sf(datum = NA) +
  theme(legend.position = "bottom", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank()) + 
  labs(x = "", y = "", fill = "%")

mapa_plot_real

bar_entidad_real <-  ggplot(entidades_long_last) + 
  geom_bar(mapping = aes(x = reorder(entidad, REAL), y = REAL), stat = "identity", fill = "#98989A") + 
  coord_flip() + 
  geom_text(mapping = aes(x = reorder(entidad, REAL), y = REAL, label = round(REAL, 2)), vjust = 0, hjust = -.5, size = 2.5) +
  theme_minimal() + 
  theme(legend.position = "", 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = .4)) + 
  labs(x = "", y = "Incrementos reales (%)", fill = "")

bar_entidad_real


combined_plot <- mapa_plot_real + bar_entidad_real + 
  plot_layout(ncol = 2, widths = c(1.9, 1.1))


print(combined_plot)

ggsave("graphs/mapa_entidades/mapa_entidades.png", combined_plot) 

#_______________________________________________________________________________














