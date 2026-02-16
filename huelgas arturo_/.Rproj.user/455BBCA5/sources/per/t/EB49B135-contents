#_______________________________________________________________________________

# Héctor Iván Soto Parra 
# 14 DE ENERO 2025
# Intento de código para base de coeficiente de estallamiento 

#_______________________________________________________________________________

library(httr)
library(readxl)
library(openxlsx)
library(dplyr) 
library(tidyverse)
library(haven)
library(rvest)
library(dplyr)
library(xml2)
library(stringr)
library(robotstxt)
pacman::p_load(janitor)
pacman::p_load(scales)
#_______________________________________________________________________________

#df_culero <- data.frame(`CUENTAS NACIONALES AKJSFDBNAJKSD`=seq(1,5,1)) %>% clean_names()

setwd("OneDrive - El Colegio de México A.C/CONASAMI/PROYECTOS/Informe mensual/huelgas arturo /")
rm(list = ls())

#  df <- read_excel("excel/negociaciones_2.xlsx", col_names = TRUE, range = "A5:L473")

j_fed <-
  read_excel("excel/negociaciones_2.xlsx", col_names = TRUE) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(fecha = make_date(año, mes)) %>%
  mutate(juris = "Federal")%>%
  arrange(fecha)

j_loc <-
  read_excel("excel/negociaciones_stata.xlsx",
             sheet = "j_local",
             col_names = T) %>%
  clean_names() %>%
  rename(año = year) %>%
  mutate(across(-fecha, as.numeric)) %>%
  mutate(fecha = as.Date(fecha)) %>%
  mutate(juris = "Local")%>%
  arrange(fecha)

j_loc_fed_2019 <-
  full_join(
    j_fed,
    j_loc,
    by = c(
      "fecha",
      "revisiones",
      "trabajadores",
      "real",
      "nominal",
      "año",
      "mes",
      "juris"
    )
  ) %>%
  filter(año >= 2019) %>%
  arrange(fecha)
#_______________________________________________________________________________

y_tick <- seq(as.Date(min(j_loc_fed_2019$fecha)), as.Date(max(j_loc_fed_2019$fecha)), by = "year")

ggplot(data = j_loc_fed_2019) +
  geom_line(mapping = aes(fecha, real, color = juris),
            linewidth = .7) +
  scale_color_manual(values = c("Federal" = "#611232", "Local" = "#98989A")) +
  theme_bw(base_size = 13, base_family = "Noto Sans") +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  labs(
    title = "Incremento real en jurisdicción Federal y Local",
    x = "",
    y = "Incremento salarial real (%)",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.y.right = element_blank(),
    axis.line.x.top = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = .2)
  ) + 
  geom_point(data = j_loc_fed_2019, mapping = aes(fecha, real, color = juris), size = .5) +
  geom_text(
    data = tail(j_fed, 1),
    # Select the last row
    aes(x = fecha, y = real, label = real),
    hjust = 0.4,
    vjust = -0.6,
    color = "#611232",
    size = 5
  ) +  geom_text(
    data = tail(j_loc, 1),
    # Select the last row
    aes(x = fecha, y = real, label = round(real,2)),
    hjust = 0.4,
    vjust = -0.6,
    color = "#98989A",
    size = 5
  )

ggsave("graphs/ts_j_fed_loc/ts_juris.png")

#_______________________________________________________________________________


j_loc_fed_bar_filtered <- j_loc_fed_2019 %>% 
  filter(mes==8)

ggplot(data = j_loc_fed_bar_filtered) + geom_bar(mapping = aes(fecha, trabajadores, fill = juris) , stat = 
                                                   "identity", position = "dodge") +
  geom_text(
    aes(fecha, trabajadores, label = scales::label_number(scale = 0.001)(trabajadores) , color = juris),
    position = position_dodge2(width = 300),
    vjust = -0.4, size = 3
  ) + 
  scale_color_manual(values = c("Federal" = "#611232", "Local" = "#98989A")) +
  scale_fill_manual(values = c("Federal" = "#611232", "Local" = "#98989A")) +
  theme_bw(base_size = 13, base_family = "Noto Sans") +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  scale_y_continuous(labels = label_number(scale = 0.001)) +
  labs(
    title = "Negociaciones salariales y contractuales",
    subtitle = "Agosto 2024",
    x = "",
    y = "Trabajadoras(es) involucradas(es) (miles)",
    fill = "", 
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = .5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.y.right = element_blank(),
    axis.line.x.top = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = .2)
  )


ggsave("graphs/bar_j_fed_loc/bar_juris_labor.png")


ggplot(df_2019) + 
  geom_smooth(mapping = aes(x = año, y = real))+ geom_point(mapping = aes(x = año, y = real)) +
  tema_gobierno + geom_smooth(mapping = aes(x = fecha, y = real),color = "#98989A")

+ 
  facet_wrap(~ juris, nrow = 2)

#dev.print 


tema_gobierno <- theme_minimal()



