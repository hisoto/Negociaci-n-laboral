#_______________________________________________________________________________

# Huelgas vigentes por causa 

#_______________________________________________________________________________


huelgas_vigentes <- read_excel("excels/negociaciones_stata.xlsx", sheet = "huelgas")

huelgas_long <- huelgas_vigentes %>% 
  select(-huelgas) %>% 
  pivot_longer(cols = -c(fecha, huelgas_vigentes), names_to = "causa", values_to = "huelga")

glimpse(huelgas_long)

#_______________________________________________________________________________

huelgas <- huelgas_vigentes %>% 
  select(c(fecha, huelgas, huelgas_vigentes)) %>% 
  rename(huelgas_estalladas = huelgas) %>%
  pivot_longer(cols = -fecha , names_to = "tipo", values_to = "huelgas")%>% 
  mutate(fecha=as.Date(paste0(fecha, "-01-01"))) %>% 
  filter(fecha >= as.Date("2007-01-01")) 


y_tick <- seq(as.Date(min(huelgas$fecha)), 
                  as.Date(max(huelgas$fecha)), 
                  by = "year") 

huelgas_text <- huelgas %>% 
  filter(huelgas != 0)


ggplot(huelgas) + 
  geom_bar(mapping = aes(x = fecha, y = huelgas, fill = tipo), stat = "identity", position = "dodge") +
  geom_text(huelgas_text, mapping = aes(fecha, huelgas, label = scales::label_number(scale = 1)(huelgas), fontface = "bold"),
            position = position_dodge2(width = 300),
            vjust = -0.4, hjust = .7, size = 4.0,
            show.legend = FALSE) +
  theme_conasami() + 
  #scale_color_manual(values = c("huelgas" = "#a57f2c", "huelgas_vigentes" = "#98989A")) + 
  scale_fill_manual(values = c("huelgas_estalladas" = "#a57f2c", "huelgas_vigentes" = "#98989A"), labels = c("Huelgas estalladas", "Huelgas vigentes")) +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    fill = "", 
    color = ""
  ) + theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20), 
    axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
    axis.text.y = element_text(size=20))


name_h <- paste0("graphs/huelgas/bar_huelgas_", fecha_interes %>%  format("%Ym%m"), ".png")

ggsave(name_h, plot = last_plot())

#_______________________________________________________________________________

huelgas_causa <- read_excel("excels/Libro1.xlsx", sheet = "df") %>% 
  filter(if_all(everything(), ~ !is.na(.))) %>% 
  mutate(huelgas = 1) %>% 
  mutate(year = year(`Fecha de inicio`)) %>% 
  group_by(Causa, year) %>% 
  summarise(sum(huelgas))

glimpse(huelgas_causa) 

labels_causa <- unique(huelgas_causa$Causa)

ggplot(huelgas_causa) + 
  geom_bar(
    mapping = aes(x = factor(year), y = `sum(huelgas)` , fill = Causa),
    stat = "identity", 
    position = "stack") + 
  scale_fill_manual(
    values = c("#611232", "#98989A","#e6d194", "#a57f2c", "#9b2247",  "#161a1d"), labels = labels_causa) + 
  theme_conasami() + 
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 15), 
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 15),
    axis.text.y = element_text(angle = 0, hjust = 1, size = 15)) + 
  guides(fill = guide_legend(nrow = 2, col = 3)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    fill = "", 
    color = ""
  ) 


name_h_2 <- paste0("graphs/huelgas/bar_huelgas_causa_", fecha_interes %>%  format("%Ym%m"), ".png")

ggsave(name_h_2, plot = last_plot(), 
       width = 30, height = 15, units = "cm")

#_______________________________________________________________________________

    # Presentación

huelgas_vigentes_p <- ggplot(huelgas) + 
  geom_bar(mapping = aes(x = fecha, y = huelgas, fill = tipo), stat = "identity", position = "dodge") +
  geom_text(huelgas_text, mapping = aes(fecha, huelgas, label = scales::label_number(scale = 1)(huelgas), fontface = "bold"),
            position = position_dodge2(width = 300),
            vjust = -0.4, hjust = .8, size = 4.0,
            show.legend = FALSE) +
  theme_conasami() + 
  #scale_color_manual(values = c("huelgas" = "#a57f2c", "huelgas_vigentes" = "#98989A")) + 
  scale_fill_manual(values = c("huelgas" = "#a57f2c", "huelgas_vigentes" = "#98989A"), labels = c("Huelgas", "Huelgas vigentes")) +
  scale_x_date(date_labels = "%Y", breaks = y_tick) +
  labs(
    title = "",
    subtitle = "",
    x = "",
    y = "",
    fill = "", 
    color = ""
  ) + theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 20),
    plot.subtitle = element_text(hjust = .5), 
    axis.title.y = element_text(size = 20),
    legend.text = element_text(size = 20), 
    axis.text.x = element_text(angle = 90, hjust = 1, size = 15),
    axis.text.y = element_text(size=20),
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA),
    legend.background = element_rect(fill = "transparent"))

huelgas_vigentes_p 

ggsave("graphs/bar_huelgas/bar_huelgas_p.svg" , plot = last_plot(), 
       width = 30, height = 15, units = "cm")




