


print(juris_local %>%
        group_by(year) %>% 
        summarise(mean(REAL)) %>% 
        filter(year == max(year)))

print(juris_local %>%
        group_by(year) %>% 
        summarise(mean(NOMINAL)) %>% 
        filter(year == max(year)))


print(juris_local %>%
        group_by(year) %>% 
        summarise(sum(REVISIONES)) %>% 
        filter(year == max(year)))

print(juris_local %>%
        group_by(year) %>% 
        summarise(sum(TRABAJADORES)) %>% 
        filter(year == max(year)))


print(juris_local %>% 
        filter(fecha == max(fecha)))

juris_local_last <- juris_local %>% 
  filter(fecha == max(fecha))

print(juris_local_last)

juris_local_last <- juris_local %>% 
  filter(fecha== max(fecha))

print(juris_local_last)
