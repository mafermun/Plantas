library(tidyverse)

load("output/tablas/data_clean.rda")

### promedio inicial y final de la atura ----

initial_mean_alt <- data_clean %>% 
  group_by(treatment, plot, SUBPARCELA, Planta) %>% 
  arrange(fecha) %>% 
  slice(1) %>% 
  ungroup(Planta, SUBPARCELA, plot) %>%
  summarise(avg_alt = mean(altura) %>% round(1),
            desv_alt = sd(altura) %>% round(1))
   

final_mean_alt <- data_clean %>% 
  group_by(treatment, plot, SUBPARCELA, Planta) %>% 
  arrange(fecha) %>% 
  slice_tail(n = 1) %>% 
  ungroup(Planta, SUBPARCELA, plot) %>% 
  summarise(avg_alt = mean(altura) %>% round(1),
            desv_alt = sd(altura) %>% round(1))


  
mean_alt <- bind_rows(list(final = final_mean_alt,
                       initial = initial_mean_alt),
                  .id = "status")

ggplot(mean_alt, aes(plot, avg_alt, color = status)) +
  geom_point(aes(shape = factor(status)), size = 2.2) +
  facet_wrap(~ treatment)


### promedio inicial y final de numero de hojas ----

initial_mean_hoj <- data_clean %>% 
  group_by(treatment, plot, SUBPARCELA, Planta) %>% 
  arrange(fecha) %>% 
  slice(1) %>% 
  ungroup(Planta, SUBPARCELA, plot) %>% 
  summarise(avg_hoj = mean(hojas) %>% round(1),
            desv_hoj = sd(hojas) %>% round(1))

final_mean_hoj <- data_clean %>% 
  group_by(treatment, plot, SUBPARCELA, Planta) %>% 
  arrange(fecha) %>% 
  slice_tail(n = 1) %>% 
  ungroup(Planta, SUBPARCELA, plot) %>% 
  summarise(avg_hoj = round(mean(hojas), digits = 1),
            desv_hoj = sd(hojas) %>% round(digits = 1))

mean_hoj <- bind_rows(list(final = final_mean_hoj,
                       initial = initial_mean_hoj),
                  .id = "status")

ggplot(mean_hoj, aes(plot, avg_hoj, color = status)) +
  geom_point(aes(shape = factor(status)), size = 2.2) +
  facet_wrap(~ treatment)

### produccion de flores/frutos

ff <- data_clean %>%
  group_by(treatment, plot, SUBPARCELA, Planta) %>%
  ungroup(Planta, SUBPARCELA) %>% 
  summarise(frutos_flores = sum(`Flores frutos`, na.rm = T),
            mean_ff = mean(`Flores frutos`, na.rm = T),
            sd_ff = sd(`Flores frutos`, na.rm = T))

ggplot(ff, aes(treatment, frutos_flores, fill = plot)) +
  geom_col(position = "dodge", aes(color = plot), width = 0.5) +
  geom_text(aes(label = frutos_flores, y = frutos_flores + 0.1), 
            position = position_dodge(width = 0.5), 
            vjust = 0)

### tabla resumen

stats_veg <- mean_alt %>% 
  merge(mean_hoj) %>%
  arrange(desc(status)) %>% 
  pivot_wider(names_from = status, values_from = starts_with(c("avg_", "desv_")))

save(stats_veg, file = "output/tablas/estadistica_descriptiva.rda")
