#librerias ----
library(tidyverse)
library(readxl)
library(xlsx)
library(broom)
library(RColorBrewer)
library(stats)

source("../AnalisisPlantas/R/01_funciones.R")
load("output/tablas/data_clean.rda")


### modelo lineal cantidad de hojas ----

mdl_tidy_hojas <- group_by(data_clean, treatment, plot, SUBPARCELA, Planta) %>% 
  group_modify(modelo_lineal, frm = hojas ~ fecha, stat = "tidy", .keep = TRUE) %>% 
  rename(Slope = `estimate`)
view(mdl_tidy)


mdl_tidy_hojas %>% 
  filter(term == "fecha", 
         p.value < 0.05
  ) %>% 
  ungroup() %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  view()

mdl_tidy_hojas %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  ggplot(aes(factor(SUBPARCELA), Slope,
             colour = factor(plot))) +
  geom_boxplot() +
  facet_grid(treatment~plot, scales = "free_x") 

plotly::ggplotly()


tmph <- mdl_tidy_hojas %>% 
  ungroup() %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  select(treatment, Slope) %>%
  group_by(treatment) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(id_cols = id, 
              names_from = treatment, 
              values_from = Slope) 

test <- ks.test(tmph$Control,tmph$Drought)
text <- test[2]


mdl_tidy_hojas %>% 
  ungroup() %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  select(treatment, plot, Slope) %>%
  ggplot() +
  aes(Slope, color= plot, scales = "free") +
  geom_density() +
  annotate(geom = "label",
           label = 'KS test\nD = 0.40952\np-value = 0.04405',
           x = 0.4,
           y = 18,
           label.padding = unit(0.65, "lines"),
           alpha = 0.45,
           size = 3,
           color = "black") +
  labs(title = "Tasa de producción de hojas (unidad/día)") +
  facet_grid(~ treatment)

tmp_hoj <- mdl_tidy_hojas %>% 
  ungroup() %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  select(treatment, Slope) %>%
  group_by(treatment) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(id_cols = id, 
              names_from = treatment, 
              values_from = Slope) 

ks.test(tmp_hoj$Control,tmp_hoj$Drought)


mdl_tidy_hojas %>% 
  ungroup() %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  select(treatment, Slope) %>%
  ggplot() +
  aes(Slope, color= treatment) +
  geom_density() +
  labs(title = "Tasa de numero de hojas")

save(mdl_tidy_hojas, file = "output/tablas/modelolineal_hojas.rda")