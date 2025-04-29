library(tidyverse)

load("output/tablas/data_clean.rda")
load("../Precipitación/R/precipitacion_tasasdecambiovegetacion.R")

data_cantidad_veg <- data_clean %>% 
  anti_join(P)


data_clean %>% 
  filter(estado) %>% 
  group_by(treatment, plot, SUBPARCELA) %>% 
  slice_min(fecha, n = 1) %>% 
  ungroup() %>%
  filter(altura > 0) %>% 
  count(treatment, plot, name = "inicio") 

data_clean %>% 
  filter(estado) %>% 
  group_by(treatment, plot, SUBPARCELA) %>% 
  slice_max(fecha, n = 1) %>% 
  ungroup() %>% 
  filter(altura > 0) %>% 
  count(treatment, plot, name = "final") 

data_clean %>% 
  count(treatment, plot, SUBPARCELA, fecha) %>% 
  ggplot(aes(fecha, n, color = factor(SUBPARCELA))) +
  geom_smooth(method = "lm", formula = "y~poly(x, 2)", se = F) +
  geom_point() +
  facet_grid(treatment ~ plot)
