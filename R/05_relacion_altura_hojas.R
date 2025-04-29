library(tidyverse)
library(readxl)
library(xlsx)
library(broom)
library(RColorBrewer)
library(stats)

source("R/funciones.R")
load("output/tablas/modelolineal_altura.rda")
load("output/tablas/modelolineal_hojas.rda")

mdl_tidy_hojas <- mdl_tidy_hojas %>% 
  filter(term == "fecha")
slope.altura.hojas <- mdl_tidy_altura %>% 
  filter(term == "fecha") %>% 
  select(treatment, plot, SUBPARCELA, Planta, slope.atura = Slope) %>% 
  full_join(mdl_tidy_hojas %>% 
              filter(term == "fecha") %>% 
              select(treatment, plot, SUBPARCELA, Planta, slope.hojas = Slope) )

ggplot(slope.altura.hojas) +
  aes( slope.atura, slope.hojas, color = factor(SUBPARCELA)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(treatment ~ plot, scales = "free") +
  labs(title = "Relación entre la tasa de crecimiento y la tasa de cambio de número de hojas")
