library(tidyverse)

source("R/funciones.R")
load("output/tablas/data_clean.rda")

ggplot(data_clean, aes(x = fecha, y = estado, color = factor(Planta))) + 
  geom_col() +
  facet_grid(treatment ~ plot) +
  labs(title = "estado")

survival_time <- data_clean %>%
  group_by(treatment, plot, SUBPARCELA, Planta, estado) %>% 
  summarise(survival = sum(fecha - lag(fecha, default = first(fecha), 
                                       order_by = Planta)))

data_clean %>% 
  group_by(treatment, plot, SUBPARCELA, Planta, estado) %>% 
  mutate()

# ggplot(survival_time, aes(x = survival, y = Planta)) + 
#   geom_bar() +
#   facet_grid(treatment ~ plot) +
#   labs(title = "estado")

