library(tidyverse)
library(ggpmisc)

load("output/tablas/data_clean.rda")

vs <- data.frame(fechas = as.Date(c("2022-07-26", "2022-09-15", "2022-10-20",
                            "2022-12-10","2023-01-01","2023-04-06",
                            "2023-05-10","2023-06-08","2023-07-15",
                            "2023-09-12","2024-04-05","2024-11-11",
                            "2024-12-05"), format = "%Y-%m-%d"),
                 Drought_A = c(28, 27, 27, 27, 27, 25, 23, 21, 20, 19, 18, 15, 15)/28,
                 Drought_B = c(26, 21, 13, 13, 8, 7, 6, 4, 4, 4, 3, 3, 3)/26,
                 Drought_C = c(63, 57, 54, 43, 45, 38, 37, 37, 36, 35, 27, 22, 22)/63,
                 Control_A = c(NA, 29, 29, NA, 24, NA, 19, 18, NA, 18, 18, 17, 16)/29,
                 Control_B = c(NA, 40, 36, NA, NA, 31, 27, NA, NA, 23, NA, 16, 15)/40,
                 Control_C = c(NA, 10, 9, NA, 9, 9, NA, NA, NA, NA, NA, 3, 3)/10) %>% 
  pivot_longer(cols = -fechas, 
               names_to = c("tratamiento", "parcela"),
               names_pattern = "(.*)_(.)",
               values_to = "cantidad") %>% 
  na.omit() %>% 
  arrange(tratamiento)

valores <- tibble(x = c(0.95, 0.95),
       tratamiento = c("Drought", "Control"), 
       tbl = list(tibble(Parcela = c("A", "B", "C"),
                         Inicio = c(28, 26, 63), 
                         Final = c(15, 3, 22)), 
                  tibble(Parcela = c("A", "B", "C"),
                         Inicio = c(29, 40, 10), 
                         Final = c(16, 15, 3))))

ggplot(vs, aes(fechas, cantidad)) +
  geom_line(aes(color = parcela)) +
  geom_table_npc(data = valores, aes(npcx = x, npcy = x, label = tbl), size = 2.5)+
  facet_wrap(~tratamiento)

  