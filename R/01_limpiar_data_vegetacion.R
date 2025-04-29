library(tidyverse)

# load("output/tablas/dataRAW_vegetation_full.rda")
# 
# data_no_codes <- data_full %>%
#   filter(altura > 0)
# 
# save(data_no_codes, file = "output/tablas/data_no_codes.rda")

load("output/tablas/data_no_codes.rda")


### plot A y eliminar plantas repetidas

a <- data_no_codes %>%
  filter(plot == "A", 
         treatment == "Drought") %>% 
  anti_join(tibble(SUBPARCELA = 12, Planta = 31))
### grafica de plot A

ggplot(aa) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y") +
  labs(title = "Drought Plot A")

ggplot(aa) +
  aes(x = `fecha`, y = `hojas`, color = factor(`Planta`)) +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y") +
  labs(title = "Drought Plot A")

plotly::ggplotly()

### eliminar fechas donde las mediciones caen
# c("2022-10-20", "2022-10-27", "2023-04-15",
#   "2023-04-06", "2023-06-14","2023-07-15")

plantasSirvena <- a %>% 
  filter(!fecha %in% c("2022-10-20", "2022-10-27", "2023-04-15",
                       "2023-04-06", "2023-06-14","2023-07-15"),
         hojas > 0) %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  filter(n >= 3)

#### unir tabla filtrada con los datos faltantes

aa <- a %>% 
  filter(hojas > 0,
         !fecha %in% c("2022-10-20", "2022-10-27", "2023-04-15",
                       "2023-04-06", "2023-06-14","2023-07-15")) %>% 
  semi_join(plantasSirvena, by = c("plot", "SUBPARCELA", "Planta"))



#### leer plot B y eliminar plantas repetidas

b <- data_no_codes %>%
  filter(plot == "B",
         treatment == "Drought") %>% 
  anti_join(tibble(SUBPARCELA = 11, Planta = 16))

ggplot(b) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y") +
  labs(title = "Drought Plot B")
 
plotly::ggplotly()

###eliminar fechas donde las mediciones caen

plantasSirvenb <- b %>% 
  filter(!fecha %in% c("2022-10-27"),
         altura > 0) %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  filter(n >= 3)

#### unir tabla filtrada con los datos faltantes

bb <- b %>% 
  filter(altura > 0,
         !fecha %in% c("2022-10-27")) %>% 
  semi_join(plantasSirvenb, by = c("plot", "SUBPARCELA", "Planta"))


ggplot(bb) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y")

plotly::ggplotly()



#### leer plot C

c <- data_no_codes %>%
  filter(plot == "C",
         treatment == "Drought") %>% 
  anti_join(tibble(SUBPARCELA = 1, Planta = c(13, 21, 30, 31))) %>% 
  anti_join(tibble(SUBPARCELA = 4, Planta = c(11, 12))) %>% 
  anti_join(tibble(SUBPARCELA = 5, Planta = c(6)))

###graficar plot C y cc y eliminar plantas repetidas

ggplot(c) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y") +
  labs(title = "Drought Plot C")

plotly::ggplotly()

###eliminar fechas donde las mediciones caen

plantasSirvenc <- c %>% 
  filter(!fecha %in% c("2023-01-14", "2023-07-15"),
         altura > 0) %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  filter(n >= 3)

# "2023-04-15",
# "2023-06-24",

#### unir tabla filtrada con los datos faltantes

cc <- c %>% 
  filter(altura > 0,
         !fecha %in% c("2023-01-14", "2023-07-15")) %>% 
  semi_join(plantasSirvenc, by = c("plot", "SUBPARCELA", "Planta"))



###leer plot c1 y eliminar plantas repetidas

c1 <- data_no_codes %>%
  filter(plot == "A",
         treatment == "Control") %>% 
  anti_join(tibble(SUBPARCELA = 2, Planta = c(4)))

###graficar plot C1 y cc1

ggplot(c1) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y") +
  labs(title = "Control Plot A")

plotly::ggplotly()

###eliminar fechas donde las mediciones caen
# c("2023-04-15")

plantasSirvenc1 <- c1 %>% 
  filter(!fecha %in% c("2023-04-15"),
         altura > 0) %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  filter(n >= 3)

### unir tabla filtrada con los datos faltantes

cc1 <- c1 %>% 
  filter(altura > 0,
         !fecha %in% c("2023-04-15")) %>% 
  semi_join(plantasSirvenc1, by = c("plot", "SUBPARCELA", "Planta"))



#### leer plot c2 y eliminar plantas repetidas

c2 <- data_no_codes %>%
  filter(plot == "B",
         treatment == "Control") %>% 
  anti_join(tibble(SUBPARCELA = 2, Planta = c(9, 14))) %>% 
  anti_join(tibble(SUBPARCELA = 15, Planta = c(2)))

ggplot(c2) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y") +
  labs(title = "Control Plot B")

plotly::ggplotly()

###eliminar fechas donde las mediciones caen

plantasSirvenc2 <- c2 %>% 
  filter(!fecha %in% c("2023-04-15", "2023-05-21"),
         altura > 0) %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  filter(n >= 3)

#### unir tabla filtrada con los datos faltantes

cc2 <- c2 %>% 
  filter(altura > 0,
         !fecha %in% c("2023-04-15", "2023-05-21")) %>% 
  semi_join(plantasSirvenc2, by = c("plot", "SUBPARCELA", "Planta"))


ggplot(cc2) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y")

plotly::ggplotly()



#### leer plot c3

c3 <- data_no_codes %>%
  filter(plot == "C",
         treatment == "Control") %>% 
  anti_join(tibble(SUBPARCELA = 1, Planta = c(13, 21, 30, 31)))

ggplot(c3) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y") +
  labs(title = "Control Plot C")

plotly::ggplotly()

###eliminar fechas donde las mediciones caen

plantasSirvenc3 <- c3 %>% 
  filter(!fecha %in% c("2023-04-15"),
         altura > 0) %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  filter(n >= 3)

#### unir tabla filtrada con los datos faltantes

cc3 <- c3 %>% 
  filter(altura > 0,
         !fecha %in% c("2023-04-15", "2023-05-21")) %>% 
  semi_join(plantasSirvenc3, by = c("plot", "SUBPARCELA", "Planta"))


ggplot(cc3) +
  aes(x = `fecha`, y = `altura`, color = factor(`Planta`)) +
  geom_line() +
  # geom_smooth(method = "lm", formula = "y ~ x", se = F) +
  facet_wrap(~SUBPARCELA, scales = "free_y")

plotly::ggplotly()



####data limpia  (aun faltan filtrar las fechas)

data_clean <- list(a, b, c, c1, c2, c3) %>% 
  bind_rows()
save(data_clean, file = "output/tablas/data_clean.rda")

# setwd("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos")
# write.xlsx(data_clean, "data_clean.xlsx")