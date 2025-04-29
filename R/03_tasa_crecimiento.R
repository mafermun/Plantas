#librerias ----
library(tidyverse)
library(readxl)
library(xlsx)
library(broom)
library(RColorBrewer)
library(stats)

# Funciones ----
modelo_lineal <- function(.x, frm = altura ~ fecha, stat = c("tidy", "glance", "augment"), ...){
  stat <- stat[1]
  ajuste_lineal <- lm(frm, data = .x) 
  
  if(stat == "glance") return(glance(ajuste_lineal))
  if(stat == "tidy") return(tidy(ajuste_lineal))
  return(augment(ajuste_lineal))
}

modelo_exponencial <- function(.x, stat = c("tidy", "glance", "augment"), ...){
  stat <- stat[1]
  ajuste_exponencial <- lm(log(altura) ~ fecha, data = .x) 
  
  if(stat == "glance") return(glance(ajuste_exponencial))
  if(stat == "tidy") return(tidy(ajuste_exponencial))
  return(augment(ajuste_exponencial))
}

# corregir codigos
code_fix <- function(x, codes = codigos){
  if(is.numeric(x)) return(x)
  recode(x, !!!codes) %>% 
    parse_number()
}

# # Datos ----
# codigos <- c( "D" = "-9", "DE" = "-10", "NP" = "-11", "DC" = "-12", "NM" = "-13")
# 
# dataA <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v9 19-11-2024.xlsx",
#                     sheet = "A", range = "A1:AY50") %>% 
#   
#   select(-3,-4, -17) %>% 
#   dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, starts_with("Altura")) %>% 
#   mutate(across(starts_with("Altura"), 
#                 .fns = code_fix)) %>%
#   pivot_longer(cols = starts_with("Altura"), 
#                names_to = "columna", values_to = "altura") %>% 
#   separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
#   mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
#          fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
#   rename(Planta = `Planta numero`) %>% 
#   filter(altura > 0) %>%
#   select(-variable)
# 
# dataB <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v9 19-11-2024.xlsx",
#                     sheet = "B", range = "A1:Q36") %>% 
#   select(-3,-4) %>% 
#   dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, starts_with("Altura")) %>% 
#   mutate(across(starts_with("Altura"), 
#                 .fns = code_fix)) %>%
#   pivot_longer(cols = starts_with("Altura"), 
#                names_to = "columna", values_to = "altura") %>% 
#   separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
#   mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
#          fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
#   rename(Planta = `Planta numero`) %>% 
#   filter(altura > 0) %>% 
#   select(-variable)
# 
# dataC <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v9 19-11-2024.xlsx",
#                     sheet = "C", range = "A1:Q69") %>% 
#   select(-3,-4) %>% 
#   dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, starts_with("Altura")) %>% 
#   mutate(across(starts_with("Altura"), 
#                 .fns = code_fix)) %>%
#   pivot_longer(cols = starts_with("Altura"), 
#                names_to = "columna", values_to = "altura") %>% 
#   separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
#   mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
#          fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
#   rename(Planta = `Planta numero`) %>% 
#   filter(altura > 0) %>% 
#   select(-variable)
# 
# data_control1 <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v9 19-11-2024.xlsx",
#                             sheet = "C1", range = "A1:J35") %>% 
#   select(-9) %>% 
#   dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, starts_with("Altura")) %>% 
#   mutate(across(starts_with("Altura"), 
#                 .fns = code_fix)) %>%
#   pivot_longer(cols = starts_with("Altura"), 
#                names_to = "columna", values_to = "altura") %>% 
#   separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
#   mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
#          fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
#   rename(Planta = `Planta numero`) %>% 
#   filter(altura > 0) %>% 
#   select(-variable)
# 
# data <- bind_rows(data_control1, dataA, dataB, dataC)
# 
# plantasSirven <- data %>% 
#   filter(!fecha %in% c("2022-10-20", "2022-10-27", "2023-04-15", 
#                        "2023-04-06", "2023-07-15")) %>% 
#   count(PARCELA, SUBPARCELA, Planta) %>% 
#   filter(n >= 3)
# 
# data_clean <- data %>%
#   filter(!fecha %in% c("2022-10-20", "2022-10-27", "2023-04-15", 
#                        "2023-04-06", "2023-07-15")) %>% 
#   semi_join(plantasSirven, by = c("PARCELA", "SUBPARCELA", "Planta"))
# 
# setwd("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos")
# write.xlsx(data_clean, "data_clean.xlsx")
 

load("output/tablas/data_clean.rda")


# Modelos ----

mdl_tidy_altura <- group_by(data_clean, plot, treatment, SUBPARCELA, Planta) %>% 
  group_modify(modelo_lineal, frm = altura ~ fecha, stat = "tidy", .keep = TRUE) %>% 
  rename(Slope = `estimate`)
mdl_tidy_altura %>% 
  filter(term == "fecha", 
         p.value < 0.05
  ) %>% 
  ungroup() %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  view()

mdl_exp <- group_by(data_clean, plot, SUBPARCELA, Planta) %>% 
  group_modify(modelo_exponencial, stat = "tidy", .keep = TRUE) %>% 
  rename(Slope = 'estimate')

# Graficos ----

mdl_tidy_altura %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  ggplot(aes(factor(treatment), Slope,
             colour = factor(plot))) +
  geom_boxplot() +
  facet_wrap(~plot, scales = "free_x") +
  labs(title = "Tasa de crecimiento (cm/día)")

plotly::ggplotly()



mdl_exp%>% 
  filter(term == 'fecha', p.value < 0.1) %>% 
  ggplot(aes(factor(SUBPARCELA), Slope)) +
  geom_boxplot() +
  facet_wrap(~plot, scales = "free_x")

mdl_exp %>% 
  filter(term == 'fecha', 
         p.value < 0.05
  ) %>% 
  ungroup() %>% 
  count(plot, SUBPARCELA)

### por tratmiento ----

drought <- data_clean %>% 
  filter(treatment == "Drought")

drought <- data_clean %>% 
  filter(treatment == "Drought")

ggplot(drought, aes(x = fecha, y = altura, color = factor(Planta))) +
  geom_line() +
  facet_wrap(plot~SUBPARCELA, scales = "free_x")


#### modelo lineal altura ----

mdl_tidy_altura <- group_by(data_clean, treatment, plot, SUBPARCELA, Planta) %>% 
  group_modify(modelo_lineal, frm = altura ~ fecha, stat = "tidy", .keep = TRUE) %>% 
  rename(Slope = `estimate`)
view(mdl_tidy)


mdl_tidy_altura %>% 
  filter(term == "fecha", 
         p.value < 0.05
  ) %>% 
  ungroup() %>% 
  count(plot, SUBPARCELA, Planta) %>% 
  view()

mdl_tidy_altura %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  ggplot(aes(factor(SUBPARCELA), Slope,
             colour = factor(plot))) +
  geom_boxplot() +
  facet_grid(treatment~plot, scales = "free_x") 

plotly::ggplotly()




tmp <- mdl_tidy_altura %>% 
  ungroup() %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  select(treatment, Slope) %>%
  group_by(treatment) %>% 
  mutate(id = 1:n()) %>% 
  pivot_wider(id_cols = id, 
              names_from = treatment, 
              values_from = Slope) 

test <- ks.test(tmp$Control,tmp$Drought)
text <- test[2]


mdl_tidy_altura %>% 
  ungroup() %>% 
  filter(term == 'fecha', p.value < 0.05) %>% 
  select(treatment, plot, Slope) %>%
  ggplot() +
  aes(Slope, color= plot, scales = "free") +
  geom_density() +
  annotate(geom = "label",
           label = 'KS test\nD = 0.48835\np-value = 0.002324',
            x = 0.4,
            y = 18,
            label.padding = unit(0.65, "lines"),
            alpha = 0.45,
            size = 3,
            color = "black") +
  labs(title = "Tasa de crecimiento (cm/día)") +
  facet_grid(~ treatment)



save(mdl_tidy_altura, file = "output/tablas/modelolineal_altura.rda")
