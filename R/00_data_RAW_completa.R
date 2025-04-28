library(tidyverse)
library(readxl)
source("R/funciones.R")

codigos <- c( "D" = "-9", "DE" = "-10", "NP" = "-11", "DC" = "-12", "NM" = "-13")

A <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v11 21-12-2024.xlsx",
                    sheet = "A", range = "A1:BS50") %>% 
  
  select(-3,-4) %>% 
  dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, 
                starts_with("Altura"),
                starts_with("Numero"),
                starts_with("Estado"),
                starts_with("Flores"),
                starts_with("Observaciones")) %>% 
  mutate(across(starts_with("Altura"), .fns = code_fix_map),
         across(starts_with("Numero"), .fns = code_fix_map),
         across(starts_with("Estado"), .fns = code_fix_map),
         across(starts_with("Flores"), .fns = code_fix_map),
         across(`Altura_del_tallo_cm_ 26/07/2022`:`Observaciones_ 05/12/2024`, 
                .fns = as.character)) %>%
  pivot_longer(cols = `Altura_del_tallo_cm_ 26/07/2022`: `Observaciones_ 05/12/2024`, 
               names_to = "columna", values_to = "valor") %>% 
  separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
  mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
  pivot_wider(id_cols = c(PARCELA, SUBPARCELA, `Planta numero`, fecha),
              names_from = variable,
              values_from = valor) %>% 
  rename(Planta = `Planta numero`,
         altura = `Altura del tallo cm`,
         hojas = `Numero de hojas`,
         estado = `Estado de la planta`) %>%
  mutate(altura = as.numeric(altura),
         hojas = as.numeric(hojas),
         estado = as.integer(estado),
         `Flores frutos` = as.integer(`Flores frutos`),
         estado = case_when(estado == -9 ~ 0,
                            estado <= -10 ~ NA_integer_, 
                            TRUE ~ estado),
         `Flores frutos` = case_when(`Flores frutos` == -9 ~ 0,
                                     `Flores frutos` <= -10 ~ NA_integer_, 
                                     TRUE ~ `Flores frutos`),
         estado = as.logical(as.numeric(estado)),
         `Flores frutos` = as.logical(as.numeric(`Flores frutos`)))
  
B <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v11 21-12-2024.xlsx",
                sheet = "B", range = "A1:BR36") %>% 
  select(-3,-4) %>% 
  dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, 
                starts_with("Altura"), 
                starts_with("Numero"),
                starts_with("Estado"),
                starts_with("Flores"),
                starts_with("Observaciones")) %>% 
  mutate(across(starts_with("Altura"), .fns = code_fix_map), 
         across(starts_with("Numero"), .fns = code_fix_map),
         across(starts_with("Estado"), .fns = code_fix_map),
         across(starts_with("Flores"), .fns = code_fix_map), 
         across(`Altura_del_tallo_cm_ 29/07/2022`:`Observaciones_ 06/12/2024`, 
                .fns = as.character)) %>% 
  pivot_longer(cols = `Altura_del_tallo_cm_ 29/07/2022`:`Observaciones_ 06/12/2024`, 
               names_to = "columna", values_to = "valor", 
               values_ptypes = list(valor = character())) %>% 
  separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
  mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
  pivot_wider(id_cols = c(PARCELA, SUBPARCELA, `Planta numero`, fecha),
              names_from = variable, 
              values_from = valor) %>% 
  rename(Planta = `Planta numero`,
         altura = `Altura del tallo cm`,
         hojas = `Numero de hojas`,
         estado = `Estado de la planta`) %>% 
  mutate(altura = as.numeric(altura),
         hojas = as.numeric(hojas),
         estado = as.integer(estado),
         `Flores frutos` = as.integer(`Flores frutos`),
         estado = case_when(estado == -9 ~ 0,
                            estado <= -10 ~ NA_integer_, 
                            TRUE ~ estado),
         `Flores frutos` = case_when(`Flores frutos` == -9 ~ 0,
                                     `Flores frutos` <= -10 ~ NA_integer_, 
                                     TRUE ~ `Flores frutos`),
         estado = as.logical(as.numeric(estado)),
         `Flores frutos` = as.logical(as.numeric(`Flores frutos`)))

C <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v11 21-12-2024.xlsx",
                    sheet = "C", range = "A1:BR72") %>% 
  select(-3,-4) %>% 
  dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, 
                starts_with("Altura"),
                starts_with("Numero"),
                starts_with("Estado"),
                starts_with("Flores"),
                starts_with("Observaciones")) %>% 
  mutate(across(starts_with("Altura"), .fns = code_fix_map),
         across(starts_with("Numero"), .fns = code_fix_map),
         across(starts_with("Estado"), .fns = code_fix_map),
         across(starts_with("Flores"), .fns = code_fix_map),
         across(`Altura_del_tallo_cm_ 10/08/2022`:`Observaciones_ 07/12/2024`, 
                .fns = as.character)) %>%
  pivot_longer(cols = `Altura_del_tallo_cm_ 10/08/2022`: `Observaciones_ 07/12/2024`, 
               names_to = "columna", values_to = "valor") %>% 
  separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
  mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
  pivot_wider(id_cols = c(PARCELA, SUBPARCELA, `Planta numero`, fecha),
              names_from = variable,
              values_from = valor) %>% 
  rename(Planta = `Planta numero`,
         altura = `Altura del tallo cm`,
         hojas = `Numero de hojas`,
         estado = `Estado de la planta`) %>%
  mutate(altura = as.numeric(altura),
         hojas = as.numeric(hojas),
         estado = as.integer(estado),
         `Flores frutos` = as.integer(`Flores frutos`),
         estado = case_when(estado == -9 ~ 0,
                            estado <= -10 ~ NA_integer_, 
                            TRUE ~ estado),
         `Flores frutos` = case_when(`Flores frutos` == -9 ~ 0,
                                     `Flores frutos` <= -10 ~ NA_integer_, 
                                     TRUE ~ `Flores frutos`),
         estado = as.logical(as.numeric(estado)),
         `Flores frutos` = as.logical(as.numeric(`Flores frutos`)))

C1 <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v11 21-12-2024.xlsx",
                            sheet = "C1", range = "A1:BC40") %>% 
  select(-3,-4) %>% 
  dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, 
                starts_with("Altura"),
                starts_with("Numero"),
                starts_with("Estado"),
                starts_with("Flores"),
                starts_with("Observaciones")) %>% 
  mutate(across(starts_with("Altura"), .fns = code_fix_map),
         across(starts_with("Numero"), .fns = code_fix_map),
         across(starts_with("Estado"), .fns = code_fix_map),
         across(starts_with("Flores"), .fns = code_fix_map),
         across(`Altura_del_tallo_cm_ 16/09/2022`:`Observaciones_ 06/11/2024`, 
                .fns = as.character)) %>%
  pivot_longer(cols = `Altura_del_tallo_cm_ 16/09/2022`: `Observaciones_ 06/11/2024`, 
               names_to = "columna", values_to = "valor") %>% 
  separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
  mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
  pivot_wider(id_cols = c(PARCELA, SUBPARCELA, `Planta numero`, fecha),
              names_from = variable,
              values_from = valor) %>% 
  rename(Planta = `Planta numero`,
         altura = `Altura del tallo cm`,
         hojas = `Numero de hojas`,
         estado = `Estado de la planta`) %>%
  mutate(altura = as.numeric(altura),
         hojas = as.numeric(hojas),
         estado = as.integer(estado),
         `Flores frutos` = as.integer(`Flores frutos`),
         estado = case_when(estado == -9 ~ 0,
                            estado <= -10 ~ NA_integer_, 
                            TRUE ~ estado),
         `Flores frutos` = case_when(`Flores frutos` == -9 ~ 0,
                                     `Flores frutos` <= -10 ~ NA_integer_, 
                                     TRUE ~ `Flores frutos`),
         estado = as.logical(as.numeric(estado)),
         `Flores frutos` = as.logical(as.numeric(`Flores frutos`)))

C2 <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v11 21-12-2024.xlsx",
                 sheet = "C2", range = "A1:AN44") %>% 
  select(-3,-4) %>% 
  dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, 
                starts_with("Altura"),
                starts_with("Numero"),
                starts_with("Estado"),
                starts_with("Flores"),
                starts_with("Observaciones")) %>% 
  mutate(across(starts_with("Altura"), .fns = code_fix_map),
         across(starts_with("Numero"), .fns = code_fix_map),
         across(starts_with("Estado"), .fns = code_fix_map),
         across(starts_with("Flores"), .fns = code_fix_map),
         across(`Altura_del_tallo_cm_ 22/09/2022`:`Observaciones_ 03/12/2024`, 
                .fns = as.character)) %>%
  pivot_longer(cols = `Altura_del_tallo_cm_ 22/09/2022`: `Observaciones_ 03/12/2024`, 
               names_to = "columna", values_to = "valor") %>% 
  separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
  mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
  pivot_wider(id_cols = c(PARCELA, SUBPARCELA, `Planta numero`, fecha),
              names_from = variable,
              values_from = valor) %>% 
  rename(Planta = `Planta numero`,
         altura = `Altura del tallo cm`,
         hojas = `Numero de hojas`,
         estado = `Estado de la planta`) %>%
  mutate(altura = as.numeric(altura),
         hojas = as.numeric(hojas),
         estado = as.integer(estado),
         `Flores frutos` = as.integer(`Flores frutos`),
         estado = case_when(estado == -9 ~ 0,
                            estado <= -10 ~ NA_integer_, 
                            TRUE ~ estado),
         `Flores frutos` = case_when(`Flores frutos` == -9 ~ 0,
                                     `Flores frutos` <= -10 ~ NA_integer_, 
                                     TRUE ~ `Flores frutos`),
         estado = as.logical(as.numeric(estado)),
         `Flores frutos` = as.logical(as.numeric(`Flores frutos`)))

C3 <- read_excel("C:/Users/USER/Desktop/Universidad/AWESOME/analisis_datos/AnalisisPlantas/datos/DATOS_CRECIMIENTO v11 21-12-2024.xlsx",
                 sheet = "C3", range = "A1:AI40") %>% 
  select(-3,-4) %>% 
  dplyr::select(PARCELA, SUBPARCELA, `Planta numero`, 
                starts_with("Altura"),
                starts_with("Numero"),
                starts_with("Estado"),
                starts_with("Flores"),
                starts_with("Observaciones")) %>% 
  mutate(across(starts_with("Altura"), .fns = code_fix_map),
         across(starts_with("Numero"), .fns = code_fix_map),
         across(starts_with("Estado"), .fns = code_fix_map),
         across(starts_with("Flores"), .fns = code_fix_map),
         across(`Altura_del_tallo_cm_ 22/09/2022`:`Observaciones_ 04/12/2024`, 
                .fns = as.character)) %>%
  pivot_longer(cols = `Altura_del_tallo_cm_ 22/09/2022`: `Observaciones_ 04/12/2024`, 
               names_to = "columna", values_to = "valor") %>% 
  separate(columna, into = c("variable", "fecha"), sep = " ") %>% 
  mutate(variable = str_replace_all(variable, "_", " ") %>% str_trim(), 
         fecha = as.Date(fecha, format = "%d/%m/%Y")) %>% 
  pivot_wider(id_cols = c(PARCELA, SUBPARCELA, `Planta numero`, fecha),
              names_from = variable,
              values_from = valor) %>% 
  rename(Planta = `Planta numero`,
         altura = `Altura del tallo cm`,
         hojas = `Numero de hojas`,
         estado = `Estado de la planta`) %>%
  mutate(altura = as.numeric(altura),
         hojas = as.numeric(hojas),
         estado = as.integer(estado),
         `Flores frutos` = as.integer(`Flores frutos`),
         estado = case_when(estado == -9 ~ 0,
                            estado <= -10 ~ NA_integer_, 
                           TRUE ~ estado),
         `Flores frutos` = case_when(`Flores frutos` == -9 ~ 0,
                                     `Flores frutos` <= -10 ~ NA_integer_, 
                            TRUE ~ `Flores frutos`),
         estado = as.logical(as.numeric(estado)),
         `Flores frutos` = as.logical(as.numeric(`Flores frutos`)))

data_full <- list(Drought.A=A,Drought.B=B,Drought.C=C,Control.A=C1,Control.B=C2,Control.C=C3) %>% 
bind_rows(.id="name") %>% 
  separate(name, into = c("treatment", "plot")) %>% 
  select(-3)

save(data_full, file = "output/tablas/dataRAW_vegetation_full.rda")
# data_full <- save("output/tablas/dataRAW_vegetation_full.xlsx")

load("output/tablas/dataRAW_vegetation_full.rda")
