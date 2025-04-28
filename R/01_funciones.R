code_fix <- function(x, codes = codigos){
  if(is.na(x)) return(NA_real_)
  if(is.numeric(x)) return(x)
  recode(x, !!!codes) %>% 
    parse_number()
}

code_fix_map <- function(x) map_dbl(x, code_fix)

idk <- function(x){
  
}

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
