library(tidyverse)
library(deSolve)
# https://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
# https://gabgoh.github.io/COVID/index.html
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

# Create an SIR function
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # # SIR
    # dS <- -beta * S * I
    # dI <-  beta * S * I - gamma * I
    # dR <-                 gamma * I
    # return(list(c(dS, dI, dR)))
    
    # SEIR
    dS <- -(R_t / T_inf) * (I * S)
    dE <- (R_t / T_inf) * (I * S) - ((1 / T_inc) * E)
    dI <- ((1 / T_inc) * E) - ((1 / T_inf) * I)
    dR <- (1 / T_inf) * I
    
    return(list(c(dS, dE, dI, dR)))
  })
}

args <- list(tabla_sintomas = "../datos/ssa_dge/tabla_casos_confirmados.csv",
             dias_retraso = 15)

# Leer 
Tab <- read_csv(args$tabla_sintomas,
                col_types = cols(estado = col_character(),
                                 sexo = col_character(),
                                 edad = col_number(),
                                 fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                 procedencia = col_character(),
                                 fecha_llegada = col_date(format = "%d/%m/%Y")))
Tab <- table(Tab$fecha_sintomas)
Tab <- tibble(fecha = names(Tab) %>% as.Date("%Y-%m-%d"),
              casos_nuevos = as.vector(Tab)) %>%
  mutate(casos_acumulados = cumsum(casos_nuevos)) %>%
  filter(fecha <= (max(fecha) - args$dias_retraso)) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))
Tab
max(Tab$fecha) - min(Tab$fecha)

sir_simular <- function(t_0, parametros, n_dias, FUN = sir){
  # funcion
  FUN <- match.fun(FUN)
  
  # Definir dias
  dias <- seq(0, n_dias, by = 1)
  
  # Usar ode para resolver, convertir a tibble
  pred <- ode(y = t_0, times = dias, func = FUN, parms = parametros)
  pred <- tibble(dia = pred[,"time"],
                 S = pred[,"S"],
                 E = pred[,'E'],
                 I = pred[,"I"],
                 R = pred[,"R"])
  
  return(pred)
}

sir_optmizable <- function(R_0, real, pob, T_inf = 3, T_inc = 5){
  # Definir Condiciones iniciales
  casos_confirmados <- real$casos_acumulados[real$dia == 0]
  t_0 <- c(S = (pob - casos_confirmados) / pob,
           E = 0,
           I = casos_confirmados / pob,
           R = 0.0)
  # Parámetros
  parametros <- c(R_t = R_0, T_inf = T_inf, T_inc = T_inc)
  
  pred <- sir_simular(t_0 = t_0, parametros = parametros,
                      n_dias = max(real$dia), FUN = sir)
  
  ss <- pred %>%
    filter(dia <= 49) %>%
    mutate(casos = I*pob) %>%
    select(dia, casos) %>%
    left_join(real %>% select(dia, casos_nuevos), by = "dia") %>%
    mutate(casos_nuevos = replace(casos_nuevos, is.na(casos_nuevos), 0)) %>%
    mutate(diff2 = (casos_nuevos - casos) ^ 2) %>%
    select(diff2) %>%
    unlist %>%
    sum
  
  return(ss)
}

pob <- 135552447

R_0_optim <- optim(2, fn = sir_optmizable,
                   method = "Brent", lower = 0.5, upper = 5,
                   real = Tab, pob = pob, T_inf = 3, T_inc = 5)
R_0_optim


Tab
p1 <- pred %>%
  pivot_longer(-dia) %>%
  filter(name != "S") %>%
  filter(name != "R") %>%
  ggplot(aes(x = dia, y = N_pob * value, group = name)) +
  geom_line(aes(col = name)) +
  xlim(c(0,50)) +
  ylim(c(0,1000)) +
  theme(panel.background = element_blank())
p1

