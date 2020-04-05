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

N_pob <- 135552447
casos_confirmados <- 1
n_dias <- 200

# Condiciones iniciales
t_0 <- c(S = (N_pob - casos_confirmados)/N_pob,
          E = 0,
          I = casos_confirmados / N_pob,
          R = 0.0)
# Parámetros
parametros <- c(R_t = 2.2, T_inf = 3, T_inc = 5)
## Time frame 
dias <- seq(0, n_dias, by = 1)

# Usar ode para resolver, convertir a tibble
pred <- ode(y = t_0, times = dias, func = sir, parms = parametros)
pred <- tibble(dia = pred[,"time"],
       S = pred[,"S"],
       E = pred[,'E'],
       I = pred[,"I"],
       R = pred[,"R"])


p1 <- pred %>%
  pivot_longer(-dia) %>%
  filter(name != "S") %>%
  filter(name != "R") %>%
  ggplot(aes(x = dia, y = N_pob * value, group = name)) +
  geom_line(aes(col = name)) +
  theme(panel.background = element_blank())
p1

