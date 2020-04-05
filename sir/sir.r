library(tidyverse)
library(deSolve)
# https://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
# https://gabgoh.github.io/COVID/index.html

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
casos_confirmados / N_pob

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
# init <- c(S = (N_pob - casos_confirmados)/N_pob, I = casos_confirmados / N_pob, R = 0.0)
init <- c(S = (N_pob - casos_confirmados)/N_pob,
          E = 0,
          I = casos_confirmados / N_pob,
          R = 0.0)
## beta: infection parameter; gamma: recovery parameter
# parameters <- c(beta = 0.125, gamma =1/20)
parameters <- c(R_t = 2.2, T_inf = 3, T_inc = 5)
## Time frame 
times <- seq(0, 365, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
pred <- ode(y = init, times = times, func = sir, parms = parameters)
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

