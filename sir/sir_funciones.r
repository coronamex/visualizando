# (C) Copyright 2020 Sur Herrera Paredes

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(tidyverse)
library(deSolve)
# https://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
# https://gabgoh.github.io/COVID/index.html
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

# Create an SIR function
sir <- function(time, state, parameters) {
  R_0 <- parameters$R_0
  T_inf <- parameters$T_inf
  T_inc <- parameters$T_inc
  T_int1 <- parameters$T_int1
  Int_f1 <- parameters$Int_f1
  
  state <- as.list(state)
  S <- state$S
  E <- state$E
  I <- state$I
  R <- state$R
  t <- state$t
  
  if(t >= T_int1){
    R_t <- Int_f1 * R_0
  }else{
    R_t <- R_0
  }
  
  # Parametrización alternativa
  # beta <- R_t / T_inf
  # a <- 1/T_inc
  # gamma <- 1/D_inf
  
  # SEIR
  dS <- -(R_t / T_inf) * (I * S)
  dE <- (R_t / T_inf) * (I * S) - ((1 / T_inc) * E)
  dI <- ((1 / T_inc) * E) - ((1 / T_inf) * I)
  dR <- (1 / T_inf) * I
  dt <- 1
  
  return(list(c(dS, dE, dI, dR, dt)))
}


sir_simular <- function(t_0, parametros, n_dias, FUN = sir){
  # t_0 <- c(S = (pob - 2)/pob,
  #          E = 1/pob,
  #          I = 1/pob,
  #          R = 0,
  #          t = 0)
  # parametros <- list(R_0 = 2.5, T_inf = 2, T_inc = 5, T_int1 = 30, Int_f1 = 0.3)
  # n_dias <- 100
  # FUN <- sir
  
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

sir_optmizable <- function(x, real, pob,
                           T_inf = 3, T_inc = 5,
                           T_int1 = 0){
  # real <- Tab
  # T_inf = 1.5
  # T_inc = 4.1
  # x <- c(2, 0.8)
  # T_int1 <- 17
  
  R_0 <- x[1]
  Int_f1 <- x[2]
  
  # Definir Condiciones iniciales
  I_actuales <- real$I_actuales[real$dia == 0]
  R_actuales <- real$R_actuales[real$dia == 0]
  t_0 <- c(S = (pob - I_actuales - R_actuales) / pob,
           E = 0,
           I = I_actuales / pob,
           R = R_actuales / pob,
           t = 0)
  # Parámetros
  parametros <- list(R_0 = R_0, T_inf = T_inf, T_inc = T_inc,
                     T_int1 = T_int1, Int_f1 = Int_f1)
  
  pred <- sir_simular(t_0 = t_0, parametros = parametros,
                      n_dias = max(real$dia), FUN = sir)
  
  ss <- pred %>%
    mutate(casos_acumulados = floor(pob*(I + R))) %>%
    select(dia, casos_acumulados) %>%
    left_join(real %>% select(dia, casos_nuevos), by = "dia") %>%
    mutate(casos_nuevos = replace(casos_nuevos, is.na(casos_nuevos), 0)) %>%
    mutate(casos_acumulados.real = cumsum(casos_nuevos)) %>%
    mutate(diff2 = (casos_acumulados - casos_acumulados.real) ^ 2) %>%
    select(diff2) %>%
    unlist %>%
    sum
  
  return(ss)
}

encontrar_R_0 <- function(real, 
                          n_dias_ajuste,
                          fecha1_dia,
                          T_inc =c(4.1, 5.2, 7.9),
                          T_inf = c(1.5, 2.9, 6),
                          pob = 135552447){
  # dias_retraso <- 16
  # periodo_ajuste = 100
  # T_inc = c(4.1, 5.2, 7.9)
  # T_inf = c(1.5, 2.9, 6)
  # pob = 127792286
  # real <- Tab
  # n_dias_ajuste
  # fecha1_dia

  cat("Formateando datos reales\n")
  real <- tibble(fecha = min(real$fecha) + 0:(n_dias_ajuste - 1),
         dia = 0:(n_dias_ajuste - 1)) %>%
    left_join(real, by = c("fecha", "dia")) %>%
    mutate(casos_nuevos = replace_na(casos_nuevos, 0)) %>%
    mutate(casos_acumulados = cumsum(casos_nuevos)) %>%
    mutate(R_actuales = lag(casos_acumulados, 2, default = 0)) %>%
    mutate(I_actuales = casos_acumulados - R_actuales)
  
  # Optimizar en matriz de parámetros
  cat("Expandiendo matríz de búsqueda\n")
  Dat <- expand.grid(T_inc = T_inc, T_inf = T_inf) %>%
    as_tibble() %>%
    mutate(modelo = paste0("m", 1:length(T_inc)))
  cat("Estimando parámetros\n")
  Est <- Dat %>% pmap_dfr(function(T_inc, T_inf, modelo, real, pob, dia1){
    # T_inc <- 4.1
    # T_inf <- 1.5
    cat(T_inc, T_inf, "\n")
    cat(">intentando optimización L-BFGS-B\n")
    metodo <- "L-BFGS-B"
    sombrero <- optim(par = c(3, 0.8),
                      fn = sir_optmizable,
                      # method = "SANN",
                      method = "L-BFGS-B",
                      lower = c(0.01,0.01),
                      upper = c(10,1),
                      real = real, 
                      pob = pob,
                      T_inf = T_inf,
                      T_inc = T_inc,
                      T_int1 = dia1)$par
    if(sombrero[1] < 1){
      cat(">Aproximando con SANN\n")
      sombrero <- optim(par = c(3, 0.8),
                        fn = sir_optmizable,
                        method = "SANN",
                        real = real, 
                        pob = pob,
                        T_inf = T_inf,
                        T_inc = T_inc,
                        T_int1 = dia1)$par
      if(sombrero[1] > 1 && sombrero[2] > 0 && sombrero[2] <= 1){
        metodo <- "SANN"
      }else{
        sombrero <- c(NA, NA)
      }
    }
    
    tibble(R_hat = sombrero[1],
           f1_hat = sombrero[2],
           metodo = metodo)
  }, real = real, pob = pob, dia1 = fecha1_dia)
  
  Dat %>%
    bind_cols(Est)
}

simular_multiples_modelos <- function(modelos, FUN, real, pob, n_dias,
                                      fecha1_dia){
  # modelos = R_hat
  # FUN = sir
  # real = Tab
  
  FUN <- match.fun(FUN)
  # Definir t_0
  real <- real %>%
    mutate(R_actuales = lag(casos_acumulados, 2, default = 0)) %>%
    mutate(I_actuales = casos_acumulados - R_actuales) 
  
  # Definir t_0
  I_actuales <- real$I_actuales[real$dia == 0]
  R_actuales <- real$R_actuales[real$dia == 0]
  t_0 <- c(S = (pob - I_actuales - R_actuales) / pob,
           E = 0,
           I = I_actuales / pob,
           R = R_actuales / pob,
           t = 0)
  
  sims <- modelos %>%
    # bind_cols(modelo = paste0("m", 1:nrow(modelos))) %>%
    pmap_dfr(function(T_inc, T_inf, modelo, R_hat, f1_hat, metodo, n_dias, FUN, t_0, dia1){
      # 5     2.5  4.04  0.262
      cat(modelo, "\n")
      parametros <- list(R_0 = R_hat, T_inf = T_inf, T_inc = T_inc,
                         T_int1 = dia1,
                         Int_f1 = f1_hat)
      
      sir_simular(t_0 = t_0, parametros = parametros, n_dias = n_dias, FUN = FUN) %>%
        mutate(casos_acumulados = floor(pob * (I + R))) %>%
        select(dia, casos_acumulados) %>%
        mutate(modelo = modelo)
    }, n_dias = n_dias , FUN = FUN, t_0 = t_0, dia1 = fecha1_dia)
  sims
}
