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

# Modelo SEIR
sir <- function(time, state, parameters) {
  R_0 <- parameters$R_0
  T_inf <- parameters$T_inf
  T_inc <- parameters$T_inc
  
  tiempos_int <- parameters$tiempos_int
  efectos_int <- parameters$efectos_int
  
  state <- as.list(state)
  S <- state$S
  E <- state$E
  I <- state$I
  R <- state$R
  t <- state$t
  
  R_t <- R_0
  for(i in 1:length(tiempos_int)){
    if(t >= tiempos_int[i]){
      R_t <- efectos_int[i]
    }
  }
  
  # Parametrización alternativa
  # beta <- R_t / T_inf
  # a <- 1/T_inc
  # gamma <- 1/T_inf
  
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
  # parametros <- list(R_0 = 3, T_inf = 2, T_inc = 5, tiempos_int = c(10, 20), efectos_int = c(0.8, 0.7))
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
                           tiempos_int = c(10, 20),
                           n_dias){
  # real <- real
  # T_inf <- 2
  # T_inc <- 4
  # x <- c(2.5, 2.2, 2, 1.5 , 1.2)
  # tiempos_int <- fechas_dias
  
  R_0 <- x[1]
  efectos_int <- x[-1]
  
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
                     tiempos_int = tiempos_int,
                     efectos_int = efectos_int)
  
  pred <- sir_simular(t_0 = t_0, parametros = parametros,
                      n_dias = n_dias, FUN = sir)
  
  ss <- pred %>%
    mutate(casos_acumulados = floor(pob*(I + R))) %>%
    select(dia, casos_acumulados) %>%
    left_join(real %>% select(dia, casos_nuevos), by = "dia") %>%
    mutate(casos_nuevos = replace(casos_nuevos, is.na(casos_nuevos), 0)) %>%
    mutate(casos_acumulados.real = cumsum(casos_nuevos)) %>%
    # mutate(casos_acumulados = replace(casos_acumulados, casos_acumulados < 0, 0),
    #        casos_acumulados.real = replace(casos_acumulados.real, casos_acumulados.real < 0, 0)) %>%
    mutate(diff2 = (casos_acumulados - casos_acumulados.real) ^ 2) %>%
    select(diff2) %>%
    unlist %>%
    sum
  
  return(ss)
}

encontrar_R_0 <- function(real, 
                          n_dias_ajuste,
                          dias_int,
                          T_inc =c(4.1, 5.2, 7.9),
                          T_inf = c(1.5, 2.9, 6),
                          pob = 135552447){
  # real = Tab
  # n_dias_ajuste = n_dias_ajuste
  # dias_int = fechas_dias
  # T_inc = T_inc
  # T_inf = T_inf
  # pob = pob

  cat("Formateando datos reales\n")
  real <- tibble(fecha = min(real$fecha) + 0:(n_dias_ajuste - 1),
         dia = 0:(n_dias_ajuste - 1)) %>%
    left_join(real, by = c("fecha", "dia")) %>%
    mutate(casos_nuevos = replace_na(casos_nuevos, 0)) %>%
    mutate(casos_acumulados = cumsum(casos_nuevos) + min(casos_acumulados,na.rm = TRUE) - casos_nuevos) %>%
    mutate(R_actuales = lag(casos_acumulados, 2, default = 0)) %>%
    mutate(I_actuales = casos_acumulados - R_actuales)
  
  # Optimizar en matriz de parámetros
  cat("Expandiendo matríz de búsqueda\n")
  Dat <- expand.grid(T_inc = T_inc, T_inf = T_inf) %>%
    as_tibble() %>%
    mutate(modelo = paste0("m", 1:length(T_inc)))
  cat("Estimando parámetros\n")
  Est <- Dat %>% pmap(function(T_inc, T_inf, modelo, real, pob, dias){
    # T_inc <- 4
    # T_inf <- 4
    # dias <- dias_int
    cat(T_inc, T_inf, "\n")
    # cat(">intentando optimización L-BFGS-B\n")
    # metodo <- "L-BFGS-B"
    # sombrero <- optim(par = c(1.5, rep(2, length(dias))),
    #                   fn = sir_optmizable,
    #                   method = "L-BFGS-B",
    #                   lower = c(1, rep(0.01, length(dias))),
    #                   upper = c(5, rep(5, length(dias))),
    #                   real = real, 
    #                   pob = pob,
    #                   n_dias = n_dias_ajuste,
    #                   T_inc = T_inc,
    #                   T_inf = T_inf,
    #                   tiempos_int = dias)$par
    
    # optim(par = c(1, rep(2, length(dias))),
    #       fn = sir_optmizable,
    #       method = "L-BFGS-B",
    #       lower = c(0.5, rep(0.01, length(dias))),
    #       upper = c(5, rep(5, length(dias))),
    #       real = real,
    #       pob = pob,
    #       n_dias = n_dias_ajuste,
    #       T_inc = T_inc,
    #       T_inf = T_inf,
    #       tiempos_int = dias)
    
    cat(">Aproximando con SANN\n")
    metodo <- "SANN"
    sombrero <- optim(par = c(2, rep(1, length(dias))),
          fn = sir_optmizable,
          method = "SANN",
          real = real,
          pob = pob,
          n_dias = n_dias_ajuste,
          T_inf = T_inf,
          T_inc = T_inc,
          tiempos_int = dias,
          control = list(trace = 1,
                         maxit = 1e4))$par
    
    # if(sombrero[1] > 0){
    #   cat(">Aproximando con SANN\n")
    #   metodo <- "SANN"
    #   sombrero <- optim(par = sombrero,
    #                     fn = sir_optmizable,
    #                     method = "SANN",
    #                     real = real,
    #                     pob = pob,
    #                     n_dias = n_dias_ajuste,
    #                     T_inf = T_inf,
    #                     T_inc = T_inc,
    #                     tiempos_int = dias)$par
    # }
    # if(any(sombrero[-1] < 0)){
    #   cat("===", sombrero, "===\n")
    #   cat("--Falló\n")
    #   sombrero <- rep(NA, length(sombrero))
    #   metodo <- NA
    # }
    
    if(sombrero[1] < 1 || any(sombrero[-1] < 0)){
      cat("===", sombrero, "===\n")
      cat("--Falló\n")
      sombrero <- rep(NA, length(sombrero))
      metodo <- NA
    }
    
    list(modelo = modelo,
         T_inc = T_inc,
         T_inf = T_inf,
         R_0 = sombrero[1],
         tiempos_int = dias,
         efectos_int = sombrero[-1],
         metodo = metodo)
    # set_names(sombrero,
    #           c("R0", paste0(rep("f", length(sombrero) - 1), 1:(length(sombrero) - 1))))  %>%
    #   bind_rows() %>%
    #   bind_cols(metodo = metodo)
  }, real = real, pob = pob, dias = dias_int)
  
  # Dat %>%
  #   bind_cols(Est)
  Est
}

simular_multiples_modelos <- function(modelos, FUN, real, pob, n_dias){
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
    map_dfr(function(l, n_dias, FUN, t_0){
      # 5     2.5  4.04  0.262
      cat(l$modelo, "\n")
      parametros <- list(R_0 = l$R_0, T_inf = l$T_inf, T_inc = l$T_inc,
                         tiempos_int = l$tiempos_int,
                         efectos_int = l$efectos_int)
      
      sir_simular(t_0 = t_0, parametros = parametros, n_dias = n_dias, FUN = FUN) %>%
        mutate(casos_acumulados = floor(pob * (I + R))) %>%
        select(dia, casos_acumulados) %>%
        mutate(modelo = l$modelo)
    }, n_dias = n_dias , FUN = FUN, t_0 = t_0)
  sims
}