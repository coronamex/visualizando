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
    
    dS <- max(0, dS)
    dE <- max(0, dE)
    dI <- max(0, dI)
    dR <- max(0, dR)
    
    return(list(c(dS, dE, dI, dR)))
  })
}


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
    mutate(casos_acumulados = pob*(I + R)) %>%
    mutate(casos = c(casos_confirmados, diff(casos_acumulados))) %>%
    select(dia, casos) %>%
    left_join(real %>% select(dia, casos_nuevos), by = "dia") %>%
    mutate(casos_nuevos = replace(casos_nuevos, is.na(casos_nuevos), 0)) %>%
    mutate(diff2 = (casos_nuevos - casos) ^ 2) %>%
    select(diff2) %>%
    unlist %>%
    sum
  
  return(ss)
}

encontrar_R_0 <- function(Tab, dias_retraso = 15,
                          T_inc =c(4.1, 5.2, 7.9),
                          T_inf = c(1.5, 2.9, 6),
                          pob = 135552447){
  Tab <- Tab %>%
    filter(fecha <= (max(fecha) - dias_retraso)) %>%
    mutate(dia = as.numeric(fecha - min(fecha)))
  
  # Optimizar en matriz de parámetros
  Dat <- expand.grid(T_inc = T_inc, T_inf = T_inf) %>%
    as_tibble() 
  Dat$R_hat <- Dat %>%
    pmap_dbl(function(T_inc, T_inf, Tab, pob){
      optim(2, fn = sir_optmizable,
            method = "Brent", lower = 0.5, upper = 10,
            real = Tab, pob = pob, T_inf = T_inf, T_inc = T_inc)$par
    }, Tab = Tab, pob = pob)
  Dat
}

simular_multiples_modelos <- function(modelos, FUN, real, pob){
  FUN <- match.fun(FUN)
  # Definir t_0
  casos_confirmados <- real$casos_acumulados[real$dia == 0]
  t_0 <- c(S = (pob - casos_confirmados) / pob,
           E = 0,
           I = casos_confirmados / pob,
           R = 0.0)
  
  sims <- modelos %>%
    bind_cols(modelo = paste0("m", 1:nrow(modelos))) %>%
    pmap_dfr(function(T_inc, T_inf, R_hat, modelo, n_dias, FUN, t_0){
      parametros <- c(R_t = R_hat, T_inf = T_inf, T_inc = T_inc)
      sir_simular(t_0 = t_0, parametros = parametros, n_dias = n_dias, FUN = FUN) %>%
        mutate(casos_acumulados = floor(pob * (I + R))) %>%
        select(dia, casos_acumulados) %>%
        mutate(modelo = modelo)
    }, n_dias = max(real$dia)+2, FUN = FUN, t_0 = t_0)
  sims
}


args <- list(tabla_sintomas = "../datos/ssa_dge/tabla_casos_confirmados.csv",
             reportes_diarios = "../datos/ssa_dge/reportes_diarios.csv",
             dias_retraso = 15,
             dir_salida = "../sitio_hugo/static/imagenes/",
             fecha_hoy = "2020-04-09")

# Leer 
Tab <- read_csv(args$tabla_sintomas,
                col_types = cols(estado = col_character(),
                                 sexo = col_character(),
                                 edad = col_number(),
                                 fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                 procedencia = col_character()))
Tab <- table(Tab$fecha_sintomas)
Tab <- tibble(fecha = names(Tab) %>% as.Date("%Y-%m-%d"),
              casos_nuevos = as.vector(Tab)) %>%
  mutate(casos_acumulados = cumsum(casos_nuevos)) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))

# Parameters to make optimization
pob <- 135552447
T_inc <- c(2, 3, 4, 5, 6, 7, 8)
T_inf <- c(1, 2, 3, 4, 5, 6)

R_hat <- encontrar_R_0(Tab, dias_retraso = args$dias_retraso,
                       T_inc = T_inc, T_inf = T_inf, pob = pob)
R_hat

# Simular con parámetros estimados
sims <- simular_multiples_modelos(modelos = R_hat, FUN = sir, real = Tab, pob = pob)
ctds <- read_csv(args$reportes_diarios) %>%
  select(fecha, casos_acumulados) %>%
  mutate(modelo = "Confirmado")

p1 <- Tab %>%
  select(dia, casos_acumulados) %>%
  mutate(modelo = "real") %>%
  bind_rows(sims) %>%
  mutate(fecha = min(Tab$fecha) + dia) %>%
  select(-dia) %>%
  bind_rows(ctds) %>%
  mutate(grupo = "Estimado (SEIR)") %>%
  mutate(grupo = replace(grupo, modelo == "real", "Inicio de síntomas")) %>%
  mutate(grupo = replace(grupo, modelo == "Confirmado", "Confirmado")) %>%
  filter(fecha >= as.Date("2020-02-27")) %>%
  
  filter(modelo != "m2") %>%
  
  ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
  geom_line(aes(col = grupo, size = grupo)) +
  scale_color_brewer(palette = "Dark2", name = NULL) +
  scale_size_manual(values = c(2, 0.1, 2)) +
  guides(size = FALSE) +
  ylab("Casos acumulados") +
  xlab("Fecha") +
  AMOR::theme_blackbox() +
  # ylim(c(0,18000)) +
  # scale_y_log10() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "sir_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

summary(R_hat$R_hat)     
