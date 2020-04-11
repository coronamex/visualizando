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

encontrar_R_0 <- function(Tab, dias_retraso = 15,
                          periodo_ajuste = 28,
                          T_inc =c(4.1, 5.2, 7.9),
                          T_inf = c(1.5, 2.9, 6),
                          pob = 135552447,
                          fecha_final = Sys.Date(),
                          fecha1 = "2020-03-16" %>% as.Date(format = "%Y-%m-%d")){
  # dias_retraso <- 16 
  # periodo_ajuste = 100
  # T_inc = c(4.1, 5.2, 7.9)
  # T_inf = c(1.5, 2.9, 6)
  # pob = 127792286
  # fecha_final = Sys.Date()
  # fecha1 <- args$fecha1
  
  Tab <- Tab %>%
    mutate(R_actuales = lag(casos_acumulados, 2, default = 0)) %>%
    mutate(I_actuales = casos_acumulados - R_actuales) %>%
    filter(fecha <= (fecha_final - dias_retraso)) %>%
    filter(fecha >= (fecha_final - dias_retraso - periodo_ajuste + 1)) %>%
    mutate(dia = as.numeric(fecha - min(fecha))) 
  
  # Calcular dia de intervención
  dia1 <- Tab$dia[ Tab$fecha == fecha1 ]
  if(length(dia1) == 0){
    dia1 <- 0
  }
  
  # Optimizar en matriz de parámetros
  Dat <- expand.grid(T_inc = T_inc, T_inf = T_inf) %>%
    as_tibble() 
  Est <- Dat %>% pmap_dfr(function(T_inc, T_inf, Tab, pob, dia1){
    # T_inc <- 4.1
    # T_inf <- 1.5
    cat(T_inc, T_inf, "\n")
    sombrero <- optim(par = c(2, 0.8),
                      fn = sir_optmizable,
                      method = "L-BFGS-B",
                      lower = c(0.1,0),
                      upper = c(10,1),
                      real = Tab, 
                      pob = pob,
                      T_inf = T_inf,
                      T_inc = T_inc,
                      T_int1 = dia1)$par
    tibble(R_hat = sombrero[1],
           f1_hat = sombrero[2])
  }, Tab = Tab, pob = pob, dia1 = dia1)
   
  Dat %>%
    bind_cols(Est)
}

simular_multiples_modelos <- function(modelos, FUN, real, pob, n_dias = 43,
                                      fecha_final = Sys.Date(),
                                      fecha1 = "2020-03-16" %>% as.Date(format = "%Y-%m-%d")){
  # n_dias = args$dias_retraso + args$periodo_ajuste
  # modelos = R_hat
  # FUN = sir
  # real = Tab
  # fecha_final <- Sys.Date()
  # fecha1 = "2020-03-16" %>% as.Date(format = "%Y-%m-%d")
  
  FUN <- match.fun(FUN)
  # Definir t_0
  real <- real %>%
    mutate(R_actuales = lag(casos_acumulados, 2, default = 0)) %>%
    mutate(I_actuales = casos_acumulados - R_actuales) %>%
    filter(fecha >= (fecha_final - n_dias + 1)) %>%
    mutate(dia = as.numeric(fecha - min(fecha)))
  
  # Calcular dia de intervención
  dia1 <- real$dia[ real$fecha == fecha1 ]
  if(length(dia1) == 0){
    dia1 <- 0
  }
  
  I_actuales <- real$I_actuales[real$dia == 0]
  R_actuales <- real$R_actuales[real$dia == 0]
  t_0 <- c(S = (pob - I_actuales - R_actuales) / pob,
           E = 0,
           I = I_actuales / pob,
           R = R_actuales / pob,
           t = 0)
  
  sims <- modelos %>%
    bind_cols(modelo = paste0("m", 1:nrow(modelos))) %>%
    pmap_dfr(function(T_inc, T_inf, R_hat, f1_hat, modelo, n_dias, FUN, t_0, dia1){
      # 5     2.5  4.04  0.262
      cat(modelo, "\n")
      parametros <- list(R_0 = R_hat, T_inf = T_inf, T_inc = T_inc,
                         T_int1 = dia1,
                         Int_f1 = f1_hat)
      
      sir_simular(t_0 = t_0, parametros = parametros, n_dias = n_dias, FUN = FUN) %>%
        mutate(casos_acumulados = floor(pob * (I + R))) %>%
        select(dia, casos_acumulados) %>%
        mutate(modelo = modelo)
    }, n_dias = n_dias , FUN = FUN, t_0 = t_0, dia1 = dia1)
  sims
}


args <- list(tabla_sintomas = "../datos/ssa_dge/tabla_casos_confirmados.csv",
             reportes_diarios = "../datos/ssa_dge/reportes_diarios.csv",
             dias_retraso = 16,
             dir_salida = "../sitio_hugo/static/imagenes/",
             periodo_ajuste = 100,
             fecha1 = "2020-03-16")
args$fecha1 <- args$fecha1 %>% as.Date(format = "%Y-%m-%d")

# Leer 
Tab <- read_csv(args$tabla_sintomas,
                col_types = cols(estado = col_character(),
                                 sexo = col_character(),
                                 edad = col_number(),
                                 fecha_sintomas = col_date(format = "%Y-%m-%d"),
                                 procedencia = col_character(),
                                 fecha_llegada = col_date(format = "%Y-%m-%d")))
stop_for_problems(Tab)
Tab <- table(Tab$fecha_sintomas)
Tab <- tibble(fecha = names(Tab) %>% as.Date("%Y-%m-%d"),
              casos_nuevos = as.vector(Tab)) %>%
  mutate(casos_acumulados = cumsum(casos_nuevos)) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))

# Parameters to make optimization
pob <- 135552447
T_inc <- c(2, 3, 4, 5, 6, 7, 8)
T_inf <- c(1, 2, 3, 4, 5, 6)
pob <- 127792286
T_inc <- c(5,5.2,5.4)
T_inf <- c(2.5,3,3.5)

R_hat <- encontrar_R_0(Tab, dias_retraso = args$dias_retraso,
                       periodo_ajuste = args$periodo_ajuste,
                       T_inc = T_inc, T_inf = T_inf, pob = pob,
                       fecha1 = args$fecha1 )
R_hat

# Simular con parámetros estimados
sims <- simular_multiples_modelos(modelos = R_hat, FUN = sir, real = Tab, pob = pob,
                                  n_dias = args$dias_retraso + args$periodo_ajuste,
                                  fecha1 = args$fecha1)
sims
ctds <- read_csv(args$reportes_diarios) %>%
  select(fecha, casos_acumulados) %>%
  mutate(modelo = "Confirmado")

p1 <- Tab %>%
  filter(fecha >= (Sys.Date() - args$dias_retraso - args$periodo_ajuste + 1)) %>%
  mutate(dia = as.numeric(fecha - min(fecha))) %>%
  select(dia, casos_acumulados) %>%
  mutate(modelo = "real") %>%
  bind_rows(sims) %>%
  mutate(fecha = min(Tab$fecha) + dia)  %>%
  filter(fecha <= Sys.Date()) %>%
  select(-dia) %>%
  bind_rows(ctds) %>%
  mutate(grupo = "Estimado (SEIR)") %>%
  mutate(grupo = replace(grupo, modelo == "real", "Inicio de síntomas")) %>%
  mutate(grupo = replace(grupo, modelo == "Confirmado", "Confirmado")) %>%
  mutate(grupo = factor(grupo, levels = c("Confirmado", "Inicio de síntomas", "Estimado (SEIR)"))) %>%
  mutate(modelo = factor(modelo, levels = c("Confirmado", "real", unique(sims$modelo)))) %>%
  # filter(modelo != "m2") %>%
  # filter(grupo != "Inicio de síntomas") %>%
  
  ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
  geom_line(aes(col = grupo, size = grupo)) +
  geom_vline(xintercept = Sys.Date() - args$dias_retraso) +
  annotate("text", label = "Fin ajuste de curva",
           x = Sys.Date() - args$dias_retraso - 1.5,
           y = 5000, angle = 90,
           size = 6) +
  geom_vline(xintercept = args$fecha1, col = "red") +
  annotate("text", label = "Medidas de mitigación",
           x = args$fecha1 - 1.5,
           y = 5000, angle = 90,
           size = 6) +
  scale_color_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  scale_size_manual(values = c(2, 2, 0.1)) +
  guides(size = FALSE) +
  ylab("Casos acumulados") +
  xlab("Fecha") +
  scale_y_continuous(labels = scales::comma) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "sir_nacional.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "sir_nacional@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

summary(R_hat$R_hat)     
