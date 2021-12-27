# (C) Copyright 2020-2021 Sur Herrera Paredes

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
# library(deSolve)
library(rstan)
source("casos/sir_funciones.r")
source("util/leer_datos_abiertos.r")
args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             estados_lut = "../datos/util/estados_lut_datos_abiertos.csv",
             poblacion = "../datos/demograficos/pob_estado.tsv",
             fecha_inicio = "2020-03-01" %>% parse_date(format = "%Y-%m-%d"),
             # dias_suavizado = 7,
             dias_retraso = 15,
             dias_pronostico_max = 60,
             dir_estimados = "estimados/")
poblacion <- 127792286

# Leer poblaciones
pob <- read_tsv(args$poblacion,
                col_types = cols(estado = col_character(),
                                 .default = col_number()))
stop_for_problems(pob)
estados_lut <- read_csv(args$estados_lut,
                        col_names = FALSE,
                        col_types = cols(.default = col_character()))
stop_for_problems(estados_lut)
estados_lut <- set_names(estados_lut$X2, estados_lut$X1)

# Leer base de datos ssa
Dat <- leer_datos_abiertos(archivo = args$base_de_datos,
                           solo_confirmados = FALSE,
                           solo_fallecidos = FALSE,
                           solo_laboratorio = FALSE,
                           version = "adivinar")

# Seleccionar USMER
Dat <- Dat %>%
  filter(ORIGEN == "1") %>%
  select(ENTIDAD_UM, ENTIDAD_RES,
         MUNICIPIO_RES,
         TIPO_PACIENTE,
         FECHA_SINTOMAS,
         EDAD,
         CLASIFICACION_FINAL)

# Convertir CLASIFICACION_FINAL a resultado, con
# definición 1 = positivo por cualquier, 2 = negativo por
# laboratorio, 3 = sospechoso
# Solo necesario para versión octubre2020 de los datos
Dat <- Dat %>%
  mutate(RESULTADO = replace(CLASIFICACION_FINAL,
                             CLASIFICACION_FINAL %in% c("1", "2", "3", "8"),
                             "1")) %>%
  mutate(RESULTADO = replace(RESULTADO, RESULTADO %in% c("7"), "2")) %>%
  mutate(RESULTADO = replace(RESULTADO, RESULTADO %in% c("4", "5", "6", "9"), "3")) %>%
  # select(RESULTADO, CLASIFICACION_FINAL) %>% table
  select(-CLASIFICACION_FINAL)


# Estimación rápida por fecha síntomas, estado, (10%/100% de ambulatorio/hospitalizado )
# Calcular agregados para estratificación.
# Falta desagregado por edad
Dat <- Dat %>%
  split(.$ENTIDAD_UM) %>%
  map_dfr(function(d){
    d %>%
      split(.$FECHA_SINTOMAS)  %>%
      map_dfr(function(d){
        tibble(positivos_leves = sum(d$TIPO_PACIENTE == "1" & d$RESULTADO == "1"),
               positivos_graves = sum(d$TIPO_PACIENTE == "2" & d$RESULTADO == "1"),
               negativos_leves = sum(d$TIPO_PACIENTE == "1" & d$RESULTADO == "2"),
               negativos_graves = sum(d$TIPO_PACIENTE == "2" & d$RESULTADO == "2"),
               sospechosos_leves = sum(d$TIPO_PACIENTE == "1" & d$RESULTADO == "3"),
               sospechosos_graves = sum(d$TIPO_PACIENTE == "2" & d$RESULTADO == "3"),
               n_pacientes = nrow(d))
      }, .id = 'fecha')
  }, .id = "estado") %>%
  mutate(fecha = parse_date(fecha, format = "%Y-%m-%d")) %>%
  arrange(estado, fecha)
Dat

# Estimados por fecha
Cen <- Dat %>%
  mutate(positividad_leves = positivos_leves / (positivos_leves + negativos_leves + 1),
         positividad_graves = positivos_graves / (positivos_graves + negativos_graves + 1)) %>%
  mutate(graves_estimados = positivos_graves + (sospechosos_graves * positividad_graves),
         leves_estimados = (positivos_leves + (sospechosos_leves * positividad_leves))) %>%
  select(fecha, estado, positividad_leves, positividad_graves, graves_estimados, leves_estimados) %>%
  mutate(positivos_estimados = graves_estimados + leves_estimados) %>%
  filter(positivos_estimados > 0)

# Normalizar por población y muestreo
Cen <- Cen %>%
  mutate(estado = as.vector(estados_lut[estado])) %>%
  left_join(pob %>%
              transmute(estado, pob = conapo_2020 / sum(conapo_2020)),
            by = "estado") %>%
  split(.$fecha) %>%
  map_dfr(function(d){
    pos_usmer_nac <- sum(d$positivos_estimados * d$pob) * 32
    # Alrededor de 7.5% casos posibles van a usmer a nivel nacional de manera 
    # consistente. Bajó como a 7 en últimas 2 semanas. Uso 7.25%
    # Bajó siginificativamente a 6.25% en semana quince, o sea que estoy
    # subestimando el número de casos real. Necesito incorporar un factor variable por
    # semana. Tal vez puedo estimarlo del bietín de influenza que parece llegar a semana
    # 16.
    tibble(casos_estimados = floor(pos_usmer_nac / 0.0725))
  }, .id = "fecha") %>%
  mutate(fecha = parse_date(fecha, format = "%Y-%m-%d"))

# Añadir ceros en fechas sin casos
n_dias <- as.numeric(max(Cen$fecha) - min(Cen$fecha))
fechas <- tibble(dia = 0:(n_dias - 1)) %>%
  mutate(fecha = min(Cen$fecha) + dia) %>%
  select(fecha)
Cen <- fechas %>%
  full_join(Cen, by = "fecha") %>%
  arrange(fecha) %>%
  mutate(casos_estimados = replace_na(casos_estimados, 0)) %>%
  mutate(casos_acumulados_estimados = cumsum(casos_estimados)) 

########

# Preparar para bayes SEIR
Cen <- Cen %>%
  filter(fecha >= args$fecha_inicio) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))

# Determinando fechas
fecha_inicio <- min(Cen$fecha)
fecha_final <- max(Cen$fecha)
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso
fechas_dias <- seq(from=0, to = n_dias_ajuste, by = 15) %>% floor
# Prácticamente no hubo cambio estos díás, uniendo periodos
# fechas_dias <- fechas_dias[-c(8:9)]
# fechas_dias <- fechas_dias[-c(8:9, 11, 19)]
fechas_dias <- fechas_dias[-c(8:9, 11, 19, 24:25)]

# fechas_dias <- fechas_dias[1:(length(fechas_dias) - 1)]

fechas_dias
c(fechas_dias, n_dias_ajuste) %>% diff

dat_train <- Cen %>%
  filter(dia < n_dias_ajuste)

t_0 <- c(poblacion - 2 * Cen$casos_acumulados_estimados[1],
         Cen$casos_acumulados_estimados[1],
         Cen$casos_acumulados_estimados[1],
         0) / poblacion

dat_train <- dat_train %>%
  filter(dia > 0)

m1.model <- stan_model("casos/sir.stan", model_name = "seir")

stan_datos <- list(n_obs = nrow(dat_train),
                   n_difeq = 4,
                   pob = poblacion,
                   y = dat_train$casos_estimados,
                   t0 = 0,
                   ts = dat_train$dia,
                   y0 = t_0,
                   n_int = length(fechas_dias),
                   fechas_dias = fechas_dias,
                   T_inc = 5.1562,
                   # T_inf = 11.01072,
                   T_inf = 5,
                   likelihood = 1,
                   f_red = log(1.22))

# init <- list(logphi = 3.6,
#              r_betas = c(0.47, 0.35,
#                          0.34, 0.28,
#                          0.25, 0.24,
#                          0.22, 0.18,
#                          0.20, 0.19,
#                          0.21, 0.23,
#                          0.22, 0.24,
#                          0.25, 0.23,
#                          0.26, 0.18,
#                          0.15, 0.19,
#                          0.19, 0.19,
#                          0.20, 0.18,
#                          0.20, 0.22,
#                          0.26, 0.31,
#                          0.39, 0.34,
#                          0.24, 0.20,
#                          0.19, 0.18,
#                          0.18))

init <- list(logphi = 3.6,
             r_betas = c(0.50, 0.35,
                         0.34, 0.28,
                         0.25, 0.24,
                         0.22, 0.18,
                         0.20, 0.19,
                         0.21, 0.23,
                         0.22, 0.24,
                         0.25, 0.23,
                         0.26, 0.17,
                         0.18, 0.20,
                         0.20, 0.18,
                         0.20, 0.23,
                         0.26, 0.31,
                         0.39, 0.34,
                         0.24, 0.20,
                         0.19, 0.19,
                         0.17, 0.18,
                         0.21, 0.24,
                         0.22))

adapt_delta <- 0.5
max_treedepth <- 10
init <- list(chain_1 = init,
             chain_2 = init,
             chain_3 = init,
             chain_4 = init)
m1.stan <- sampling(m1.model,
                    data = stan_datos,
                    pars = c("r_betas",
                             "phi",
                             "I_hoy"),
                    init = init,
                    chains = 4,
                    iter = 4000,
                    warmup = 3000,
                    thin = 1,
                    cores = 4,
                    control = list(max_treedepth = max_treedepth,
                                   adapt_delta = adapt_delta))
# save(m1.stan, file = "m1.cen_coronamex.stan.rdat")
# load("m1.cen_coronamex.stan.rdat")

m1.stan
print(m1.stan, pars = c("r_betas", "phi"))
post <- rstan::extract(m1.stan)

# tibble(chain1 = (as.array(m1.stan)[,1,] %>% colMeans())[1:length(fechas_dias)],
#        chain2 = (as.array(m1.stan)[,2,] %>% colMeans())[1:length(fechas_dias)],
#        chain3 = (as.array(m1.stan)[,3,] %>% colMeans())[1:length(fechas_dias)],
#        chain4 = (as.array(m1.stan)[,4,] %>% colMeans())[1:length(fechas_dias)]) %>%
#   print(n = length(fechas_dias))

# (as.array(m1.stan)[,1,] %>% colMeans())[1:length(fechas_dias)]
# (as.array(m1.stan)[,2,] %>% colMeans())[1:length(fechas_dias)]
# (as.array(m1.stan)[,3,] %>% colMeans())[1:length(fechas_dias)]
# (as.array(m1.stan)[,4,] %>% colMeans())[1:length(fechas_dias)]

p1 <- apply(post$I_hoy, 2, quantile, prob = c(0.1, 0.5, 0.9), na.rm = TRUE) %>%
  t %>%
  as_tibble() %>%
  rename(stan_lower = "10%",
         stan_median = "50%",
         stan_upper = "90%") %>%
  mutate(dia = 1:(n_dias_ajuste - 1 )) %>%
  left_join(Cen, by = "dia") %>%
  ggplot(aes(x = fecha)) +
  geom_bar(aes(y = casos_estimados), stat = "identity") +
  geom_line(aes(y = stan_median)) +
  geom_ribbon(aes(ymin = stan_lower, ymax = stan_upper), alpha = 0.2) +
  theme_classic()
p1

### Diagnostics
par.names <- summary(m1.stan, pars = c("r_betas", "phi"))$summary %>% 
  row.names()
lp <- bayesplot::log_posterior(m1.stan)
np <- bayesplot::nuts_params(m1.stan)
# bayesplot::mcmc_parcoord(as.matrix(m1.stan),
#                          np = np,
#                          pars = par.names,
#                          transformations = list(phi = "log"))
# bayesplot::mcmc_trace(as.array(m1.stan), pars = par.names, np = np)
# 
# bayesplot::mcmc_pairs(as.array(m1.stan),
#                       pars = par.names,
#                       np = np,
#                       off_diag_args = list(size = 0.75))
# 
# bayesplot::mcmc_nuts_divergence(np, lp)
# bayesplot::mcmc_nuts_energy(np)
# bayesplot::mcmc_acf(as.array(m1.stan),
#                     pars = par.names,
#                     lags = 10)
# 
# stan_diag(m1.stan)
##

bayesplot::mcmc_areas(as.array(m1.stan), pars = par.names[1:length(fechas_dias)], prob = 0.8)

# R0
apply(post$r_betas * stan_datos$T_inf, 2, quantile, prob = c(0.1, 0.5, 0.9), na.rm = TRUE) %>%
  t %>%
  as_tibble() %>%
  rename(stan_lower = "10%",
         stan_median = "50%",
         stan_upper = "90%") %>%
  mutate(dia = stan_datos$fechas_dias,
         param = "r_beta") %>%
  ggplot(aes(x = dia)) +
  geom_line(aes(y = stan_median), size = 2) +
  geom_hline(yintercept = 1) +
  geom_ribbon(aes(ymin = stan_lower, ymax = stan_upper), alpha = 0.2) +
  ylab("R0") +
  theme_classic()

t_0 <- as.numeric(t_0)
t_0 <- c(S = t_0[1],
         E = t_0[2],
         I = t_0[3],
         R = t_0[4],
         t = 0)
t_0
modelos <- list()
for(i in 1:length(post$phi)){
  modelos[[i]] <- list(modelo = i,
                       T_inc = stan_datos$T_inc,
                       T_inf = stan_datos$T_inf,
                       tiempos_betas = stan_datos$fechas_dias,
                       r_betas = post$r_betas[i,],
                       phi = post$phi[i],
                       t_0 = t_0,
                       metodo = "stan")
}


# Integrar modelos posterior
sims <- simular_ode(modelos = modelos,
                    n_dias = stan_datos$n_obs + args$dias_pronostico_max,
                    odefun = seir2,
                    otros_par = "phi")

# Encontrar mediana posterior casos nuevos (1-4)
dat <- seir_ci(sims = sims, pob = stan_datos$pob, fecha_inicio = fecha_inicio)
# dat$fecha_estimacion <- Sys.Date() - 1
dat$fecha_estimacion <- Sys.Date()
dat
write_csv(dat, "estimados/bayes_seir_centinela_coronamex.csv")

dat <- read_csv("estimados/bayes_seir_centinela_coronamex.csv")

p1 <- Cen %>%
  full_join(dat %>%
              mutate(dia = as.numeric(fecha - min(fecha))) %>%
              select(dia, nuevos_mu_10, nuevos_mu_50, nuevos_mu_90) %>%
              filter(dia <= stan_datos$n_obs), by = "dia") %>%
  full_join(dat %>%
              mutate(dia = as.numeric(fecha - min(fecha))) %>%
              select(dia, nuevos_obs_10, nuevos_obs_50, nuevos_obs_90) %>%
              filter(dia > stan_datos$n_obs), by = "dia") %>%
  # print(n = 1000)
  mutate(fecha = min(fecha, na.rm = TRUE) + dia) %>%
  filter(fecha <= Sys.Date()) %>%
  # print(n = 500)
  ggplot(aes(x = fecha, y = casos_estimados)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  
  geom_ribbon(aes(ymin = nuevos_mu_10, ymax = nuevos_mu_90), color = "blue", alpha = 0.2) +
  geom_line(aes(y = nuevos_mu_50), size = 2, col = "blue") +
  
  geom_ribbon(aes(ymin = nuevos_obs_10, ymax = nuevos_obs_90), color = "red", alpha = 0.2) +
  geom_line(aes(y = nuevos_obs_50), size = 2, col = "red") +
  
  # geom_vline(xintercept = fecha_inicio + fechas_dias) +
  # ylim(c(0, 1e4)) +
  # scale_y_log10() +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top",
        axis.title = element_text(face = "bold", size = 12),
        plot.margin = margin(l = 20, r = 20, b = 20),
        strip.text = element_text(face = "bold"))
p1

