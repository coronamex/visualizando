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
library(rstan)
source("casos/sir_funciones.r")

args <- list(serie_real = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv.gz",
             modelo_stan = "casos/sir.stan",
             dias_retraso = 15,
             dias_extra_sim = 60,
             dias_ajuste = 180,
             modelo_previo = "m1.stan.rdat",
             dir_esimados = "estimados/",
             dir_salida = "../sitio_hugo/static/imagenes/")
fecha_inicio <- parse_date("2020-03-01", format = "%Y-%m-%d")
pob <- 127792286

Dat <- read_csv(args$serie_real,
                col_types = cols(fecha = col_date("%Y-%m-%d"),
                                 .default = col_number()))
Dat <- Dat %>% select(fecha, sintomas_acumulados, sintomas_nuevos)

# Ajustando sólo a fechas recientes
fecha_I <- max(Dat$fecha) - args$dias_retraso - args$dias_ajuste - 1 - 4
fecha_E <- max(Dat$fecha) - args$dias_retraso - args$dias_ajuste - 1
Dat <- Dat %>%
  filter(fecha > fecha_I)
approx_E <- Dat %>%
  filter(fecha > fecha_I & fecha <= fecha_E) %>%
  select(sintomas_nuevos) %>%
  unlist %>% sum
Dat <- Dat %>%
  filter(fecha > fecha_E)

Dat <- Dat %>%
  filter(fecha >= fecha_inicio) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))


# Determinando fechas
fecha_inicio <- min(Dat$fecha)
fecha_final <- max(Dat$fecha)
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso
fechas_dias <- seq(from=0, to = n_dias_ajuste, by = 15) %>% floor
# fechas_dias <- fechas_dias[-(11)]
# fechas_dias <- fechas_dias[-c(8,11,17)]
fechas_dias <- fechas_dias[1:(length(fechas_dias) - 1)]

fechas_dias
c(fechas_dias, n_dias_ajuste) %>% diff

dat_train <- Dat %>%
  filter(dia < n_dias_ajuste)

t_0 <- c(pob - Dat$sintomas_acumulados[1],
         approx_E,
         approx_E,
         Dat$sintomas_acumulados[1] - 2 * approx_E) / pob

dat_train <- dat_train %>%
  filter(dia > 0)

m1.model <- stan_model("casos/sir.stan", model_name = "seir")

stan_datos <- list(n_obs = nrow(dat_train),
                   n_difeq = 4,
                   pob = pob,
                   y = dat_train$sintomas_nuevos,
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

# init <- list(logphi = 3.8,
#              r_betas = c(0.746, 0.381,
#                          0.366, 0.296,
#                          0.276, 0.243,
#                          0.228, 0.217,
#                          0.183, 0.200,
#                          0.177, 0.201,
#                          0.231, 0.211,
#                          0.236, 0.236,
#                          0.200, 0.253))

init <- list(logphi = 3.8,
             r_betas = c(0.27, 0.23,
                         0.21, 0.26,
                         0.16, 0.14,
                         0.18, 0.17,
                         0.17, 0.19,
                         0.17, 0.16))

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
                    iter = 2000,
                    warmup = 1000,
                    thin = 1,
                    cores = 4,
                    control = list(max_treedepth = 10,
                                   adapt_delta = 0.5))
# save(m1.stan, file = "m1.stan.rdat")
# load("m1.stan.rdat")
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
  left_join(Dat, by = "dia") %>%
  ggplot(aes(x = fecha)) +
  geom_bar(aes(y = sintomas_nuevos), stat = "identity") +
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

### Necesito calcular desde día 0 hasta n_dias ajuste + dias extra
# 1. Intervalo creible de casos nuevos
# 2. Intervalo creible de casos acumulados
# 3. Intervalo creible de casos nuevos detectados,
# 4. Intervalo creible de casos acumulados detectados
# 5. Intervalo creible de casos nuevos desde antes de sana distancia
# 6. Intervalo creible de casos acumulados desde antes de sana distancia
# fecha_estimacion <- Sys.Date() - 1
fecha_estimacion <- Sys.Date()

# Integrar modelos posterior
sims <- simular_ode(modelos = modelos,
                    n_dias = stan_datos$n_obs + args$dias_extra_sim,
                    odefun = seir2,
                    otros_par = "phi")

# Encontrar mediana posterior casos nuevos (1-4)
dat <- seir_ci(sims = sims, pob = stan_datos$pob, fecha_inicio = fecha_inicio) %>%
  filter(fecha > fecha_inicio)
dat$fecha_estimacion <- fecha_estimacion
dat
write_csv(dat, "estimados/bayes_seir_nacional.csv")


# # Modelo simulando comportamiento antes de sana distancia
# dat <- modelos %>%
#   map(function(l){
#     l$tiempos_betas <- l$tiempos_betas[1]
#     l$r_betas <- l$r_betas[1]
#     l
#   }) 
# dat <- simular_ode(modelos = dat, n_dias = 200,
#               odefun = seir2,
#               otros_par = "phi")
# dat <- seir_ci(sims = dat, pob = stan_datos$pob, fecha_inicio = fecha_inicio)
# dat$fecha_estimacion <- fecha_estimacion
# dat
# write_csv(dat, "estimados/bayes_seir_nacional_pre_2020-03-16.csv")
# 
# # dat <- read_csv("estimados/bayes_seir_nacional_pre_2020-03-16.csv")
# # dat %>% print(n = 100)
# 
# # dat <- (modelos %>%
# #   map(function(l){
# #     l$tiempos_betas <- l$tiempos_betas[1]
# #     l$r_betas <- l$r_betas[1]
# #     l
# #   }))[1:10] %>%
# #   simular_ode(n_dias = stan_datos$n_obs,odefun = seir2, otros_par = "phi") %>%
# #   seir_ci(pob = stan_datos$pob, fecha_inicio = fecha_inicio) %>% print(n = 100)
# # dat %>% print(n = 100)
# # 
# # simular_ode(n_dias = stan_datos$n_obs + args$dias_extra_sim,
# #               odefun = seir2,
# #               otros_par = "phi") %>%
# #   seir_ci(pob = stan_datos$pob, fecha_inicio = fecha_inicio)
# # dat$fecha_estimacion <- fecha_estimacion
# # dat
# # 
# # cat("======================\n")
# # class(modelos[1])
# # 
# # cat("======================\n")
# # class(modelos[[1]])
# # modelos
# # str(modelos[1:2])
# 
# # Modelo simulando comportamiento antes de 2020-04-15
# dat <- (modelos %>%
#   map(function(l){
#     l$tiempos_betas <- l$tiempos_betas[1:3]
#     l$r_betas <- l$r_betas[1:3]
#     l
#   })) %>%
#   simular_ode(n_dias = 200,
#               odefun = seir2,
#               otros_par = "phi") %>%
#   seir_ci(pob = stan_datos$pob, fecha_inicio = fecha_inicio)
# dat$fecha_estimacion <- fecha_estimacion
# dat
# write_csv(dat, "estimados/bayes_seir_nacional_pre_2020-04-15.csv")

dat <- read_csv("estimados/bayes_seir_nacional.csv")
p1 <- Dat %>%
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
  ggplot(aes(x = fecha, y = sintomas_nuevos)) +
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

