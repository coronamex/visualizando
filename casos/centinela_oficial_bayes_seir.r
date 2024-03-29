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
# library(deSolve)
source("casos/sir_funciones.r")
source("util/leer_datos_abiertos.r")
args <- list(centinela_official = "../datos/centinela/semana_15/estimados_nacionales.csv",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             semana1_fecha = "2019-12-29" %>% parse_date(format = "%Y-%m-%d"),
             fecha_inicio = "2020-03-01" %>% parse_date(format = "%Y-%m-%d"),
             dias_suavizado = 7,
             dias_retraso = 0,
             dias_pronostico_max = 20,
             dir_estimados = "estimados/")
pob <- 127792286

# Leer Centinela oficial
Cen_oficial <- read_csv(args$centinela_official,
                        col_types = cols(.default = col_number()))
stop_for_problems(Cen_oficial)
Cen_oficial <- Cen_oficial %>%
  mutate(fecha = args$semana1_fecha + 7*(semana),
         estimados_acumulados_nacional = cumsum(estimados_positivos_nacional)) %>%
  select(-semana)

# Leer base de datos ssa
Dat <- leer_datos_abiertos(archivo = args$base_de_datos,
                           solo_confirmados = FALSE, solo_fallecidos = FALSE)


# Seleccionar USMER
Dat <- Dat %>%
  filter(ORIGEN == "1") %>%
  select(ENTIDAD_UM, ENTIDAD_RES,
         MUNICIPIO_RES,
         TIPO_PACIENTE,
         FECHA_SINTOMAS,
         EDAD,
         RESULTADO)
Dat <- Dat %>%
  mutate(fecha = FECHA_SINTOMAS) %>%
  group_by(fecha) %>%
  summarise(casos_usmer = sum(RESULTADO == "1")) %>%
  ungroup() %>%
  arrange(fecha)

# Rellenar fechas
Dat <- Dat %>%
  full_join(tibble(fecha = min(Dat$fecha) + 0:as.numeric(max(Dat$fecha) - min(Dat$fecha))),
            by = "fecha") %>%
  arrange(fecha) %>%
  mutate(casos_usmer = replace_na(casos_usmer, 0)) %>%
  mutate(acumulados_usmer = cumsum(casos_usmer))
Dat
  
Cen_oficial <- Cen_oficial %>%
  full_join(tibble(fecha = min(Cen_oficial$fecha) + 0:as.numeric(max(Cen_oficial$fecha) - min(Cen_oficial$fecha))),
            by = "fecha", ) %>%
  select(-total_usmer, -posibles_usmer, -estimados_positivos_nacional,
         -totales_nacional, -estimados_posibles_nacional, -pruebas_usmer,
         -positivos_usmer) %>%
  arrange(fecha) 
Cen_oficial

########### Rellenar centinela oficial con "factor de corrección" de la SSA
Cen_oficial <- full_join(Dat, Cen_oficial, by = "fecha") %>%
  arrange(fecha) %>%
  mutate(factor_ssa = estimados_acumulados_nacional / acumulados_usmer) %>%
  mutate(factor_ssa = replace(factor_ssa, is.nan(factor_ssa), 0))
Cen_oficial

n_dias <- as.numeric(max(Cen_oficial$fecha) - args$semana1_fecha)
n_semanas <- floor(n_dias/7)
semanas <- tibble(semana = rep(1:n_semanas, each = 7)) %>%
  mutate(dia = 0:(length(semana) - 1)) %>%
  mutate(fecha = args$semana1_fecha + dia) %>%
  select(-dia)

Cen_oficial <- Cen_oficial %>%
  left_join(semanas, by = "fecha") %>%
  split(.$semana) %>%
  map_dfr(function(d){
    fc <- d$factor_ssa
    fc <- fc[!is.na(fc)]
    
    if(length(fc) == 0){
      fc <- NA
    }else if(length(fc) > 1){
      stop("ERROR")
    }
    
    d$acumulados_estimados <- fc * d$acumulados_usmer
    d
  }) %>%
  select(fecha, acumulados_estimados) %>%
  filter(!is.na(acumulados_estimados)) %>%
  mutate(nuevos_estimados = acumulados_estimados - lag(acumulados_estimados, 1, 0)) %>%
  filter(fecha >= args$fecha_inicio) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))
Cen_oficial


### Preparar para ajustar bayes seir
fecha_inicio <- min(Cen_oficial$fecha)
fecha_final <- max(Cen_oficial$fecha)
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso
fechas_dias <- seq(from=0, to = n_dias_ajuste, by = 15) %>% floor
fechas_dias <- fechas_dias[1:3] # De otra manera último período sólo tiene 3 días
fechas_dias
c(fechas_dias, n_dias_ajuste) %>% diff

dat_train <- Cen_oficial %>%
  filter(dia < n_dias_ajuste)

t_0 <- c(pob - 2 * dat_train$acumulados_estimados[1],
         dat_train$acumulados_estimados[1],
         dat_train$acumulados_estimados[1],
         0) / pob
t_0
dat_train <- dat_train %>%
  filter(dia > 0)
dat_train

m1.model <- stan_model("casos/sir.stan", model_name = "seir")

stan_datos <- list(n_obs = nrow(dat_train),
                   n_difeq = 4,
                   pob = pob,
                   y = floor(dat_train$nuevos_estimados),
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
# init <- list(logphi = log(30),
#              r_betas = c(0.59, 0.27,
#                          0.22, 0.15,
#                          0.15, 0.13, 0.11))
# init
m1.stan <- sampling(m1.model,
                    data = stan_datos,
                    pars = c("r_betas",
                             "phi",
                             "I_hoy"),
                    # init = list(chain_1 = init,
                    #             chain_2 = init,
                    #             chain_3 = init,
                    #             chain_4 = init),
                    init = function(){
                      list(logphi = rnorm(n=1, mean = 3, sd = 0.5),
                           r_betas = runif(length(stan_datos$fechas_dias),
                                           min = 0,
                                           max = 1) %>%
                             sort(decreasing = TRUE))
                    },
                    chains = 4,
                    iter = 7000,
                    warmup = 5000,
                    thin = 1,
                    cores = 4,
                    control = list(max_treedepth = 10,
                                   adapt_delta = 0.5))


save(m1.stan, file = "m1.cen_oficial.stan.rdat")
# load("m1.stan.rdat")
m1.stan
print(m1.stan, pars = c("r_betas", "phi"))
post <- rstan::extract(m1.stan)

p1 <- apply(post$I_hoy, 2, quantile, prob = c(0.1, 0.5, 0.9), na.rm = TRUE) %>%
  t %>%
  as_tibble() %>%
  rename(stan_lower = "10%",
         stan_median = "50%",
         stan_upper = "90%") %>%
  mutate(dia = 1:(n_dias_ajuste - 1 )) %>%
  left_join(Cen_oficial, by = "dia") %>%
  ggplot(aes(x = fecha)) +
  geom_bar(aes(y = nuevos_estimados), stat = "identity") +
  geom_line(aes(y = stan_median)) +
  geom_ribbon(aes(ymin = stan_lower, ymax = stan_upper), alpha = 0.2) +
  theme_classic()
p1


### Diagnostics
par.names <- summary(m1.stan, pars = c("r_betas", "phi"))$summary %>% 
  row.names()
lp <- bayesplot::log_posterior(m1.stan)
np <- bayesplot::nuts_params(m1.stan)
bayesplot::mcmc_parcoord(as.matrix(m1.stan),
                         np = np,
                         pars = par.names,
                         transformations = list(phi = "log"))
bayesplot::mcmc_trace(as.array(m1.stan), pars = par.names, np = np)

bayesplot::mcmc_pairs(as.array(m1.stan),
                      pars = par.names,
                      np = np,
                      off_diag_args = list(size = 0.75))

bayesplot::mcmc_nuts_divergence(np, lp)
bayesplot::mcmc_nuts_energy(np)
bayesplot::mcmc_acf(as.array(m1.stan),
                    pars = par.names,
                    lags = 10)

stan_diag(m1.stan)
##

bayesplot::mcmc_areas(as.array(m1.stan), pars = par.names[1:3], prob = 0.8)

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



# Integrar modelos posterior
sims <- simular_ode(modelos = modelos,
                    n_dias = stan_datos$n_obs + args$dias_pronostico_max,
                    odefun = seir2,
                    otros_par = "phi")

# Encontrar mediana posterior casos nuevos (1-4)
dat <- seir_ci(sims = sims, pob = stan_datos$pob, fecha_inicio = fecha_inicio)
dat$fecha_estimacion <- Sys.Date() - 1
dat
write_csv(dat, "estimados/bayes_seir_centinela_oficial.csv")

p1 <- Cen_oficial %>%
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
  ggplot(aes(x = fecha, y = nuevos_estimados)) +
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













