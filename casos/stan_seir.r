library(tidyverse)
library(rstan)

args <- list(serie_real = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv",
             modelo_stan = "casos/sir.stan",
             dias_retraso = 15,
             dir_esimados = "estimados/",
             dir_salida = "../sitio_hugo/static/imagenes/")
fecha_inicio <- parse_date("2020-03-01", format = "%Y-%m-%d")

Dat <- read_csv(args$serie_real)
Dat <- Dat %>% select(fecha, sintomas_acumulados, sintomas_nuevos)
Dat <- Dat[48: 80, ]
Dat <- Dat %>% mutate(dia = as.numeric(fecha - min(fecha)) + 1)
Dat

stan_datos <- list(n_obs = nrow(Dat),
                   n_params = 3,
                   n_difeq = 4,
                   pob = 1e6,
                   y = Dat$sintomas_nuevos,
                   t0 = 0,
                   ts = Dat$dia,
                   y0 = c(1e6 - 2 * Dat$sintomas_acumulados[1],
                          Dat$sintomas_acumulados[1],
                          Dat$sintomas_acumulados[1], 0) / 1e6,
                   n_int = 3,
                   fechas_dias = c(10, 20, 30))
  
m1.stan <- stan("casos/sir.stan", data = stan_datos,
                pars = c("T_inc", "T_inf", "r_beta",
                         "f_int",
                         "params", "R_0", "E_hoy"),
                chains = 1, iter = 500)
m1.stan
# 