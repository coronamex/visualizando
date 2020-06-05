library(tidyverse)
library(rstan)
# source("util/leer_datos_abiertos.r")
# Tab <- leer_datos_abiertos("../datos/datos_abiertos/base_de_datos.csv", solo_confirmados = TRUE, solo_fallecidos = TRUE)
# Tab <- as.numeric(Tab$FECHA_DEF - Tab$FECHA_SINTOMAS)
# Tab <- Tab[Tab > 0]
# hist(Tab)
# summary(Tab)
# length(Tab)
# fitdistrplus::fitdist(Tab, "gamma", start = list(shape = 4, rate = 1/2))
# hist(rgamma(1000, shape = 2.71, rate = 0.246))


args <- list(serie_real = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv",
             modelo_stan = "casos/sir.stan")
Dat <- read_csv(args$serie_real)
Dat <- Dat %>% select(fecha, sintomas_acumulados)
Dat <- Dat[50: 70, ]
Dat <- Dat %>% mutate(dia = as.numeric(fecha - min(fecha)) + 1)
Dat

stan_datos <- list(n_obs = nrow(Dat),
                   n_params = 3,
                   n_difeq = 4,
                   pob = 1e6,
                   y = Dat$sintomas_acumulados,
                   t0 = 0,
                   ts = Dat$dia,
                   y0 = c(1e6 - Dat$sintomas_acumulados[1],
                          0,
                          Dat$sintomas_acumulados[1], 0) / 1e6)


m1.stan <- stan("casos/sir.stan", data = stan_datos,
                chains = 1, iter = 100)
m1.stan
# 