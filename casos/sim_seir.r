library(tidyverse)
library(deSolve)
library(rstan)
source("casos/sir_funciones.r")


pob <- 127e6
casos_0 <- 10

sim <- ode(y = c(S = (pob - 2 * casos_0) / pob,
          E = casos_0 / pob,
          I = casos_0 / pob,
          R = 0,
          t = 0),
    times = 0:500,
    func = seir,
    parms = list(r_beta = 0.2,
                 alpha = 1 / 5,
                 gamma = 1 / 10)) %>%
  as_tibble()  %>%
  mutate_all(as.numeric) %>%
  mutate(I_nuevos = I + R - lag(I + R, 1, default = 0),
         I_acum = I + R)
sim

sim %>%
  ggplot(aes(x = t, y = I_acum)) +
  geom_line() +
  geom_line(aes(y = I), col = "red") +
  geom_line(aes(y = I_nuevos), col = "blue")

sim <- sim %>%
  mutate(sintomas_nuevos = rpois(n = 501, lambda = pob * I_nuevos))

sim %>%
  filter(t < 130) %>%
  ggplot(aes(x = t, y = sintomas_nuevos)) +
  geom_col() +
  geom_line(aes(y = I_nuevos)) 

Dat <- sim %>% filter(t < 130) %>%
  rename(dia = t)
Dat

stan_datos <- list(n_obs = nrow(Dat) - 1,
                   n_params = 3,
                   n_difeq = 4,
                   pob = pob,
                   y = Dat$sintomas_nuevos[-1],
                   t0 = 0,
                   ts = Dat$dia[-1],
                   y0 = c(pob - 2 * Dat$sintomas_nuevos[1],
                          Dat$sintomas_nuevos[1],
                          Dat$sintomas_nuevos[1], 0) / pob)
stan_datos

m1.stan <- stan("casos/sir.stan", data = stan_datos,
                chains = 3, iter = 20000, warmup = 10000 , cores = 4,
                control = list(adapt_delta = 0.9))
m1.stan
traceplot(m1.stan)
pairs(m1.stan)

#########
args <- list(serie_real = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv",
             modelo_stan = "casos/sir.stan")
Dat <- read_csv(args$serie_real)
Dat <- Dat %>% select(fecha, sintomas_nuevos)
Dat <- Dat %>%
  filter(fecha >= "2020-03-01")
Dat <- Dat %>% mutate(dia = as.numeric(fecha - min(fecha)) )
Dat <- Dat %>%
  filter(dia < max(dia) - 15)
# Dat <- Dat[1:20,]
Dat

stan_datos <- list(n_obs = nrow(Dat) - 1,
                   n_params = 3,
                   n_difeq = 4,
                   pob = pob,
                   y = Dat$sintomas_nuevos[-1],
                   t0 = 0,
                   ts = Dat$dia[-1],
                   y0 = c(pob - 2 * Dat$sintomas_nuevos[1],
                          Dat$sintomas_nuevos[1],
                          Dat$sintomas_nuevos[1], 0) / pob)
stan_datos

m1.stan <- stan("casos/sir.stan", data = stan_datos,
                chains = 1, iter = 1e4, cores = 4,
                control = list(adapt_delta = 0.8))
m1.stan
traceplot(m1.stan)
pairs(m1.stan)

casos_0 <- Dat$sintomas_nuevos[1]
pred <- ode(y = c(S = (pob - 2 * casos_0) / pob,
          E = casos_0 / pob,
          I = casos_0 / pob,
          R = 0,
          t = 0),
    times = Dat$dia,
    func = seir,
    parms = list(r_beta = median(extract(m1.stan)$r_beta),
                 alpha = 1 / 5,
                 gamma = 1 / 10)) %>%
  as_tibble() %>%
  mutate_all(as.numeric) %>%
  mutate(I_nuevos = I + R - lag(I + R, 1, default = 0),
         I_acum = I + R)
pred

ggplot(Dat, aes(x = dia, y = sintomas_nuevos)) +
  geom_col() +
  geom_line(data = pred, aes(x = t, y = I_nuevos * pob), col = "grey", size = 3) +
  AMOR::theme_blackbox()
s  
