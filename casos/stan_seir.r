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
Dat <- Dat %>% select(fecha, sintomas_nuevos)
Dat <- Dat %>%
  filter(fecha >= "2020-03-01")

Dat <- Dat %>% mutate(dia = as.numeric(fecha - min(fecha)) + 1)
Dat <- Dat %>%
  filter(dia < max(dia) - 15)
# Dat <- Dat[1:20,]
Dat

pob <- 127000000

stan_datos <- list(n_obs = nrow(Dat),
                   n_params = 3,
                   n_difeq = 4,
                   pob = pob,
                   y = Dat$sintomas_nuevos,
                   t0 = 0,
                   ts = Dat$dia,
                   y0 = c(pob - 2 * Dat$sintomas_nuevos[1],
                          Dat$sintomas_nuevos[1],
                          Dat$sintomas_nuevos[1], 0) / pob)
stan_datos
  
m1.stan <- stan("casos/sir.stan", data = stan_datos,
                # pars = c("T_inc", "T_inf",
                #          "params", "R_0", "E_hoy"),
                chains = 8, iter = 1e4, cores = 8,
                control = list(max_treedepth = 15))
# Warning messages:
#   1: There were 4479 divergent transitions after warmup. Increasing adapt_delta above 0.8 may help. See
# http://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup 
# 2: There were 34159 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 3: There were 6 chains where the estimated Bayesian Fraction of Missing Information was low. See
# http://mc-stan.org/misc/warnings.html#bfmi-low 
# 4: Examine the pairs() plot to diagnose sampling problems
# 
# 5: The largest R-hat is 6.8, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#r-hat 
# 6: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#bulk-ess 
# 7: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# http://mc-stan.org/misc/warnings.html#tail-ess 
m1.stan
save(m1.stan, file = "m1.stan.rdat")

pairs(m1.stan, pars = "params")
extract(m1.stan, pars = "params")

traceplot(m1.stan, pars = c("params", "R_0"))
# summary(rgamma(1000, shape = 2.03, rate = 1/2.54))

