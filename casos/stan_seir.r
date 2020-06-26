library(tidyverse)
library(rstan)
source("casos/sir_funciones.r")

# https://rpubs.com/tjmahr/hpdi
compute_hpdi <- function(xs, prob = .9) {
  x_sorted <- sort(xs)
  n <- length(xs)
  
  num_to_keep <- ceiling(prob * n)
  num_to_drop <- n - num_to_keep
  
  possible_starts <- seq(1, num_to_drop + 1, by = 1)
  # Just count down from the other end
  possible_ends <- rev(seq(from = n, length = num_to_drop + 1, by = -1))
  
  # Find smallest interval
  span <- x_sorted[possible_ends] - x_sorted[possible_starts]
  edge <- which.min(span)
  edges <- c(possible_starts[edge], possible_ends[edge])
  
  # My requirement: length of span interval must be same as number to keep.
  # Other methods produce intervals that are 1 longer.
  stopifnot(length(edges[1]:edges[2]) == num_to_keep)
  
  x_sorted[edges]
}

args <- list(serie_real = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv",
             modelo_stan = "casos/sir.stan",
             dias_retraso = 15,
             dias_extra_sim = 15,
             dir_esimados = "estimados/",
             dir_salida = "../sitio_hugo/static/imagenes/")
fecha_inicio <- parse_date("2020-03-01", format = "%Y-%m-%d")
pob <- 127792286

Dat <- read_csv(args$serie_real,
                col_types = cols(fecha = col_date("%Y-%m-%d"),
                                 .default = col_number()))
Dat <- Dat %>% select(fecha, sintomas_acumulados, sintomas_nuevos)
Dat <- Dat %>%
  filter(fecha >= fecha_inicio) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))
# Dat

# 2.03 * 2.54
# 2.712 * 4.06
# rgamma(n = 10000, shape = 2.03, rate = 1/2.54) %>% summary
# qgamma(p = 0.5, shape = 2.03, rate = 1/2.54)
# Dat %>%
#   pmap_dfr(function(fecha, sintomas_acumulados, sintomas_nuevos, dia){
#     rgamma(n = sintomas_nuevos, shape = )
#   })

# Determinando fechas
fecha_inicio <- min(Dat$fecha)
fecha_final <- max(Dat$fecha)
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso

# fechas_dias <- as.numeric((c("2020-03-15") %>% parse_date(format = "%Y-%m-%d")) - fecha_inicio)
# fechas_dias <- c(fechas_dias, seq(from = fechas_dias[length(fechas_dias)] + 15,
#                                   to = n_dias_ajuste,
#                                   by = 15))
# fechas_dias
# fecha_inicio + fechas_dias
# fechas_dias <- as.numeric((c("2020-03-15", "2020-03-30", "2020-04-29")
#                            %>% parse_date(format = "%Y-%m-%d")) - fecha_inicio)
# fechas_dias
fechas_dias <- seq(from=14, to = n_dias_ajuste, by = 15) %>% floor
fechas_dias <- seq(from=0, to = n_dias_ajuste, by = 15) %>% floor
# fechas_dias <- fechas_dias[1:5]
fechas_dias
c(fechas_dias, n_dias_ajuste) %>% diff


dat_train <- Dat %>%
  filter(dia < n_dias_ajuste)

t_0 <- c(pob - 2 * Dat$sintomas_acumulados[1],
         Dat$sintomas_acumulados[1],
         Dat$sintomas_acumulados[1],
         0) / pob

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
                   T_inf = 11.01072,
                   likelihood = 1,
                   f_red = log(1.22))
# m1.opt <- optimizing(m1.model,
#                      data = stan_datos,
#                      verbose = TRUE,
#                      init = list(r_beta = 0.61,
#                                  f_int = c(0.5,0.4,0.3,0.3,0.2,0.2),
#                                  phi = 3),
#                      hessian = TRUE,
#                      iter = 2000,
#                      algorithm = "Newton")
# init <- list(r_beta = m1.opt$par["r_beta"],
#              # phi = m1.opt$par["phi"],
#              logphi = log(m1.opt$par["phi"]),
#              f_int = m1.opt$par[3:(2+stan_datos$n_int)])
# init <- list(r_beta = 0.6226926,
#              logphi = log(9),
#              f_int = c(0.6226926, 0.4447154,
#                        0.3560217, 0.2849129,
#                        0.2384390, 0.2038462, 0.1873019))
init <- list(logphi = log(9),
             r_betas = c(0.6226926, 0.4447154,
                       0.3560217, 0.2849129,
                       0.2384390, 0.2038462, 0.1873019))
init
m1.stan <- sampling(m1.model,
                    data = stan_datos,
                    pars = c("r_betas",
                             "phi",
                             "I_hoy"),
                    init = list(chain_1 = init,
                                chain_2 = init,
                                chain_3 = init,
                                chain_4 = init),
                    chains = 4,
                    iter = 4000,
                    warmup = 3000,
                    thin = 1,
                    cores = 4,
                    control = list(max_treedepth = 10,
                                   adapt_delta = 0.5))
# m1.vb <- vb(object = m1.model,
#             data = stan_datos,
#             init = list(r_beta = 0.61,
#                         f_int = c(0.5,0.4,0.3,0.3,0.2,0.2),
#                         phi = 3))


# load("m1.stan.rdat")
m1.stan
# save(m1.stan, file = "m1.stan.rdat")
# q()
# summary(m1.stan, pars = c("f_int"))$summary
# summary(m1.stan, pars = c("r_beta", "f_int", "phi"))
#
print(m1.stan, pars = c("r_betas", "phi"))
post <- rstan::extract(m1.stan)

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

# bayesplot::mcmc_pairs(as.matrix(m1.stan), np = np, pars = c("r_beta", "phi", names(init$f_int)),
#                       off_diag_args = list(size = 0.75))

stan_diag(m1.stan)



modelos <- list()
for(i in 1:length(post$r_beta)){
  modelos[[i]] <- list(modelo = i,
                       T_inc = 5.1562,
                       T_inf = 11.01072,
                       R_0 = post$R_0[i],
                       tiempos_int = stan_datos$fechas_dias,
                       efectos_int = post$f_int[i,],
                       phi = post$phi[i],
                       metodo = "stan")
  
}


# t_0 <- c(stan_datos$y0, 0)
t_0 <- as.numeric(t_0)
t_0 <- c(S = t_0[1],
         E = t_0[2],
         I = t_0[3],
         R = t_0[4],
         t = 0)
t_0

sims <- modelos %>%
  map_dfr(function(l){
    sir_simular(t_0 = t_0,
                parametros = l,
                n_dias = n_dias_ajuste - 1,
                FUN = bayes_seir) %>%
      mutate(casos_acumulados = (I + R)) %>%
      select(dia, casos_acumulados) %>%
      mutate(phi = l$phi,
             modelo = l$modelo)
  }) %>%
  mutate(lambda = casos_acumulados - lag(casos_acumulados, 1, default = 0)) %>%
  mutate(lambda = stan_datos$pob * lambda) %>%
  mutate(lambda = replace(lambda, lambda < 0, 1))
# sims %>% print(n = 100)

sims %>%
  group_by(dia) %>%
  summarize(lambda = mean(lambda)) %>%
  ungroup() %>%
  filter(dia > 0) %>%
  bind_cols(post = colMeans(post$I_hoy)) %>%
  mutate(diff = post - lambda) %>%
  print(n = 100) %>%
  ggplot() +
  # geom_histogram(aes(x = diff), bins = 20) +
  geom_point(aes(x = lambda, y = post)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic()

# sims

sims <- sims %>%
  mutate(casos_nuevos_pred = MASS::rnegbin(n = length(lambda), mu = lambda, theta = phi)) %>%
  group_by(dia) %>%
  summarise(lower_90 = compute_hpdi(casos_nuevos_pred, prob = 0.9)[1],
            median = median(casos_nuevos_pred),
            upper_90 = compute_hpdi(casos_nuevos_pred, prob = 0.9)[2]) %>%
  mutate(dia = dia + 1)
sims %>% print(n = 100)

p1 <- Dat %>%
  full_join(sims, by = "dia") %>%
  # left_join(apply(post$I_hoy,2,compute_hpdi, prob = 0.9) %>%
  #             t %>% as_tibble() %>%
  #             rename(stan_lower = V1, stan_upper = V2) %>%
  #             mutate(dia = 1:(n_dias_ajuste-1)),
  #           by = "dia") %>%
  # print(n = 200)
  ggplot(aes(x =fecha, y = sintomas_nuevos)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), color = "blue", alpha = 0.2) +
  # geom_ribbon(aes(ymin = stan_lower, ymax = stan_upper), color = "green", alpha = 0.2) +
  geom_line(aes(y = median), size = 2, col = "blue") +
  geom_vline(xintercept = fecha_inicio + fechas_dias) +
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

# res <- stan("casos/lognormal.stan",
#             data = list(n = 1000,
#                         # mu = 0,
#                         # sigma = 0.5,
#                         alpha = 2.712,
#                         beta = 1/4.06),
#             iter = 1,
#             algorithm = "Fixed_param",
#             chains = 1)
# extract(res)$rnd %>% as.vector() %>% summary()
# extract(res)$rnd %>% as.vector() %>% hist