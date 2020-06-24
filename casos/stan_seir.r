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

# Determinando fechas
fecha_inicio <- min(Dat$fecha)
fecha_final <- max(Dat$fecha)
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso

fechas_dias <- as.numeric((c("2020-03-15") %>% parse_date(format = "%Y-%m-%d")) - fecha_inicio)
fechas_dias <- c(fechas_dias, seq(from = fechas_dias[length(fechas_dias)] + 15,
                                  to = n_dias_ajuste,
                                  by = 15))
fechas_dias

dat_train <- Dat %>%
  filter(dia < n_dias_ajuste)

t_0 <- c(pob - 2 * Dat$sintomas_acumulados[1],
         Dat$sintomas_acumulados[1],
         Dat$sintomas_acumulados[1],
         0) / pob

dat_train <- dat_train %>%
  filter(dia > 0)

stan_datos <- list(n_obs = nrow(dat_train),
                   n_params = 3,
                   n_difeq = 4,
                   pob = pob,
                   y = dat_train$sintomas_nuevos,
                   t0 = 0,
                   ts = dat_train$dia,
                   y0 = t_0,
                   n_int = length(fechas_dias),
                   fechas_dias = fechas_dias)
  
m1.stan <- stan("casos/sir.stan", data = stan_datos,
                pars = c("T_inc", "T_inf", "r_beta",
                         "f_int", "R_0", "E_hoy"),
                init = list(list(T_inc = 6.29,
                                 T_inf = 4.1,
                                 r_beta = 0.62,
                                 f_int = rep(0.8, times = length(fechas_dias)))),
                chains = 1, iter = 2000,
                control = list(max_treedepth = 10))
# load("m1.stan.rdat")
m1.stan
# save(m1.stan, file = "m1.stan.rdat")

#
post <- extract(m1.stan)
plot(1:(n_dias_ajuste - 1), colMeans(post$E_hoy))

traceplot(m1.stan, pars = c("T_inc", "T_inf", "r_beta", "f_int"))
# pairs(m1.stan, pars = c("T_inc", "T_inf", "r_beta", "f_int"))

modelos <- list()
for(i in 1:length(post$r_beta)){
  modelos[[i]] <- list(modelo = i,
                       T_inc = post$T_inc[i],
                       T_inf = post$T_inf[i],
                       R_0 = post$R_0[i],
                       tiempos_int = stan_datos$fechas_dias,
                       efectos_int = post$f_int[i,],
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
sims <- simular_seir_post(modelos = modelos,
                    FUN = bayes_seir,
                    n_dias = n_dias_ajuste - 1,
                    t_0 = t_0) %>%
  mutate(lambda = casos_acumulados - lag(casos_acumulados, 1, default = 0)) %>%
  mutate(lambda = stan_datos$pob * lambda) %>%
  mutate(lambda = replace(lambda, lambda < 0, 1))
# sims %>% print(n = 100)

sims %>%
  group_by(dia) %>%
  summarize(lambda = mean(lambda)) %>%
  ungroup() %>%
  filter(dia > 0) %>%
  bind_cols(post = colMeans(post$E_hoy)) %>%
  mutate(diff = post - lambda) %>%
  print(n = 100) %>%
  ggplot() +
  # geom_histogram(aes(x = diff), bins = 20) +
  geom_point(aes(x = lambda, y = post)) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_log10() +
  scale_y_log10() +
  theme_classic()



sims <- sims %>%
  mutate(casos_nuevos_pred = rpois(n = length(lambda), lambda = lambda)) %>%
  group_by(dia) %>%
  summarise(lower_90 = compute_hpdi(casos_nuevos_pred, prob = 0.9)[1],
            median = median(casos_nuevos_pred),
            upper_90 = compute_hpdi(casos_nuevos_pred, prob = 0.9)[2]) %>%
  mutate(dia = dia + 1)
# sims %>% print(n = 100)

p1 <- Dat %>%
  full_join(sims, by = "dia")  %>%
  mutate(fecha = min(fecha, na.rm = TRUE) + dia - 1) %>%
  # print(n = 200)
  ggplot(aes(x =fecha, y = sintomas_nuevos)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), color = "blue", alpha = 0.2) +
  geom_line(aes(y = median), size = 2, col = "blue") +
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
