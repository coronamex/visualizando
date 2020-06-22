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
             dias_extra_sim = 0,
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
                   n_int = 2,
                   fechas_dias = c(10,20))
  
# m1.stan <- stan("casos/sir.stan", data = stan_datos,
#                 pars = c("T_inc", "T_inf", "r_b4eta",
#                          "f_int",
#                          "params", "R_0", "E_hoy"),
#                 chains = 1, iter = 100)
load("m1.stan.rdat")
m1.stan
save(m1.stan, file = "m1.stan.rdat")

#
post <- extract(m1.stan)

modelos <- list()
for(i in 1:50){
  modelos[[i]] <- list(modelo = i,
                       T_inc = post$T_inc[i],
                       T_inf = post$T_inf[i],
                       R_0 = post$R_0[i],
                       tiempos_int = stan_datos$fechas_dias,
                       efectos_int = post$f_int[i,],
                       metodo = "stan")
  
}


t_0 <- c(stan_datos$y0, 0)
t_0 <- c(S = t_0[1],
         E = t_0[2],
         I = t_0[3],
         R = t_0[4],
         t = 0)

  
sims <- simular_seir_post(modelos = modelos,
                    FUN = sir,
                    n_dias = nrow(Dat),
                    t_0 = t_0) %>%
  mutate(lambda = casos_acumulados - lag(casos_acumulados, 1, default = 0)) %>%
  mutate(lambda = stan_datos$pob * lambda) %>%
  mutate(casos_nuevos_pred = rpois(n = length(lambda), lambda = lambda)) %>%
  mutate(casos_nuevos_pred = replace(casos_nuevos_pred, is.na(casos_nuevos_pred), 0)) %>%
  group_by(dia) %>%
  summarise(lower_90 = compute_hpdi(casos_nuevos_pred, prob = 0.9)[1],
            median = median(casos_nuevos_pred),
            upper_90 = compute_hpdi(casos_nuevos_pred, prob = 0.9)[2]) %>%
  mutate(dia = dia + 1)
sims

Dat %>%
  left_join(sims, by = "dia") %>%
  ggplot(aes(x =fecha, y = sintomas_nuevos)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), color = "blue", alpha = 0.2) +
  geom_line(aes(y = median), size = 2, colo = "blue") +
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




