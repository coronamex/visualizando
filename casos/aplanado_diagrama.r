library(tidyverse)
library(deSolve)

# Create an SIR function
sir <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    # SEIR
    dS <- -(R_t / T_inf) * (I * S)
    dE <- (R_t / T_inf) * (I * S) - ((1 / T_inc) * E)
    dI <- ((1 / T_inc) * E) - ((1 / T_inf) * I * (1-L)) - ((1/T_inf) * I * L)
    dR <- (1 / T_inf) * I * (1-L)
    dD <- (1/ T_inf) * I * L
    
    return(list(c(dS, dE, dI, dR, dD)))
  })
}


sir_simular <- function(t_0, parametros, n_dias, FUN = sir){
  # funcion
  FUN <- match.fun(FUN)
  
  # Definir dias
  dias <- seq(0, n_dias, by = 1)
  
  # Usar ode para resolver, convertir a tibble
  pred <- ode(y = t_0, times = dias, func = FUN, parms = parametros)
  pred <- tibble(dia = pred[,"time"],
                 S = pred[,"S"],
                 E = pred[,'E'],
                 I = pred[,"I"],
                 R = pred[,"R"],
                 D = pred[,"D"])
  
  return(pred)
}


args <- list(dir_salida = "../sitio_hugo/static/imagenes/")

pob <- 1e7
t_0 <- c(S = (pob - 1) / pob,
         E = 0,
         I = 1 / pob,
         R = 0,
         D = 0)

# Simular no aplanamiento
parametros <-  c(R_t = 3, T_inf = 2, T_inc = 6, L = 0.04)
pred_orig  <- sir_simular(t_0 = t_0, parametros = parametros,
                    n_dias = 365, FUN = sir)


# Simular aplanamiento
parametros <-  c(R_t = 1.5, T_inf = 2, T_inc = 6, L = 0.04)
pred_apl  <- sir_simular(t_0 = t_0, parametros = parametros,
                          n_dias = 365, FUN = sir)

# Combinar modelos
dat <- bind_rows(pred_orig %>%
                   mutate(casos_acumulados = I + R + D) %>%
                   select(dia, casos_acumulados, muertes_acumuladas = D) %>%
                   mutate(casos_nuevos = casos_acumulados - lag(casos_acumulados, 1, default = 0),
                          muertes_nuevas = muertes_acumuladas - lag(muertes_acumuladas, 1, default = 0)) %>%
                   mutate(modelo = "orig",
                          momento = "ascenso") %>%
                   mutate(momento = replace(momento, dia > dia[casos_nuevos == max(casos_nuevos)], "descenso")),
                 pred_apl %>%
                   mutate(casos_acumulados = I + R + D) %>%
                   select(dia, casos_acumulados, muertes_acumuladas = D) %>%
                   mutate(casos_nuevos = casos_acumulados - lag(casos_acumulados, 1, default = 0),
                          muertes_nuevas = muertes_acumuladas - lag(muertes_acumuladas, 1, default = 0)) %>%
                   mutate(modelo = "aplanado",
                          momento = "ascenso") %>%
                   mutate(momento = replace(momento, dia > dia[casos_nuevos == max(casos_nuevos)], "descenso")))

dat <- dat %>%
  mutate(color = interaction(modelo, momento, sep = "."))

p1 <- ggplot(dat, aes(x = dia, y = casos_nuevos, group = modelo)) +
  geom_line(aes(col = color), size = 3) +
  scale_color_manual(values = c("#4575b4","#d73027", "#abd9e9", "#fdae61"),
                     labels = c("Sin aplanado", "Con aplanado", "", ""),
                     name = "Simulación") +
  scale_y_continuous(labels = scales::percent) +
  ylab("Casos nuevos\n(% poblacional)") +
  xlab("Día") +
  guides(color = guide_legend(override.aes = list(color = c("red", "blue", NA, NA)))) +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        # plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

dat <- dat %>%
  mutate(casos_acumulados = floor(pob * casos_acumulados),
         muertes_acumuladas = floor(pob * muertes_acumuladas),
         casos_nuevos = floor(pob * casos_nuevos),
         muertes_nuevas = floor(pob * muertes_nuevas))

p2 <- dat %>%
  ggplot(aes(x = casos_acumulados, y = casos_nuevos, group = modelo)) +
  geom_point(aes(col = color)) +
  scale_color_manual(values = c("#4575b4","#d73027", "#abd9e9", "#fdae61"),
                     labels = c("Sin aplanado", "Con aplanado", "", ""),
                     name = "") +
  ylab("Casos o muertes nuevas") +
  xlab("Casos o muertes acumuladas") +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  # guides(color = guide_legend(override.aes = list(color = c("red", "blue", NA, NA)))) +
  guides(color = FALSE) +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        # plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.key = element_blank(),
        legend.text = element_text(size = 12),
        legend.background = element_blank())

pp <- cowplot::plot_grid(p1, p2, nrow = 2)
# ggsave("test.png", pp, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "aplanamiento_diagrama.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "aplanamiento_diagrama@2x.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)

# roll_mean <- tibbletime::rollify(mean, window = 7)
# dat %>%
#   split(.$modelo) %>%
#   map_dfr(function(d){
#     d %>%
#       arrange(dia) %>%
#       mutate(incremento = roll_mean(casos_nuevos)) %>%
#       mutate(incremento = incremento / casos_acumulados)
#   }) %>%
# 
#   filter(casos_acumulados >= 1000) %>%
# 
#   ggplot(aes(x = casos_acumulados, y = incremento, group = modelo)) +
#   geom_line(aes(col = color)) +
#   scale_y_continuous(labels = scales::percent) +
#   scale_x_log10()
