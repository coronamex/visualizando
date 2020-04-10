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
parametros <-  c(R_t = 3, T_inf = 2, T_inc = 6, L = 0.04)
t_0 <- c(S = (pob - 1) / pob,
         E = 0,
         I = 1 / pob,
         R = 0,
         D = 0)


pred <- sir_simular(t_0 = t_0, parametros = parametros,
                    n_dias = 120, FUN = sir)
dat <- pred %>%
  mutate(casos_acumulados = (I + R + D),
         muertes_acumuladas = D) %>%
  mutate(letalidad = muertes_acumuladas/casos_acumulados,
         letalidad2 = muertes_acumuladas/lag(casos_acumulados,6)) %>%
  mutate(casos_nuevos = casos_acumulados - lag(casos_acumulados)) %>%
  select(dia, casos_nuevos, casos_acumulados, muertes_acumuladas, letalidad, letalidad2) %>%
  pivot_longer(-dia, names_to = "grupo", values_to="porcentage_poblacion") 

p1 <- dat %>%
  filter(grupo != "casos_acumulados") %>%
  mutate(grupo = replace(grupo, grupo == "casos_nuevos", "Casos\nnuevos")) %>%
  mutate(grupo = replace(grupo, grupo == "muertes_acumuladas", "Muertes\ntotales")) %>%
  mutate(grupo = replace(grupo, grupo == "letalidad2", "Letalidad\ninstantanea")) %>%
  mutate(grupo = replace(grupo, grupo == "letalidad", "Letalidad\ncon retraso")) %>%
  ggplot(aes(x = dia, y = porcentage_poblacion)) +
  geom_line(aes(color = grupo), size = 3) +
  geom_hline(yintercept = 0.04) +
  annotate("text",
           x = 30,
           y = 0.045,
           label = 'Letalidad verdadera',
           size = 8,
           hjust = "middle",
           parse = FALSE) +
  scale_y_continuous(labels=scales::percent) +
  ylab(label = "% de la población") +
  xlab(label = "Día") +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "letalidad_diagrama.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "letalidad_diagrama@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

