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
library(deSolve)
source("casos/sir_funciones.r")
# https://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
# https://gabgoh.github.io/COVID/index.html
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

args <- list(reportes_diarios = "../datos/datos_abiertos/serie_tiempo_nacional_fecha_confirmacion.csv",
             # reportes_diarios = "../datos/ssa_dge/reportes_diarios.csv",
             dias_retraso = 15,
             dir_salida = "../sitio_hugo/static/imagenes/",
             periodo_ajuste = 100,
             fecha1 = "2020-03-01",
             fecha2 = "2020-03-15",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv")
args$fecha1 <- args$fecha1 %>% as.Date(format = "%Y-%m-%d")
args$fecha2 <- args$fecha2 %>% as.Date(format = "%Y-%m-%d")

# Lee base de datos
Tab <- read_csv(args$base_de_datos,
                col_types = cols(FECHA_ACTUALIZACION = col_date(format = "%Y-%m-%d"),
                                 FECHA_INGRESO = col_date(format = "%Y-%m-%d"),
                                 FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"),
                                 FECHA_DEF = col_character(),
                                 EDAD = col_number(),
                                 .default = col_character())) 
stop_for_problems(Tab)
Tab <- Tab %>%
  mutate(FECHA_DEF = parse_date(x = FECHA_DEF, format = "%Y-%m-%d", na = c("9999-99-99", "", "NA")),
         PAIS_NACIONALIDAD = parse_character(PAIS_NACIONALIDAD, na = c("99", "", "NA")),
         PAIS_ORIGEN = parse_character(PAIS_ORIGEN, na = c("97", "", "NA")))
Tab <- Tab %>%
  filter(RESULTADO == "1") %>%
  select(fecha_sintomas = FECHA_SINTOMAS)
Tab <- table(Tab$fecha_sintomas)
Tab <- tibble(fecha = names(Tab) %>% as.Date("%Y-%m-%d"),
              casos_nuevos = as.vector(Tab)) %>%
  mutate(casos_acumulados = cumsum(casos_nuevos)) %>%
  filter(casos_acumulados >= 5) %>%
  # filter(fecha >= "2020-03-01") %>%
  mutate(dia = as.numeric(fecha - min(fecha)))
Tab

# Precalcular dias
fecha_inicio <- min(Tab$fecha)
fecha_final <- Sys.Date()
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso + 1
fechas <- c("2020-03-02", "2020-03-09", "2020-03-16") %>% parse_date(format = "%Y-%m-%d")
fechas <- c("2020-03-09", "2020-03-16", "2020-03-23") %>% parse_date(format = "%Y-%m-%d")
fechas <- c("2020-03-02", "2020-03-09","2020-03-16", "2020-03-23", "2020-03-30") %>% parse_date(format = "%Y-%m-%d")
fechas <- c("2020-03-01","2020-03-08", "2020-03-15", "2020-03-22", "2020-03-29", "2020-04-05") %>% parse_date(format = "%Y-%m-%d")
# fechas <- fechas - 3
fechas
fechas_dias <- as.numeric(fechas - fecha_inicio)
fechas_dias <- sort(n_dias_ajuste - seq(from = 10, by = 10, length.out = 4))
fechas_dias

# Parameters to make optimization
# pob <- 135552447
T_inc <- c(4, 5, 6)
T_inf <- c(2, 3, 4)
# T_inc <- c(4, 5)
# T_inf <- c(2)   
pob <- 127792286
# T_inc <- c(5,5.2,5.4)
# T_inf <- c(2.5,3,3.5)
# T_inc <- 5
# T_inf <- 3

R_hat <- encontrar_R_0(real = Tab, n_dias_ajuste = n_dias_ajuste,
                       dias_int = fechas_dias,
                       T_inc = T_inc, T_inf = T_inf, pob = pob)
R_hat 

# Simular con parámetros estimados
sims <- simular_multiples_modelos(modelos = R_hat,
                                  FUN = sir, real = Tab, pob = pob,
                                  n_dias = n_dias)
ctds <- read_csv(args$reportes_diarios,
                 col_types = cols(fecha = col_date(format = "%Y-%m-%d"))) %>%
  select(fecha, casos_acumulados) %>%
  mutate(dia = as.numeric(fecha - fecha_inicio),
         modelo = "Confirmado")
# ctds
sims <- sims %>%
  split(.$modelo) %>%
  map_dfr(function(d){
    # d <- sims %>% filter(modelo == "m1")
    
    max_diff <- Tab %>%
      select(dia, casos_acumulados) %>%
      mutate(modelo = "real") %>%
      filter(dia < n_dias_ajuste) %>%
      left_join(d, by = "dia") %>%
      mutate(diff = abs(casos_acumulados.x - casos_acumulados.y)) %>%
      # arrange(desc(diff))
      select(diff) %>%
      max
    
    d
    # if(max_diff < 500)
    #   return(d)
  })
sims %>%
  filter(dia >= 50) %>%
  print(n = 300)

p1 <- Tab %>%
  select(fecha, dia, casos_acumulados) %>%
  mutate(modelo = "real") %>%
  bind_rows(sims %>% mutate(fecha = fecha_inicio + dia)) %>%
  bind_rows(ctds) %>%
  mutate(grupo = "Estimado (SEIR)") %>%
  mutate(grupo = replace(grupo, modelo == "real", "Inicio de síntomas")) %>%
  mutate(grupo = replace(grupo, modelo == "Confirmado", "Confirmado")) %>%
  mutate(grupo = factor(grupo, levels = c("Confirmado", "Inicio de síntomas", "Estimado (SEIR)"))) %>%
  mutate(modelo = factor(modelo, levels = c("Confirmado", "real", unique(sims$modelo)))) %>%
  
  # filter(fecha >= "2020-02-15") %>%
  # filter(!(modelo %in% para_quitar)) %>%

  ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
  geom_line(aes(col = grupo, size = grupo)) +
  geom_vline(xintercept = Sys.Date() - args$dias_retraso) +
  annotate("text", label = "Fin ajuste de curva",
           x = Sys.Date() - args$dias_retraso - 1.5,
           y = 10000, angle = 90,
           size = 6) +
  # geom_vline(xintercept = args$fecha1, col = "red") +
  # annotate("text", label = "Transmisión comunitaria",
  #          x = args$fecha1 - 1.5,
  #          y = 10000, angle = 90,
  #          size = 6) +
  geom_vline(xintercept = fecha_inicio + fechas_dias, col = "red") +
  scale_color_manual(values = c("#1b9e77", "#7570b3", "#d95f02"),
                     name = "") +
  # geom_vline(xintercept = args$fecha2, col = "red") +
  # annotate("text", label = "Medidas de mitigación",
  #          x = args$fecha2 - 1.5,
  #          y = 10000, angle = 90,
  #          size = 6) +
  scale_size_manual(values = c(2, 2, 0.2)) +
  guides(size = FALSE) +
  ylab("Casos acumulados") +
  xlab("Fecha") +
  scale_y_continuous(labels = scales::comma, breaks = function(lims){seq(from = 0, to = lims[2], by = 2500)}) +
  # scale_y_log10() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "sir_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
archivo <- "seir_estimados.csv"
write_csv(p1$data, archivo)

##### R_hat

p1 <- R_hat %>%
  map_dfr(~ tibble(R0 = c(.x$R_0, .x$R_0 * .x$efectos_int),
               dias = c(0, fechas_dias),
               modelo = .x$modelo)) %>%
  mutate(fecha = fecha_inicio + dias) %>%
  
  ggplot(aes(x = fecha, y = R0, group = modelo)) +
  geom_line() +
  ylab("Promedio de infectados por enfermo (R_t)") +
  xlab("Fecha") +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "sir_nacional_r0.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional_r0@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

  # dgamma_otimizable <- function(x, dat, dias_cambio = 10){
#   # dat <- d
#   # x <- c(4.2, 1.32)
#   
#   dat$efectos_norm <- dat$efectos / sum(dat$efectos)
#   
#   gamma_x <- (dat$dias / dias_cambio) + 1
#   # gamma_x <- dat$dias + 1
#   # gamma_x <- gamma_x / max(gamma_x)
#   
#   dat$pred <- dgamma(x = gamma_x, shape = x[1], rate = x[2])
#   
#   # dat <- dat %>% filter(dias > 0)
#   ss <- sum((dat$efectos_norm - dat$pred) ^ 2)
#   # ss <- sum(log(dat$pred / dat$efectos_norm) ^ 2)
#   
#   ss
# }
# 
# proyectar_R_t <- function(l, n_dias = 365, dias_cambio = 10){
#   # n_dias <- dias_proyeccion
#   # dias_cambio <- 10
#   # l <- R_hat[[1]]
#   
#   # Crear tibble con datos
#   d <- tibble(dias = c(0, l$tiempos_int),
#               efectos = c(l$R_0, l$R_0 * l$efectos_int))
#   # d <- tibble(dias = c(l$tiempos_int),
#   #             efectos = c(l$R_0 * l$efectos_int))
#   # d <- tibble(dias = c(0, 10, 20, 30, 40),
#   #             efectos = c(2, 4, 2.5, 1.8, 1.4))
#   # d
#   
#   # m1 <- MASS::fitdistr(x = d$efectos / sum(d$efectos), densfun = "gamma")
#   # m1 <- MASS::fitdistr(x = d$efectos / sum(d$efectos), densfun = "gamma")
#   # m1
#   ajuste_R_0 <- nlm(f = dgamma_otimizable , p = c(4,2), dat = d, dias_cambio = dias_cambio)
#   # ajuste_R_0 <- nlm(f = dgamma_otimizable , p = c(80,30), dat = d, dias_cambio = dias_cambio)
#   # ajuste_R_0
#   
#   dias_nuevos_R_0 <- seq(from = max(d$dias) + dias_cambio , to = n_dias, by = dias_cambio)
#   gamma_x <- (dias_nuevos_R_0 / dias_cambio) + 1
#   # gamma_x <- dias_nuevos_R_0  + 1
#   # gamma_x <- gamma_x / max((d$dias / dias_cambio) + 1)
#   R_0_proyectadas <- dgamma(x = gamma_x, 
#                             shape = ajuste_R_0$estimate[1],
#                             rate = ajuste_R_0$estimate[2]) * sum(d$efectos)
#   # R_0_proyectadas[R_0_proyectadas < 0.8] <- 0.8
#   plot(d$dias , d$efectos)
#   # gamma_x <- (d$dias / dias_cambio)
#   # gamma_x <- d$dias + 1
#   # plot(d$dias,
#   #      dgamma(x = gamma_x, shape = ajuste_R_0$estimate[1], rate = ajuste_R_0$estimate[2]) * sum(d$efectos))
#   
#   efectos_proyectados <- R_0_proyectadas / l$R_0
#   efectos_proyectados[ efectos_proyectados < 0.8 ] <- 0.8
#   l$tiempos_int <- c(l$tiempos_int, dias_nuevos_R_0)
#   l$efectos_int <- c(l$efectos_int, efectos_proyectados)
#   
#   l
# }
# 
# 
# fecha_proyeccion <- parse_date("2020-07-01", format = "%Y-%m-%d")
# dias_proyeccion <- as.numeric(fecha_proyeccion - fecha_inicio)
# 
# R_proyecciones <- R_hat %>%
#   map(proyectar_R_t, n_dias = dias_proyeccion, dias_cambio = 10) 
# R_hat[[1]]$efectos_int
# R_proyecciones[[1]]$efectos_int
# R_hat[[1]]$efectos_int * R_hat[[1]]$R_0
# R_proyecciones[[1]]$R_0 * R_proyecciones[[1]]$efectos_int
# 
# 
# simular_multiples_modelos(modelos = R_proyecciones,
#                           FUN = sir, real = Tab, pob = pob,
#                           # n_dias = as.numeric(parse_date("2020-07-01", format = "%Y-%m-%d") - fecha_inicio),
#                           n_dias = dias_proyeccion) %>%
#   split(.$modelo) %>%
#   map_dfr(~.x %>% mutate(casos_nuevos = casos_acumulados - lag(casos_acumulados, 1, default = min(casos_acumulados)))) %>%
#   mutate(fecha = fecha_inicio + dia) %>%
#   
#   ggplot(aes(x = fecha, y = casos_nuevos, group = modelo)) +
#   geom_line()

