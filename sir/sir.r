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
source("sir/sir_funciones.r")
# https://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
# https://gabgoh.github.io/COVID/index.html
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

args <- list(reportes_diarios = "../datos/ssa_dge/reportes_diarios.csv",
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
  mutate(dia = as.numeric(fecha - min(fecha)))
Tab

# Precalcular dias
fecha_inicio <- min(Tab$fecha)
fecha_final <- Sys.Date()
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso + 1
fecha1_dia <- as.numeric(args$fecha1 - fecha_inicio)
fecha2_dia <- as.numeric(args$fecha2 - fecha_inicio)
fecha1_dia
fecha2_dia

# Parameters to make optimization
# pob <- 135552447
T_inc <- c(3, 4, 5, 6)
T_inf <- c(1, 2, 3)
# T_inc <- c(4, 5)
# T_inf <- c(1, 2)
pob <- 127792286
# T_inc <- c(5,5.2,5.4)
# T_inf <- c(2.5,3,3.5)
# T_inc <- 2
# T_inf <- 1

R_hat <- encontrar_R_0(real = Tab, n_dias_ajuste = n_dias_ajuste,
                       fecha1_dia = fecha1_dia,
                       fecha2_dia = fecha2_dia,
                       T_inc = T_inc, T_inf = T_inf, pob = pob)
R_hat %>%
  print(n = 100)

# Simular con parámetros estimados
sims <- simular_multiples_modelos(modelos = R_hat %>%
                                    filter(!is.na(metodo)),
                                  FUN = sir, real = Tab, pob = pob,
                                  n_dias = n_dias,
                                  fecha1_dia = fecha1_dia,
                                  fecha2_dia = fecha2_dia)
ctds <- read_csv(args$reportes_diarios) %>%
  select(fecha, casos_acumulados) %>%
  mutate(dia = as.numeric(fecha - fecha_inicio),
         modelo = "Confirmado")
# ctds
# sims %>%
#   filter(dia == max(Tab$fecha) - min(Tab$fecha)) %>%
#   arrange(casos_acumulados) %>%
#   # filter(casos_acumulados < max(Tab$casos_acumulados)) %>%
#   filter(casos_acumulados < 0) %>%
#   print(n = 100) %>%
#   select(modelo) %>%
#   unlist -> para_quitar

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
      
    if(max_diff < 500)
      return(d)
  })

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
  
  # filter(!(modelo %in% para_quitar)) %>%

  ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
  geom_line(aes(col = grupo, size = grupo)) +
  geom_vline(xintercept = Sys.Date() - args$dias_retraso) +
  annotate("text", label = "Fin ajuste de curva",
           x = Sys.Date() - args$dias_retraso - 1.5,
           y = 10000, angle = 90,
           size = 6) +
  geom_vline(xintercept = args$fecha1, col = "red") +
  annotate("text", label = "Transmisión comunitaria",
           x = args$fecha1 - 1.5,
           y = 10000, angle = 90,
           size = 6) +
  scale_color_manual(values = c("#1b9e77", "#7570b3", "#d95f02")) +
  geom_vline(xintercept = args$fecha2, col = "red") +
  annotate("text", label = "Medidas de mitigación",
           x = args$fecha2 - 1.5,
           y = 10000, angle = 90,
           size = 6) +
  scale_size_manual(values = c(2, 2, 0.2)) +
  guides(size = FALSE) +
  ylab("Casos acumulados") +
  xlab("Fecha") +
  scale_y_continuous(labels = scales::comma) +
  # scale_y_log10() +
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
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "sir_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

summary(R_hat$R_hat)
summary(R_hat$f1_hat)
summary(R_hat$f2_hat)

############
# Centinela <- tibble(semana = 8:13,total_usmer = c(43329, 43113, 45420, 42072, 48679, 33500),
#                     posibles_usmer = c(2472, 2597, 2465, 2919, 4081,5215),
#                     pruebas_usmer = c(961, 1034, 991, 1423, 2360, 3181),
#                     totales_nacional = c(583545, 561882,
#                                          609935, 550483,
#                                          652335, 488071),
#                     posibles_nacional = c(42259, 43534,
#                                           40680, 48186,
#                                           69718, 90678),
#                     positivos_usmer = c(3,3,27,167,295,544),
#                     estimados = c(133,131,751,4718,7566,13221))
# Centinela <- Centinela %>%
#   mutate(fecha = "2019-12-29" %>% as.Date(format = "%Y-%m-%d")) %>%
#   mutate(fecha = fecha + 7*(semana),
#          casos_acumulados = cumsum(estimados)) %>%
#   transmute(fecha,
#             casos_nuevos = casos_acumulados - lag(casos_acumulados, 1, default = 0),
#             casos_acumulados,
#             dia = as.numeric(fecha - min(fecha)))
# 
# T_inc <- c(5,5.2,5.4)
# T_inf <- c(2.5,3,3.5)
# T_inc <- c(2, 3, 4, 5, 6, 7, 8)
# T_inf <- c(1, 2, 3, 4, 5, 6)
# 
# R_hat <- encontrar_R_0(Centinela, dias_retraso = args$dias_retraso,
#                        periodo_ajuste = args$periodo_ajuste,
#                        T_inc = T_inc, T_inf = T_inf, pob = pob,
#                        fecha1 = args$fecha1 )
# R_hat
# 
# # Simular con parámetros estimados
# sims <- simular_multiples_modelos(modelos = R_hat, FUN = sir, real = Centinela, pob = pob,
#                                   n_dias = args$dias_retraso + args$periodo_ajuste,
#                                   fecha1 = args$fecha1)
# 
# sims %>%
#   # filter(dia == Sys.Date() - min(Centinela$fecha)) %>%
#   filter(dia == max(Centinela$fecha) - min(Centinela$fecha)) %>%
#   # arrange(desc(casos_acumulados)) %>%
#   # print(n = 1000)
#   filter(casos_acumulados < 21000) %>%
#   print(n = 100) %>%
#   select(modelo) %>%
#   unlist -> para_quitar
# 
# 
# p1 <- Centinela %>%
#   filter(fecha >= (Sys.Date() - args$dias_retraso - args$periodo_ajuste + 1)) %>%
#   mutate(dia = as.numeric(fecha - min(fecha))) %>%
#   select(dia, casos_acumulados) %>%
#   mutate(modelo = "Centinela") %>%
#   bind_rows(sims) %>%
#   mutate(fecha = min(Tab$fecha) + dia)  %>%
#   filter(fecha <= Sys.Date()) %>%
#   select(-dia) %>%
#   
#   mutate(grupo = "SEIR + Centinela") %>%
#   mutate(grupo = replace(grupo, modelo == "Centinela", "Centinela")) %>%
#   # mutate(grupo = factor(grupo, levels = c("Confirmado", "Inicio de síntomas", "Estimado (SEIR)"))) %>%
#   # mutate(modelo = factor(modelo, levels = c("Confirmado", "real", unique(sims$modelo)))) %>%
#   # filter(modelo != "m2") %>%
#   # filter(grupo != "Inicio de síntomas") %>%
#   # filter(!(modelo %in% para_quitar)) %>%
#   filter(!(modelo %in% para_quitar)) %>%
#   
#   ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
#   geom_line(aes(col = grupo, size = grupo)) +
#   geom_vline(xintercept = Sys.Date() - args$dias_retraso) +
#   annotate("text", label = "Fin ajuste de curva",
#            x = Sys.Date() - args$dias_retraso - 1.5,
#            y = 300000, angle = 90,
#            size = 6) +
#   geom_vline(xintercept = args$fecha1, col = "red") +
#   annotate("text", label = "Medidas de mitigación",
#            x = args$fecha1 - 1.5,
#            y = 300000, angle = 90,
#            size = 6) +
#   scale_color_manual(values = c("#e7298a", "#66a61e")) +
#   scale_size_manual(values = c(2, 0.1)) +
#   guides(size = FALSE) +
#   ylab("Casos acumulados") +
#   xlab("Fecha") +
#   scale_y_continuous(labels = scales::comma) +
#   AMOR::theme_blackbox() +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill = NA, color = "black", size = 3),
#         legend.position = "top",
#         legend.text = element_text(size = 14),
#         axis.title = element_text(size = 20),
#         axis.text = element_text(size = 10, color = "black"),
#         plot.margin = margin(l = 20, r = 20))
# p1
# # ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "sir_nacional_centinela.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "sir_nacional_centinela@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
# 
# 
# summary(R_hat$R_hat)     
# summary(R_hat$f1_hat)
