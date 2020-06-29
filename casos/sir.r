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
# library(deSolve)
# source("casos/sir_funciones.r")
# source("util/leer_datos_abiertos.r")
# https://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
# https://gabgoh.github.io/COVID/index.html
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

args <- list(#reportes_diarios = "../datos/datos_abiertos/serie_tiempo_nacional_fecha_confirmacion.csv.gz",
             # dias_retraso = 15,
             dir_salida = "../sitio_hugo/static/imagenes/",
             casos_nacionales = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv.gz",
             dir_estimados = "estimados/",
             fecha_inicio = "2020-03-01",
             dias_extra = 0)

# Leer y filtrar casos observados
Tab <- read_csv(args$casos_nacionales,
                 col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                  .default = col_number()))
Tab <- Tab %>%
  filter(fecha >= args$fecha_inicio) %>%
  select(fecha, sintomas_nuevos, sintomas_acumulados) 

final_datos <- max(Tab$fecha)
  
# Leer estimados
archivo <- file.path(args$dir_estimados, "bayes_seir_nacional.csv")
Est <- read_csv(archivo,
                col_types = cols(fecha_estimacion = col_date(format = "%Y-%m-%d"),
                                 fecha = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))
Est <- Est %>%
  filter(fecha >= args$fecha_inicio) %>%
  filter(fecha <= final_datos + args$dias_extra)
fin_ajuste_curva <- max(Est$fecha_estimacion) - 16
# fin_ajuste_curva

#### Preparar datos para SEIR casos nuevos
mu_est <- Est %>%
  filter(fecha <= fin_ajuste_curva) %>%
  select(fecha, nuevos_mu_10, nuevos_mu_50, nuevos_mu_90)
obs_est <- Est %>%
  filter(fecha > fin_ajuste_curva) %>%
  select(fecha, nuevos_obs_10, nuevos_obs_50, nuevos_obs_90)

final_datos
est_final <- obs_est %>% filter(fecha == final_datos) %>%
  select(nuevos_obs_10, nuevos_obs_90)
ymax <- max(obs_est$nuevos_obs_90)
Tab

bind_rows(Tab %>%
            select(fecha, sintomas_nuevos) %>%
            mutate(modelo = "real") %>%
            rename(q_50 = sintomas_nuevos),
          mu_est %>%
            mutate(modelo = "final_mu") %>%
            rename(q_10 = nuevos_mu_10,
                   q_50 = nuevos_mu_50,
                   q_90 = nuevos_mu_90),
          obs_est %>%
            mutate(modelo = "final_obs") %>%
            rename(q_10 = nuevos_obs_10,
                   q_50 = nuevos_obs_50,
                   q_90 = nuevos_obs_90))

p1 <- Tab %>%
  select(fecha, sintomas_nuevos) %>%
  full_join(bind_rows(mu_est %>%
                        mutate(modelo = "final_mu") %>%
                        rename(q_10 = nuevos_mu_10,
                               q_50 = nuevos_mu_50,
                               q_90 = nuevos_mu_90),
                      obs_est %>%
                        mutate(modelo = "final_obs") %>%
                        rename(q_10 = nuevos_obs_10,
                               q_50 = nuevos_obs_50,
                               q_90 = nuevos_obs_90)),
            by = "fecha") %>%
  
  ggplot(aes(x = fecha)) +
  geom_rect(aes(linetype = "Casos en estos días pueden aumentar"),
                xmin = final_datos - 15,
                xmax = final_datos,
                ymin = -Inf,
                ymax = Inf,
                fill = "pink") +
  # geom_rect(aes(linetype = "futuro"),
  #           xmin = final_datos,
  #           xmax = final_datos + args$dias_extra,
  #           ymin = -Inf,
  #           ymax = Inf,
  #           fill = "lightblue") +
  scale_linetype_manual(values = c("Casos en estos días pueden aumentar" = 0),
                        name = "",
                        guide = guide_legend(override.aes = list(fill = c("pink")))) +

  
  geom_bar(aes(y = sintomas_nuevos), width = 1, stat = "identity", color = "darkgrey", fill = "darkgrey") +
  

  
  geom_line(aes(y = q_50, col = modelo)) +
  geom_ribbon(aes(ymin = q_10, ymax = q_90, fill = modelo, col = modelo), alpha = 0.2) +
  scale_color_manual(values = c("#fb9a99", "#e31a1c"), guide = FALSE) +
  scale_fill_manual(values = c("#fb9a99", "#e31a1c"),
                    guide = FALSE) +

  geom_vline(xintercept = fin_ajuste_curva + 0.5) +
  annotate("text", label = paste("Fin ajuste de curva:", fin_ajuste_curva),
           x = fin_ajuste_curva - 3,
           y = ymax / 3.5, angle = 90,
           size = 4) +
  
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::breaks_extended(n=7)) +

  ylab("Número de casos nuevos") +
  xlab("Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "sir_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


#### Preparar datos para SEIR aplanamiento
# Tab
# Est

# Modelo final
mu_est <- Est %>%
  filter(fecha <= fin_ajuste_curva) %>%
  select(fecha, acum_mu_10, acum_mu_50, acum_mu_90)
obs_est <- Est %>%
  filter(fecha > fin_ajuste_curva) %>%
  select(fecha, acum_obs_10, acum_obs_50, acum_obs_90)
ymax <- max(obs_est$acum_obs_90)

# Pre-sana distancia
archivo <- file.path(args$dir_estimados, "bayes_seir_nacional_pre_2020-03-16.csv")
Est_pre1 <- read_csv(archivo,
                col_types = cols(fecha_estimacion = col_date(format = "%Y-%m-%d"),
                                 fecha = col_date(format = "%Y-%m-%d"),
                                 .default = col_number())) %>%
  filter(fecha >= args$fecha_inicio) %>%
  filter(fecha <= final_datos + args$dias_extra)
mu_est_pre1 <- Est_pre1 %>%
  filter(fecha <= fin_ajuste_curva) %>%
  select(fecha, acum_mu_10, acum_mu_50, acum_mu_90)
obs_est_pre1 <- Est_pre1 %>%
  filter(fecha > fin_ajuste_curva) %>%
  select(fecha, acum_obs_10, acum_obs_50, acum_obs_90)

# Pre-sana distancia
archivo <- file.path(args$dir_estimados, "bayes_seir_nacional_pre_2020-04-15.csv")
Est_pre2 <- read_csv(archivo,
                     col_types = cols(fecha_estimacion = col_date(format = "%Y-%m-%d"),
                                      fecha = col_date(format = "%Y-%m-%d"),
                                      .default = col_number())) %>%
  filter(fecha >= args$fecha_inicio) %>%
  filter(fecha <= final_datos + args$dias_extra)
mu_est_pre2 <- Est_pre2 %>%
  filter(fecha <= fin_ajuste_curva) %>%
  select(fecha, acum_mu_10, acum_mu_50, acum_mu_90)
obs_est_pre2 <- Est_pre2 %>%
  filter(fecha > fin_ajuste_curva) %>%
  select(fecha, acum_obs_10, acum_obs_50, acum_obs_90)



p1 <- bind_rows(Tab %>%
            select(fecha, sintomas_acumulados) %>%
            mutate(modelo = "real") %>%
            rename(q_50 = sintomas_acumulados),
          mu_est %>%
            mutate(modelo = "final_mu") %>%
            rename(q_10 = acum_mu_10,
                   q_50 = acum_mu_50,
                   q_90 = acum_mu_90),
          obs_est %>%
            mutate(modelo = "final_obs") %>%
            rename(q_10 = acum_obs_10,
                   q_50 = acum_obs_50,
                   q_90 = acum_obs_90),
          
          mu_est_pre1 %>%
            mutate(modelo = "pre1_mu") %>%
            rename(q_10 = acum_mu_10,
                   q_50 = acum_mu_50,
                   q_90 = acum_mu_90),
          obs_est_pre1 %>%
            mutate(modelo = "pre1_obs") %>%
            rename(q_10 = acum_obs_10,
                   q_50 = acum_obs_50,
                   q_90 = acum_obs_90),
          
          mu_est_pre2 %>%
            mutate(modelo = "pre2_mu") %>%
            rename(q_10 = acum_mu_10,
                   q_50 = acum_mu_50,
                   q_90 = acum_mu_90),
          obs_est_pre2 %>%
            mutate(modelo = "pre2_obs") %>%
            rename(q_10 = acum_obs_10,
                   q_50 = acum_obs_50,
                   q_90 = acum_obs_90)
          ) %>%
  mutate(modelo = factor(modelo, levels = c("real", "final_mu", "final_obs",
                                            "pre1_mu", "pre1_obs",
                                            "pre2_mu", "pre2_obs"))) %>%
  
  ggplot(aes(x = fecha, group = modelo, col = modelo)) +
  # geom_rect(aes(xmin = final_datos - 15, xmax = final_datos,
  #               ymin = -Inf, ymax = Inf,
  #               fill = "pink", col = NA)) +
  # scale_fill_identity(guide = "legend", name = "", labels = "Casos en estas fechas pueden aumentar") +
  # geom_rect(aes(xmin = final_datos, xmax = final_datos + args$dias_extra,
  #               ymin = -Inf, ymax = Inf, fill = "lightblue", col = NA)) +
  
  geom_line(aes(y = q_50)) +
  geom_ribbon(aes(ymin = q_10, ymax = q_90, fill = modelo), alpha = 0.2) +
  
  scale_color_manual(values = c("black",
                                "#fb9a99", "#e31a1c",
                                "#a6cee3", "#1f78b4",
                                "#b2df8a", "#33a02c"), ) +
  scale_fill_manual(values = c("black",
                                "#fb9a99", "#e31a1c",
                                "#a6cee3", "#1f78b4",
                                "#b2df8a", "#33a02c")) +
  
  
  geom_vline(xintercept = fin_ajuste_curva + 0.5) +
  annotate("text", label = paste("Fin ajuste de curva:", fin_ajuste_curva),
           x = fin_ajuste_curva - 3,
           y = ymax / 3.5, angle = 90,
           size = 4) +

  scale_y_continuous(labels = scales::comma,
                     breaks = scales::breaks_extended(n=7),
                     limits = c(0, ymax)) +
  # ylim(c(0, ymax)) +
   
  ylab("Número de casos acumulados") +
  xlab("Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "none",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "aplanamiento.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "aplanamiento@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)







# ### Viejo SEIR
# 
# Tab <- Tab %>%
#   select(fecha, casos_nuevos = sintomas_nuevos,
#          casos_acumulados = sintomas_acumulados) %>%
#   arrange(fecha) %>%
#   mutate(casos_nuevos = floor(zoo::rollmean(casos_nuevos, k = 7, fill = NA, align = "center"))) %>%
#   # print(n = 200)
#   filter(casos_nuevos > 0) %>%
#   # print(n = 30)
#   mutate(dia = as.numeric(fecha - min(fecha)))
# 
# # Precalcular dias
# fecha_inicio <- min(Tab$fecha)
# fecha_final <- Sys.Date()
# n_dias <- as.numeric(fecha_final - fecha_inicio)
# n_dias_ajuste <- n_dias - args$dias_retraso + 1
# # fechas <- c("2020-03-02", "2020-03-09", "2020-03-16") %>% parse_date(format = "%Y-%m-%d")
# # fechas <- c("2020-03-09", "2020-03-16", "2020-03-23") %>% parse_date(format = "%Y-%m-%d")
# # fechas <- c("2020-03-02", "2020-03-09","2020-03-16", "2020-03-23", "2020-03-30") %>% parse_date(format = "%Y-%m-%d")
# # fechas <- c("2020-03-01","2020-03-08", "2020-03-15", "2020-03-22", "2020-03-29", "2020-04-05") %>%
# #   parse_date(format = "%Y-%m-%d")
# # fechas <- c("2020-03-01", "2020-03-15", "2020-04-15", "2020-05-10") %>%
# #   parse_date(format = "%Y-%m-%d")
# # fechas_dias <- as.numeric(fechas - fecha_inicio)
# fechas_dias <- sort(n_dias_ajuste - seq(from = 10, by = 15, length.out = 6))
# # fechas_dias <- seq(from = 15, by = 15, length.out = 6)
# # fechas_dias
# 
# # Parameters to make optimization
# # pob <- 135552447
# T_inc <- c(4, 5, 6)
# # T_inf <- c(2, 3, 4)
# T_inf <- c(5, 6, 7)
# # T_inc <- c(4, 5)
# # T_inf <- c(2)
# pob <- 127792286
# # T_inc <- 4
# # T_inf <- 4
# 
# R_hat <- encontrar_R_0(real = Tab, n_dias_ajuste = n_dias_ajuste,
#                        dias_int = fechas_dias,
#                        T_inc = T_inc, T_inf = T_inf, pob = pob)
# # R_ha1 <- R_hat
# R_hat
# # save(R_hat, file = "smooth.rdat")
# # save(R_hat, file = "r_hat_sir_7x15_lbfgsb.rdat")
# # save(R_hat, file = "r_hat_sir_7x15_sann.rdat")
# save(R_hat, file = "R_hat_coronamex.rdat")
# # load("R_hat_coronamex.rdat")
# 
# # Simular con parámetros estimados
# sims <- simular_multiples_modelos(modelos = R_hat,
#                                   FUN = sir, real = Tab, pob = pob,
#                                   n_dias = n_dias)
# ctds <- read_csv(args$reportes_diarios,
#                  col_types = cols(fecha = col_date(format = "%Y-%m-%d"))) %>%
#   select(fecha, casos_acumulados) %>%
#   mutate(dia = as.numeric(fecha - fecha_inicio),
#          modelo = "Confirmado")
# # ctds
# sims <- sims %>%
#   split(.$modelo) %>%
#   map_dfr(function(d){
#     # d <- sims %>% filter(modelo == "m1")
#     
#     max_diff <- Tab %>%
#       select(dia, casos_acumulados) %>%
#       mutate(modelo = "real") %>%
#       filter(dia < n_dias_ajuste) %>%
#       left_join(d, by = "dia") %>%
#       mutate(diff = abs(casos_acumulados.x - casos_acumulados.y)) %>%
#       # arrange(desc(diff))
#       select(diff) %>%
#       max
#     
#     d
#     # if(max_diff < 500)
#     #   return(d)
#   })
# # sims %>%
# #   filter(dia >= 50) %>%
# #   print(n = 300)
# 
# p1 <- Tab %>%
#   select(fecha, dia, casos_acumulados) %>%
#   mutate(modelo = "real") %>%
#   bind_rows(sims %>% mutate(fecha = fecha_inicio + dia)) %>%
#   bind_rows(ctds) %>%
#   mutate(grupo = "Estimado (SEIR)") %>%
#   mutate(grupo = replace(grupo, modelo == "real", "Inicio de síntomas")) %>%
#   mutate(grupo = replace(grupo, modelo == "Confirmado", "Confirmado")) %>%
#   mutate(grupo = factor(grupo, levels = c("Confirmado", "Inicio de síntomas", "Estimado (SEIR)"))) %>%
#   mutate(modelo = factor(modelo, levels = c("Confirmado", "real", unique(sims$modelo)))) %>%
#   
#   # filter(fecha >= "2020-02-15") %>%
#   # filter(!(modelo %in% para_quitar)) %>%
# 
#   ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
#   geom_line(aes(col = grupo, size = grupo)) +
#   geom_vline(xintercept = Sys.Date() - args$dias_retraso) +
#   annotate("text", label = "Fin ajuste de curva",
#            x = Sys.Date() - args$dias_retraso - 3,
#            y = 50000, angle = 90,
#            size = 6) +
#   geom_vline(xintercept = fecha_inicio + fechas_dias, col = "red") +
#   scale_color_manual(values = c("#1b9e77", "#7570b3", "#d95f02"),
#                      name = "") +
#   scale_size_manual(values = c(2, 2, 0.2)) +
#   guides(size = FALSE) +
#   ylab("Casos acumulados") +
#   xlab("Fecha") +
#   scale_y_continuous(labels = scales::comma, breaks = function(lims){seq(from = 0, to = lims[2], by = 2.5 * 1e4)}) +
#   # scale_y_log10() +
#   guides(color = guide_legend(override.aes = list(size = 3))) +
#   AMOR::theme_blackbox() +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill = NA, color = "black", size = 3),
#         legend.position = "top",
#         legend.text = element_text(size = 14),
#         legend.key = element_blank(),
#         axis.title = element_text(size = 20),
#         axis.text = element_text(size = 10, color = "black"),
#         plot.margin = margin(l = 20, r = 20))
# # p1
# # ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
# archivo <- file.path(args$dir_salida, "sir_nacional.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "sir_nacional@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
# archivo <- file.path(args$dir_estimados, "seir_estimados.csv")
# write_csv(p1$data %>%
#             mutate(fecha_estimacion = Sys.Date()), archivo)
# 
# max_estimado_actual <- max(p1$data$casos_acumulados, na.rm = TRUE)
# # Aplanamiento
# 
# sims_parciales <- R_hat %>%
#   map_dfr(function(l){
#     if(!is.na(l$R_0)){
#       m_nombre <- l$modelo
#       n_tiempos <- length(l$tiempos_int)
# 
# 
#       Ms <- list()
#       for(i in c(2, 3, 6)){
#         m_parcial <- l
#         m_parcial$tiempos_int <- m_parcial$tiempos_int[1:i]
#         m_parcial$efectos_int <- m_parcial$efectos_int[1:i]
#         m_parcial$modelo <- paste(m_parcial$modelo, i, sep = ".")
#         Ms[[length(Ms) + 1]] <- m_parcial
#       }
#       simular_multiples_modelos(modelos = Ms,
#                                 FUN = sir, real = Tab, pob = pob,
#                                 n_dias = n_dias)
# 
#     }
#   })
# 
# 
# p1 <- Tab %>%
#   select(fecha, dia, casos_acumulados) %>%
#   mutate(modelo = "real") %>%
#   bind_rows(sims_parciales %>% mutate(fecha = fecha_inicio + dia)) %>%
#   # select(modelo) %>% table
#   # filter(str_detect(modelo, "[.]7$"))
#   mutate(grupo = "¿Qué pudo haber pasado?") %>%
#   mutate(grupo =  replace(grupo, modelo %>% str_detect(pattern = "[.]6$"), "¿Qué creemos que está pasando?")) %>%
#   mutate(grupo = replace(grupo,
#                          modelo %>% str_detect(pattern = "[.]2$"),
#                          paste("¿Qué pudo haber pasado?\n(hace", n_dias - fechas_dias[2], "días)"))) %>%
#   mutate(grupo = replace(grupo,
#                          modelo %>% str_detect(pattern = "[.]3$"),
#                          paste("¿Qué pudo haber pasado?\n(hace", n_dias - fechas_dias[3], "días)"))) %>%
#   mutate(grupo = replace(grupo, modelo == "real", "Inicio de síntomas")) %>%
#   mutate(grupo = factor(grupo, levels = c("Inicio de síntomas",
#                                           paste("¿Qué pudo haber pasado?\n(hace", n_dias - fechas_dias[2], "días)"),
#                                           paste("¿Qué pudo haber pasado?\n(hace", n_dias - fechas_dias[3], "días)"),
#                                           "¿Qué creemos que está pasando?"))) %>%
#   mutate(modelo = factor(modelo, levels = c("real", unique(sims_parciales$modelo)))) %>%
#   # filter(str_detect(modelo, "[.]7$")) %>% print(n = 1000)
# 
#   filter(casos_acumulados <= max_estimado_actual) %>%
#   
#   # arrange(casos_acumulados)
#   # filter(fecha >= "2020-02-15") %>%
#   # filter(modelo == "real") %>%
#   # filter(dia < 75) %>%
#   # select(modelo) %>% table
#   # select(grupo) %>% table
#   # filter(modelo == "mediana.3") %>% print(n = 100)
#   # filter(modelo %in% c("real", "mediana.2")) %>%
#   # print(n = 100)
# 
#   ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
#   geom_line(aes(col = grupo, size = grupo, linetype = grupo)) +
#   geom_vline(xintercept = Sys.Date() - args$dias_retraso) +
#   annotate("text", label = "Fin ajuste de curva",
#            x = Sys.Date() - args$dias_retraso - 3,
#            y = 50000, angle = 90,
#            size = 6) +
#   # geom_vline(xintercept = fecha_inicio + fechas_dias, col = "red") +
#   scale_color_manual(values = c("#7570b3", "#b35806",
#                                 "#fdb863",
#                                 "black"),
#                      name = "") +
#   scale_size_manual(values = c(2, 0.5, 0.5, 0.3)) +
#   scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
#   guides(size = FALSE) +
#   ylab("Casos acumulados") +
#   xlab("Fecha") +
#   scale_y_continuous(labels = scales::comma, breaks = function(lims){seq(from = 0, to = lims[2], by = 2.5 * 1e4)}) +
#   # scale_y_log10() +
#   guides(color = guide_legend(override.aes = list(size = 3,
#                                                   linetype = c("solid", "dashed", "dashed", "solid")),
#                               nrow = 2),
#          linetype = FALSE) +
#   AMOR::theme_blackbox() +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill = NA, color = "black", size = 3),
#         legend.position = "top",
#         legend.text = element_text(size = 12),
#         legend.key = element_blank(),
#         axis.title = element_text(size = 20),
#         axis.text = element_text(size = 10, color = "black"),
#         plot.margin = margin(l = 20, r = 20))
# # p1
# # ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
# archivo <- file.path(args$dir_salida, "aplanamiento.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "aplanamiento@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
# 
# ##### R_hat
# 
# # p1 <- R_hat %>%
# #   map_dfr(~ tibble(R0 = c(.x$R_0, .x$R_0 * .x$efectos_int),
# #                dias = c(0, fechas_dias),
# #                modelo = .x$modelo)) %>%
# #   mutate(fecha = fecha_inicio + dias) %>%
# #   
# #   ggplot(aes(x = fecha, y = R0, group = modelo)) +
# #   geom_line() +
# #   ylab("Promedio de infectados por enfermo (R_t)") +
# #   xlab("Fecha") +
# #   AMOR::theme_blackbox() +
# #   theme(panel.background = element_blank(),
# #         panel.border = element_rect(fill = NA, color = "black", size = 3),
# #         legend.position = "top",
# #         legend.text = element_text(size = 14),
# #         legend.key = element_blank(),
# #         axis.title = element_text(size = 20),
# #         axis.text = element_text(size = 10, color = "black"),
# #         plot.margin = margin(l = 20, r = 20))
# # p1
# # ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
# # archivo <- file.path(args$dir_salida, "sir_nacional_r0.png")
# # ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# # archivo <- file.path(args$dir_salida, "sir_nacional_r0@2x.png")
# # ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
