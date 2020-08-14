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
cat("Modelos seir...\n")

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
fin_ajuste_curva <- max(Est$fecha_estimacion) - 15
# fin_ajuste_curva

#### Preparar datos para SEIR casos nuevos
mu_est <- Est %>%
  filter(fecha <= fin_ajuste_curva) %>%
  select(fecha, nuevos_mu_10, nuevos_mu_50, nuevos_mu_90)
obs_est <- Est %>%
  filter(fecha > fin_ajuste_curva) %>%
  select(fecha, nuevos_obs_10, nuevos_obs_50, nuevos_obs_90)

# final_datos
est_final <- obs_est %>% filter(fecha == final_datos) %>%
  select(nuevos_obs_10, nuevos_obs_90)
ymax <- max(obs_est$nuevos_obs_90)
# Tab
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
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "aplanamiento.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "aplanamiento@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

#### Graficar modelo centinela
# Modelo CoronaMex
archivo <- file.path(args$dir_estimados, "bayes_seir_centinela_coronamex.csv")
Est <- read_csv(archivo,
                col_types = cols(fecha_estimacion = col_date(format = "%Y-%m-%d"),
                                 fecha = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))
Est <- Est %>%
  filter(fecha >= args$fecha_inicio) %>%
  filter(fecha <= final_datos + args$dias_extra)
fin_ajuste_curva <- max(Est$fecha_estimacion) - 16

mu_est <- Est %>%
  filter(fecha <= fin_ajuste_curva) %>%
  select(fecha, acum_mu_10, acum_mu_50, acum_mu_90)
obs_est <- Est %>%
  filter(fecha > fin_ajuste_curva) %>%
  select(fecha, acum_obs_10, acum_obs_50, acum_obs_90)
ymax <- max(obs_est$acum_obs_90)

# Centinela oficial
archivo <- file.path(args$dir_estimados, "bayes_seir_centinela_oficial.csv")
Est_pre1 <- read_csv(archivo,
                     col_types = cols(fecha_estimacion = col_date(format = "%Y-%m-%d"),
                                      fecha = col_date(format = "%Y-%m-%d"),
                                      .default = col_number())) %>%
  filter(fecha >= args$fecha_inicio)
mu_est_pre1 <- Est_pre1 %>%
  filter(fecha <= "2020-04-18") %>%
  select(fecha, acum_mu_10, acum_mu_50, acum_mu_90)
obs_est_pre1 <- Est_pre1 %>%
  filter(fecha > "2020-04-18") %>%
  select(fecha, acum_obs_10, acum_obs_50, acum_obs_90)

p1 <- bind_rows(mu_est %>%
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
                         q_90 = acum_obs_90)) %>%
  mutate(modelo = factor(modelo, levels = c("final_mu", "final_obs",
                                            "pre1_mu", "pre1_obs"))) %>%
  
  ggplot(aes(x = fecha, group = modelo, col = modelo)) +
  
  geom_line(aes(y = q_50)) +
  geom_ribbon(aes(ymin = q_10, ymax = q_90, fill = modelo), alpha = 0.2) +
  
  scale_color_manual(values = c("#a6cee3", "#1f78b4",
                                "#b2df8a", "#33a02c"),
                     guide = FALSE) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4",
                               "#b2df8a", "#33a02c"),
                    labels = c("Centinela\n(CoronaMex)", "CoronaMex\n+\nSEIR",
                               "Centinela\n(SSA)", "SSA\n+\nSEIR"),
                    name = "",
                    guide = guide_legend(override.aes = list(alpha = 1))) +
  
  
  geom_vline(xintercept = fin_ajuste_curva + 0.5) +
  annotate("text", label = paste("Fin ajuste de curva:", fin_ajuste_curva),
           x = fin_ajuste_curva - 3,
           y = ymax / 3.5, angle = 90,
           size = 4) +
  
  scale_y_continuous(labels = scales::comma,
                     breaks = scales::breaks_extended(n=7),
                     limits = c(0, ymax)) +
  
  ylab("Casos acumulados estimados") +
  xlab("Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
# p1
archivo <- file.path(args$dir_salida, "sir_nacional_centinela.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional_centinela@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
