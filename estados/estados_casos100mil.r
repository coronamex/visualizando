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

library(tidyverse)

args <- list(poblacion = "../datos/demograficos/pob_estado.tsv",
             tasa_min = 0,
             dir_salida = "../sitio_hugo/static/imagenes/",
             max_dias = 10,
             max_semanas = 3,
             dias_activos = 14,
             serie_tiempo_estados = "../datos/datos_abiertos/serie_tiempo_estados_um_confirmados.csv.gz")
cat("Incidencia por estado...\n")

incidencia_cortes <- 200
mortalidad_cortes <- 2
casos_acum_cortes <- 4000
muertes_acum_cortes <- 150

# Leer poblaciones
pob <- read_tsv(args$poblacion,
                col_types = cols(estado = col_character(),
                                 .default = col_number()))
stop_for_problems(pob)

Dat <- read_csv(args$serie_tiempo_estados,
                col_types = cols(estado = col_character(),
                                 fecha = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))
stop_for_problems(Dat)

dat <- Dat %>%
  select(fecha, sintomas_nuevos, muertes_nuevas, estado) %>% 
  mutate(periodo_id = floor(as.numeric((max(fecha) - args$dias_activos - fecha) / 7))) %>%
  # filter(estado == "Aguascalientes") %>% print(n = 300)
  mutate(periodo_id = replace(periodo_id, periodo_id < 0, -1)) %>%
  filter(periodo_id < args$max_semanas) %>%
  # print(n = 500)
  group_by(estado, periodo_id) %>%
  summarise(casos = sum(sintomas_nuevos),
            muertes = sum(muertes_nuevas),
            fecha_inicio = min(fecha),
            fecha_final = max(fecha),
            .groups = 'drop') %>%
  left_join(pob %>%
              select(estado, pob = conapo_2020),
            by = "estado") %>%
  mutate(incidencia = casos / (pob / 1e5),
         mortalidad = muertes / (pob / 1e5)) %>%
  select(-casos, -muertes, -pob)  %>%
  # ggplot(aes(x = incidencia, y = mortalidad)) + geom_point()
  mutate(periodo_nombre = paste0(lubridate::day(fecha_inicio), " ",
                                 lubridate::month(fecha_inicio,
                                                  label = TRUE, abbr = TRUE),
                                 "-",
                                 lubridate::day(fecha_final), " ",
                                 lubridate::month(fecha_final,
                                                  label = TRUE, abbr = TRUE))) %>%
  select(-fecha_inicio, -fecha_final) %>%
  mutate(periodo_nombre = factor(periodo_nombre,
                                 levels = unique(periodo_nombre[ order(periodo_id, decreasing = TRUE) ]))) %>%
  mutate(estado = factor(estado, levels = rev(unique(estado))))


# pal <- colorRampPalette(colors = rev(c("#a50026",
#                                        "#d73027",
#                                        "#f46d43",
#                                        "#fdae61",
#                                        "#fee090",
#                                        "#ffffbf",
#                                        "#e0f3f8",
#                                        "#abd9e9",
#                                        "#74add1",
#                                        "#4575b4",
#                                        "#313695")))

axis.text.size.x <- 10
axis.text.size.y <- 10
axis.title.size.x <- 8
legend.text.size <- 10

## Totales
# dat
pal <- colorRampPalette(colors = c("#c7eae5", "#01665e"))
pal <- colorRampPalette(colors = c("white", "#01665e"))
p1 <- dat %>%
  filter(periodo_id >= 0) %>%
  ggplot(aes(x = periodo_nombre, y = estado)) +
  geom_tile(aes(fill = incidencia), col = "black",
            width = 0.8, height = 0.8) +
  scale_fill_gradient(low = pal(11)[1],
                       high = pal(11)[11],
                       name = expression(frac("Casos", "100 mil hab."))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.x = element_text(angle = 90,
                                   size = axis.text.size.x,
                                   vjust = 0.5),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = legend.text.size, angle = 90),
        legend.title = element_text(size = legend.text.size),
        plot.margin = margin(l = 0, r = 0))
# p1

pal <- colorRampPalette(colors = c("#f6e8c3", "#8c510a"))
pal <- colorRampPalette(colors = c("white", "#8c510a"))
p2 <- dat %>%
  filter(periodo_id >= 0) %>%
  ggplot(aes(x = periodo_nombre, y = estado)) +
  geom_tile(aes(fill = mortalidad), col = "black",
            width = 0.8, height = 0.8) +
  scale_fill_gradient(low = pal(11)[1],
                      high = pal(11)[11],
                      name = expression(frac("Muertes", "100 mil hab."))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.x = element_text(angle = 90,
                                   size = axis.text.size.x,
                                   vjust = 0.5),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = legend.text.size),
        legend.title = element_text(size = legend.text.size),
        plot.margin = margin(l = 0, r = 0))
# p2

### Recientes
p3 <- dat %>%
  filter(periodo_id < 0) %>%
  ggplot(aes(x = incidencia, y = estado)) +
  geom_col(aes(fill = "pink")) +
  scale_fill_manual(name = "", values = "pink",
                    labels = "Últimos 14 días\n(puede aumentar)") +
  geom_vline(xintercept = c(20), col = "red", size = 1) +
  scale_x_continuous(breaks = function(lims){ seq(from = 0,
                                                  to = lims[2],
                                                  by = incidencia_cortes) }) +
  xlab(label = expression(frac("Casos recientes", "100 mil hab."))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.x = element_text(angle = 90, size = axis.text.size.x,
                                   vjust = 0.5),
        axis.title.x = element_text(size = axis.title.size.x),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = legend.text.size),
        plot.margin = margin(l = 0, r = 0))
# p3

p4 <- dat %>%
  filter(periodo_id < 0) %>%
  ggplot(aes(x = mortalidad, y = estado)) +
  geom_col(fill = "pink") +
  geom_vline(xintercept = c(2), col = "red", size = 1) +
  scale_x_continuous(breaks = function(lims){ seq(from = 0,
                                                  to = lims[2],
                                                  by = mortalidad_cortes) }) +
  xlab(label = expression(frac("Muertes recientes", "100 mil hab."))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_line(),
        axis.text.x = element_text(angle = 90, size = axis.text.size.x,
                                   vjust = 0.5),
        axis.title.x = element_text(size = axis.title.size.x),
        axis.ticks.x = element_blank(),
        plot.margin = margin(l = 0, r = 20))
# p4

#############
dat <- Dat %>%
  group_by(estado) %>%
  summarise(casos_acumulados = sintomas_acumulados[fecha == max(fecha)],
            muertes_acumuladas = muertes_acumuladas[fecha == max(fecha)],
            .groups = 'drop') %>%
  left_join(pob %>%
              select(estado, pob = conapo_2020),
            by = "estado") %>%
  mutate(incidencia_acumulada = casos_acumulados / (pob / 1e5),
         mortalidad_acumulada = muertes_acumuladas / (pob / 1e5)) %>%
  select(estado, incidencia_acumulada, mortalidad_acumulada) %>%
  mutate(estado = factor(estado, levels = rev(unique(estado))))
# dat

## Acumulados
p5 <- dat %>%
  ggplot(aes(x = incidencia_acumulada, y = estado)) +
  geom_col() +
  scale_x_continuous(breaks = function(lims){ seq(from = 0,
                                                  to = lims[2],
                                                  by = casos_acum_cortes) }) +
  xlab(label = expression(frac("Casos totales", "100 mil hab."))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = axis.title.size.x),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        plot.margin = margin(r = 0, l = 20))
# p5

p6 <- dat %>%
  ggplot(aes(x = mortalidad_acumulada, y = estado)) +
  geom_col() +
  scale_x_continuous(breaks = function(lims){ seq(from = 0,
                                                  to = lims[2],
                                                  by = muertes_acum_cortes) }) +
  xlab(label = expression(frac("Muertes totales", "100 mil hab."))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        
        axis.title.x = element_text(size = axis.title.size.x),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        plot.margin = margin(r = 0, l = 0))
# p6

leyenda_casos <- cowplot::get_legend(p1)
leyenda_muertes <- cowplot::get_legend(p2)
leyenda_recientes <- cowplot::get_legend(p3)
p_leyendas <- cowplot::plot_grid(leyenda_recientes,
                                 leyenda_casos,
                                 leyenda_muertes,
                              
                                 nrow = 1,
                                 rel_widths = c(0.4, 1, 0.8)) +
  theme(plot.margin = margin(l = 20, r = 20))
# p_leyendas
p_graficos <- cowplot::plot_grid(p5 + aplot::ylim2(p1),
                         p1 + theme(legend.position = "none"),
                         p3 + aplot::ylim2(p1) + theme(legend.position = "none"),
                         
                         p6 + aplot::ylim2(p1),
                         p2 + aplot::ylim2(p1) + theme(legend.position = "none"),
                         p4 + aplot::ylim2(p1),
                         axis = "bt", align = "h",
                         nrow = 1, 
                         rel_widths = c(0.22, 0.08, 0.1,
                                        0.1, 0.08, 0.1)) *
  theme(panel.background = element_blank(), plot.background = element_blank())
# p_graficos
pp <- cowplot::plot_grid(p_leyendas, p_graficos,
                         nrow = 2,
                         rel_heights = c(0.1, 0.9))
# pp
# ggsave("test.png", pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_100mil.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_100mil@2x.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)



# # Para graficar subconjunto de estados
# estados <- NULL
# # estados <- "Guanajuato"
# Dat <- Dat %>%
#   left_join(pob %>% select(estado, pob = conapo_2020), 
#             by = "estado") %>%
#   mutate(casos_100mil = sintomas_acumulados / (pob/1e5)) %>%
#   split(.$estado) %>%
#   map_dfr(function(d, tasa_min){
#     d <- d %>%
#       mutate(casos_recientes = sintomas_acumulados - lag(sintomas_acumulados, args$dias_activos, default = 0)) %>%
#       mutate(recientes = casos_recientes / (pob/1e5))
#     
#     if(max(d$sintomas_acumulados >= tasa_min) || d$estado[1] %in% estados){
#       return(d)
#     }
#   }, tasa_min = args$tasa_min) %>%
#   select(estado, acumulados = casos_100mil, recientes, fecha) %>%
#   filter(fecha > max(fecha) - args$max_dias) %>%
#   pivot_longer(cols = c(-estado, -fecha), names_to = "grupo", values_to = "casos_100mil") %>%
#   filter(!(grupo == "recientes" & fecha < max(fecha))) %>%
#   mutate(estado = factor(estado, levels = rev(unique(estado))))
# # Dat
# 
# p1 <- Dat %>%
#   filter(grupo == "acumulados") %>%
#   
#   ggplot(aes(x = fecha, y = estado)) +
#   geom_tile(aes(fill = casos_100mil), width = 0.8, height = 0.8) +
#   # facet_grid(~ grupo, scales = "free_x", space = "free_x") +
#   scale_fill_gradient2(low = pal(11)[1], mid = pal(11)[6],
#                        high = pal(11)[11], midpoint = 20,
#                        name = expression(frac("Casos totales", "100 mil habitantes"))) +
#   scale_x_date(breaks = unique(Dat$fecha), labels = function(x){strftime(x, format = "%b %d")}) +
#   AMOR::theme_blackbox() +
#   theme(panel.background = element_blank(),
#         axis.text = element_text(size = 10),
#         axis.text.x = element_text(angle = 90),
#         axis.title = element_blank(),
#         axis.ticks.x = element_blank(),
#         legend.position = "top",
#         plot.margin = margin(l = 20))
# 
# p2 <- Dat %>%
#   filter(grupo == "recientes") %>%
#   
#   ggplot(aes(x = casos_100mil, y = estado)) +
#   # geom_bar(stat = "identity") +
#   geom_col() +
#   geom_vline(xintercept = c(20, 50), col = "red", size = 1) +
#   scale_x_continuous(breaks = function(lims){ seq(from = 0,
#                                                   to = lims[2],
#                                                   by = 20) }) +
#   xlab(label = expression(frac("Casos recientes", "100 mil habitantes"))) +
#   AMOR::theme_blackbox() +
#   theme(panel.background = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = 0.5),
#         axis.title.y = element_blank(),
#         axis.ticks.x = element_blank(),
#         legend.position = "top",
#         plot.margin = margin(r = 20))
# # p2
# 
# pp <- cowplot::plot_grid(p1, p2 + aplot::ylim2(p1), axis = "bt", align = "h", rel_widths = c(0.8, 0.2))
# # ggsave("test.png", pp, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "casos_100mil_estados.png")
# ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "casos_100mil_estados@2x.png")
# ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)
