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

args <- list(Serie_confirmados = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv.gz",
             serie_deteccion = "../datos/datos_abiertos/serie_tiempo_nacional_fecha_confirmacion.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Calcular tiempos de detección...\n")

Dat_pos <- read_csv(args$Serie_confirmados,
                    col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                     .default = col_number()))
# Dat_pos 
Dat_conf <- read_csv(args$serie_deteccion,
                     col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                      .default = col_number()))
# Dat_conf

# Unir datos
Dat <- Dat_pos %>%
  select(fecha, sintomas_acumulados, ingreso_acumulados) %>%
  full_join(Dat_conf %>%
              select(fecha, casos_acumulados),
            by = "fecha") %>%
  mutate(casos_acumulados = replace(casos_acumulados, is.na(casos_acumulados), 0))
Dat <- tibble(fecha = min(Dat$fecha) - 1,
       sintomas_acumulados = 0,
       ingreso_acumulados = 0,
       casos_acumulados = 0) %>%
  bind_rows(Dat)
# Dat

Res <- tibble(dias = 0:as.numeric(max(Dat$fecha) - min(Dat$fecha)),
              sintomas_ingreso = 0,
              ingreso_confirmacion = 0,
              tiempo_deteccion = 0)
# Res
for(i in 2:nrow(Dat)){
  ingreso_bandera <- FALSE
  conf_bandera <- FALSE
  det_bandera <- FALSE
  for(j in i:nrow(Dat)){
    
    dias <- j - i
    diff_ingreso <- Dat$ingreso_acumulados[j] - Dat$sintomas_acumulados[i]
    diff_conf <- Dat$casos_acumulados[j] - Dat$ingreso_acumulados[i]
    diff_det <- Dat$casos_acumulados[j] - Dat$sintomas_acumulados[i]
    
    if(diff_ingreso >= 0 && ingreso_bandera == FALSE){
      nuevos <- Dat$sintomas_acumulados[i] - Dat$sintomas_acumulados[i-1]
      Res$sintomas_ingreso[dias + 1] <-  Res$sintomas_ingreso[dias + 1] + nuevos
      ingreso_bandera <- TRUE
    }
    if(diff_conf >= 0 && conf_bandera == FALSE){
      nuevos <- Dat$ingreso_acumulados[i] - Dat$ingreso_acumulados[i-1]
      Res$ingreso_confirmacion[dias + 1] <-  Res$ingreso_confirmacion[dias + 1] + nuevos
      conf_bandera <- TRUE
    }
    if(diff_det >= 0 && det_bandera == FALSE){
      nuevos <- Dat$sintomas_acumulados[i] - Dat$sintomas_acumulados[i-1]
      Res$tiempo_deteccion[dias + 1] <-  Res$tiempo_deteccion[dias + 1] + nuevos
      det_bandera <- TRUE
    }
    
    if(ingreso_bandera == TRUE && conf_bandera && det_bandera){
      break()
    }
  }
}
# Res %>% print(n = 100)
# Dat %>% print(n = 100)

# sum(Res$sintomas_ingreso)
# sum(Res$ingreso_confirmacion)
# sum(Res$tiempo_deteccion)

p1 <- Dat %>%
  pivot_longer(-fecha, names_to = "tipo", values_to = "casos_acumulados") %>%
  filter(fecha >= "2020-03-01") %>%
  mutate(tipo = factor(tipo, levels = c("sintomas_acumulados", "ingreso_acumulados", "casos_acumulados"))) %>%
  
  ggplot(aes(x = fecha, y = casos_acumulados, group = tipo)) +
  geom_line(aes(col = tipo), size = 2) +
  
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"),
                     name = "",
                     labels = c("Inicio de síntomas", "Ingreso a unidad médica", "Confirmados")) +
  scale_y_continuous(labels = scales::comma) +
  xlab("Fecha") +
  ylab("Casos totales") +
  guides(color = guide_legend(override.aes = list(size = 3), nrow = 2)) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))

p2 <- Res %>%
  mutate(sintomas_ingreso = sintomas_ingreso / sum(sintomas_ingreso),
         ingreso_confirmacion = ingreso_confirmacion / sum(ingreso_confirmacion),
         tiempo_deteccion = tiempo_deteccion / sum(tiempo_deteccion)) %>%
  pivot_longer(-dias, names_to = "tipo", values_to = "probabilidad") %>%
  mutate(tipo = factor(tipo, levels = c("sintomas_ingreso",
                                        "ingreso_confirmacion",
                                        "tiempo_deteccion"))) %>%
  
  filter(probabilidad > 0) %>%
  filter(dias <= 20) %>%
  
  ggplot(aes(x = dias, y = probabilidad, group = tipo)) +
  geom_line(aes(col = tipo), size = 2) +
  scale_color_manual(values = c("#7570b3", "#d95f02", "#1b9e77"),
                     name = "",
                     labels =   c("Síntomas a unidad médica",
                                  "Unidad médica a confirmación",
                                  "Tiempo de detección")) +
  
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = function(lims){ seq(from = 0, to = lims[2], by = 2) }) +
  # scale_y_log10(labels = scales::percent) +
  xlab("# de días de retraso") +
  ylab("% confirmados") +
  guides(color = guide_legend(override.aes = list(size = 3), nrow = 2)) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
# p2

pp <- cowplot::plot_grid(p2, p1, nrow = 2)
# pp
# ggsave("test.png", pp, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "tiempo_deteccion.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "tiempo_deteccion@2x.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)
