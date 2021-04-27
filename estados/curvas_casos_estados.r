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
rolling_mean <- tibbletime::rollify(mean, window = 7, na_value = 0)

graficar_entidades <- function(Dat, entidades, fecha_inicio,
                               fecha_final, offset_totales = 20){
  # entidades <- unique(Dat$estado)[25:32]
  # entidades <- unique(Dat$estado)[1:8]
  # fecha_final <- max(Dat$fecha)
  # fecha_inicio <- parse_date("2020-03-01", format = "%Y-%m-%d")
  # offset_totales <- 45
  
  Dat <- Dat %>%
    filter(estado %in% entidades) %>%
    split(.$estado) %>%
    map_dfr(function(d){
      d %>%
        mutate(casos = rolling_mean(sintomas_nuevos),
               muertes = rolling_mean(muertes_nuevas))
    }) 
  
  
  # Dat %>%
  #   select(estado, fecha, sintomas_nuevos, casos) %>%
  #   print(n = 200)
  
  hoy <- fecha_final
  ent_tots <- Dat %>%
    group_by(estado) %>%
    summarise(casos_totales = sum(sintomas_nuevos),
              max_casos = max(sintomas_nuevos),
              casos_previos = sum(sintomas_nuevos[fecha < hoy - 14 & fecha >= hoy - 28]),
              casos_previos2 = sum(sintomas_nuevos[fecha < hoy - 21 & fecha >= hoy - 35]),
              .groups = "drop") %>%
    # ungroup() %>%
    mutate(cambio = casos_previos / casos_previos2)
  ent_tots <- ent_tots %>%
    mutate(cambio = cambio > 1) %>%
    mutate(cambio = replace(cambio, cambio, "q En aumento")) %>%
    mutate(cambio = replace(cambio, cambio == "FALSE", "Sin aumento"))
  
  cambio_etiquetas <- ent_tots$cambio %>% unique %>% sort
  cambio_cols <- set_names(c("#fc8d62", "#66c2a5"),
                           nm = c("q En aumento", "Sin aumento"))[cambio_etiquetas] %>%
    as.character()
  cambio_etiquetas[ str_detect(cambio_etiquetas, "q En aumento") ] <- "En aumento"
  
  p1 <- Dat %>%
    left_join(ent_tots %>%
                select(estado, cambio),
              by = "estado") %>%
    filter(fecha >= fecha_inicio) %>%
    select(fecha, casos, sintomas_nuevos, cambio, estado) %>%
    
    ggplot(aes(x = fecha)) +
    geom_rect(aes(xmin = fecha_final - 15,
                  xmax = fecha_final,
                  ymin = -Inf, ymax = Inf,
                  fill = "pink")) +
    geom_bar(aes(y = sintomas_nuevos,
                 col = cambio,
                 fill = cambio),
             width = 1,
             stat = "identity") +
    geom_vline(xintercept = fecha_final - 15) +
    scale_fill_manual(values = c("pink", cambio_cols),
                      name = "",
                      labels = c("Casos en estas fechas pueden aumentar", cambio_etiquetas)) +
    scale_color_manual(values = cambio_cols) +
    geom_line(data = Dat %>%
                filter(fecha <= fecha_final - 15) %>%
                filter(fecha >= fecha_inicio),
              aes(y = casos),
              size = 2, col = "black") +
    geom_text(data = ent_tots, aes(label = paste(casos_totales, "casos"),
                                   y = 0.9 * max_casos), x = fecha_inicio + offset_totales) +
    guides(color = FALSE) +
    ylab(label = "Número de nuevos casos") +
    AMOR::theme_blackbox() +
    theme(axis.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90),
          plot.margin = margin(l = 20, r = 20),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.background = element_blank())
  
  if(length(entidades) > 1){
    p1 <- p1 + facet_wrap(~ estado, scales = "free_y")
  }
  
  p1
}


args <- list(serie_estados = "../datos/datos_abiertos/serie_tiempo_estados_um_confirmados.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Curvas por estados...\n")

Dat <- read_csv(args$serie_estados,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 estado = col_character(),
                                 .default = col_number()))
fecha_final <- max(Dat$fecha)
fecha_inicio <- parse_date("2020-03-01", format = "%Y-%m-%d")

#
p1 <- graficar_entidades(Dat = Dat, entidades = sort(unique(Dat$estado))[1:8],
                         fecha_inicio = fecha_inicio,
                         fecha_final = fecha_final,
                         offset_totales = 150)
# p1
archivo <- file.path(args$dir_salida, "estados_casos1.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_casos1@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_entidades(Dat = Dat, entidades = sort(unique(Dat$estado))[9:16],
                         fecha_inicio = fecha_inicio,
                         fecha_final = fecha_final,
                         offset_totales = 150)
# p1
archivo <- file.path(args$dir_salida, "estados_casos2.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_casos2@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_entidades(Dat = Dat, entidades = sort(unique(Dat$estado))[17:24],
                         fecha_inicio = fecha_inicio,
                         fecha_final = fecha_final,
                         offset_totales = 150)
# p1
archivo <- file.path(args$dir_salida, "estados_casos3.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_casos3@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_entidades(Dat = Dat, entidades = sort(unique(Dat$estado))[25:32],
                         fecha_inicio = fecha_inicio,
                         fecha_final = fecha_final,
                         offset_totales = 150)
# p1
archivo <- file.path(args$dir_salida, "estados_casos4.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_casos4@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

