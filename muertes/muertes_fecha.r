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
library(cowplot)

args <- list(serie = "../datos/datos_abiertos/serie_tiempo_estados_um_confirmados.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Muertes por fecha y región...\n")

Dat <- read_csv(args$serie,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 estado = col_character(),
                                 .default = col_number()))

Dat <- Dat %>%
  select(fecha, muertes = muertes_nuevas, estado) %>%
  mutate(region = estado) %>%
  mutate(region = replace(region, region %in% c("Baja California",
                                                "Baja California Sur",
                                                "Chihuahua",
                                                "Sinaloa",
                                                "Durango",
                                                "Sonora"),
                          "Noroeste")) %>%
  mutate(region = replace(region, region %in% c("Campeche",
                                                "Quintana Roo",
                                                "Yucatán",
                                                "Tabasco"),
                          "Sureste")) %>%
  mutate(region = replace(region,
                          region %in% c("Colima",
                                        "Jalisco",
                                        "Michoacán",
                                        "Nayarit"),
                          "Occidente")) %>%
  mutate(region = replace(region,
                          region %in% c("Hidalgo",
                                        "Puebla",
                                        "Tlaxcala",
                                        "Veracruz"),
                          "Oriente")) %>%
  mutate(region = replace(region,
                          region %in% c("Coahuila",
                                        "Nuevo León",
                                        "Tamaulipas"),
                          "Noreste")) %>%
  mutate(region = replace(region,
                          region %in% c("Aguascalientes",
                                        "Guanajuato",
                                        "Querétaro",
                                        "San Luis Potosí",
                                        "Zacatecas"),
                          "Centronorte")) %>%
  mutate(region = replace(region,
                          region %in% c("Ciudad de México",
                                        "Morelos",
                                        "México"),
                          "Centrosur")) %>%
  mutate(region = replace(region,
                          region %in% c("Chiapas",
                                        "Oaxaca",
                                        "Guerrero"),
                          "Suroeste")) %>%
  group_by(fecha, region) %>%
  summarise(muertes = sum(muertes),
            .groups = "drop") %>%
  filter(fecha >= "2020-03-18")
# Dat


p1 <- Dat %>%
  split(.$fecha) %>%
  map_dfr(function(d){
    tot_def <- sum(d$muertes)
    
    d$ymax <- cumsum(d$muertes) - (tot_def / 2)
    d$xmin <- d$fecha - 1
    
    d %>%
      mutate(ymin = lag(ymax, n = 1, default = - (tot_def / 2))) %>%
      filter(muertes > 0)
  }) %>%
  ggplot(aes(fill = region)) +
  geom_rect(aes(xmin = max(fecha) - 15, xmax = max(fecha),
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  geom_rect(aes(xmin = xmin, xmax = fecha,
                ymin = ymin, ymax = ymax), col = NA) +
  scale_fill_brewer(type = "qual", name = "") +
  guides(fill = guide_legend(nrow = 2)) +
  
  geom_vline(xintercept = max(Dat$fecha) - 15, col = "black") +
  scale_y_continuous(breaks = function(lims){
    seq(from = lims[1], to = lims[2], by = 100)
    },
    labels = function(labs){
      labs <- as.numeric(labs)
      labs + abs(min(labs))
    }) +
  ylab(label = "Fallecimientos") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  AMOR::theme_blackbox() +
  theme(legend.position = "none",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20, b = 10))
# p1

p2 <- Dat %>%
  filter(muertes > 0) %>%
  ggplot(aes(x = fecha, y = muertes)) +
  geom_bar(aes(fill = region),
           position = "fill", stat = "identity", width = 1) +
  scale_fill_brewer(type = "qual", name = "") +
  guides(fill = guide_legend(nrow = 2)) +
  
  geom_vline(xintercept = max(Dat$fecha) - 15, col = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab(label = "% de fallecimientos") +
  xlab(label = "Fecha de defunción") +
  AMOR::theme_blackbox() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90),
        axis.text.y = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))

pp <- cowplot::plot_grid(p1,p2, nrow = 2)
archivo <- file.path(args$dir_salida, "muertes_region.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_region@2x.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)
