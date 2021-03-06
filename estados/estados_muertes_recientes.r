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

args <- list(serie_tiempo = "../datos/datos_abiertos/serie_tiempo_estados_um_confirmados.csv.gz",
             n_dias = 10,
             n_municipios = 100,
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Fallecimiento recientes por estado...\n")

Dat <- read_csv(args$serie_tiempo,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 estado = col_character(),
                                 .default = col_number()))
stop_for_problems(Dat)

# Números recientes
Dat <- Dat %>%
  filter(fecha > max(fecha) - args$n_dias)

dat <- Dat %>%
  select(fecha, muertes_nuevas, estado) %>%
  # filter(muertes_nuevas > 0) %>%
  mutate(estado = factor(estado, levels = sort(unique(Dat$estado), decreasing = TRUE))) 

p1 <- dat %>%
  ggplot(aes(x = fecha, y = estado)) +
  geom_point(aes(size = muertes_nuevas),
             shape = 21, fill = "black", col = "white") +
  # scale_size_continuous(name = "Muertes") +
  scale_size_area(name = "Fallecimientos") +
  # scale_size(name = "Fallecimientos", range = c(-0.5,6)) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        # panel.border = element_rect(fill = NA, color = "black", size = 3),
        panel.border = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 14),
        # legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
# p1 
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_muertes_recientes.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "estados_muertes_recientes@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

