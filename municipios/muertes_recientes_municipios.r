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

args <- list(serie_tiempo = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv.gz",
             n_dias = 10,
             n_municipios = 20,
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Muertes recientes en municipios...\n")

Dat <- read_csv(args$serie_tiempo,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 clave = col_character(),
                                 municipio = col_character(),
                                 .default = col_number()))
stop_for_problems(Dat)

# Números recientes
Dat <- Dat %>%
  filter(fecha > max(fecha) - args$n_dias)

muertes_recientes <- Dat %>%
  group_by(clave) %>%
  summarise(muertes_recientes = sum(muertes_nuevas),
            .groups = "drop") %>%
  arrange(desc(muertes_recientes)) %>%
  head(args$n_municipios) %>%
  # print(n = 100) %>%
  select(clave) %>%
  unlist
# muertes_recientes


dat <- Dat %>%
  filter(clave %in% muertes_recientes) %>%
  select(fecha, muertes_nuevas, clave, municipio) %>%
  filter(muertes_nuevas > 0)
  
p1 <- dat %>%
  ggplot(aes(x = fecha, y = clave)) +
  geom_point(aes(size = muertes_nuevas)) +
  scale_size_continuous(name = "Muertes") +
  scale_y_discrete(labels = unique(dat$municipio)) +
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
archivo <- file.path(args$dir_salida, "top_municipios_muertes.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_municipios_muertes@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
