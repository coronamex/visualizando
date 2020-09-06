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
args <- list(serie_tiempo = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv.gz",
             dias_retraso = 9,
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Letalidad en México...\n")

Dat <- read_csv(args$serie_tiempo,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))
# Dat
# Dat %>%
#   filter(muertes_acumuladas > 0)

p1 <- Dat %>%
  mutate(letalidad_retr = muertes_acumuladas / lag(sintomas_acumulados, args$dias_retraso, default = 0)) %>%
  mutate(letalidad_inst = muertes_acumuladas / sintomas_acumulados) %>%
  filter(muertes_acumuladas > 0) %>%
  select(fecha, letalidad_inst, letalidad_retr) %>%
  pivot_longer(-fecha, names_to = "estimado", values_to = "letalidad") %>%
  
  filter(fecha >= "2020-03-15") %>%
  # filter(letalidad < 0.15) %>%
  # arrange(desc(letalidad)) %>%
  # print(n = 100)
  
  ggplot(aes(x = fecha, y = letalidad, group = estimado)) +
  geom_line(aes(col = estimado), size = 3) +
  scale_color_manual(values = c("#00BCF4","#7CAE00"),
                     labels = c("Letalidad\ninstantánea", "Letalidad\ncon retraso")) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Letalidad") +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "letalidad_mexico.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "letalidad_mexico@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
