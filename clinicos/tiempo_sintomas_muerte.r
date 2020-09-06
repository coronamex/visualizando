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
source("util/leer_datos_abiertos.r")

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Tiempo entre síntomas y defunción...\n")

# Lee base de datos
Dat <- leer_datos_abiertos(archivo = args$base_de_datos, solo_confirmados = TRUE, solo_fallecidos = TRUE)
dat <- Dat %>%
  select(FECHA_DEF, FECHA_SINTOMAS) %>%
  mutate(numero_dias = as.numeric(FECHA_DEF - FECHA_SINTOMAS))

p1 <- dat %>%
  filter(numero_dias <= 50) %>%
  ggplot(aes(x = numero_dias)) +
  # facet_wrap(~ RENAL_CRONICA, ncol = 1) + 
  # facet_grid(ASMA ~ ., scales = "free_y") +
  geom_histogram(bins = 15) +
  geom_vline(aes(xintercept = median(numero_dias))) +
  scale_x_continuous(breaks = function(lims){seq(from = 0, to = lims[2], by = 5)}) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Número de defunciones") +
  xlab("Días entre inicio de síntomas y defunción") +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
# p1
archivo <- file.path(args$dir_salida, "tiempo_defuncion.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "tiempo_defuncion@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)  

