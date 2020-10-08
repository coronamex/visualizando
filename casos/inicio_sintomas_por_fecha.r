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
args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz")
cat("Inicio síntomas por fecha...\n")

Dat <- leer_datos_abiertos(args$base_de_datos, solo_confirmados = TRUE, solo_fallecidos = FALSE)
# Dat <- leer_datos_abiertos(args$base_de_datos, solo_confirmados = FALSE, solo_fallecidos = FALSE)
# 
# table(Dat$OTRO_CASO)
# Dat %>%
#   mutate(OTRO_CASO = replace(OTRO_CASO, OTRO_CASO == "99", NA)) %>%
#   mutate(OTRO_CASO = OTRO_CASO == "1") %>%
#   # filter(OTRO_CASO) %>%
#   filter(!is.na(FECHA_DEF)) %>%
#   filter((RESULTADO == "1") | (OTRO_CASO & RESULTADO == "3"))
#   # filter(FECHA_INGRESO < "2020-10-03") %>%
#   # filter(RESULTADO != "1") %>%
#   # select(OTRO_CASO, RESULTADO) %>% table



# Todo el país
fecha_inicio <- min(Dat$FECHA_SINTOMAS) - 0.5
fecha_inicio <- parse_date("2020-03-01", format = "%Y-%m-%d") - 0.5
fecha_final <- max(Dat$FECHA_SINTOMAS) + 0.5
p1 <- Dat %>%
  filter(FECHA_SINTOMAS >= fecha_inicio) %>%
  ggplot(aes(x = FECHA_SINTOMAS)) +
  geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  geom_bar(aes(y=..count.., fill=TIPO_PACIENTE), width = 1) +
  scale_fill_manual(values = c("#66c2a5", "#8da0cb"),
                    labels = c("Ambulatorio", "Hospitalizado"),
                    name = "") +
  annotate("text",
           x = fecha_final - 6,
           y = 0.88 * max(table(Dat$FECHA_SINTOMAS)),
           label = 'italic("Estos\nnúmeros\npueden\naumentar")',
           hjust = "middle",
           parse = TRUE) +
  xlim(c(fecha_inicio, fecha_final)) +
  scale_y_continuous(labels = scales::comma,
                     breaks = function(lims){
                       seq(from = 0, to = lims[2], by = 1e3)
                     }) +
  ylab(label = "Número de nuevos casos") +
  xlab(label = "Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank())
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "inicio_sintomas_por_fecha_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "inicio_sintomas_por_fecha_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
