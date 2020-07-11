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

args <- list(tabla_estimados = "estimados/rt_live_estimados.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Graficando R efectiva...\n")

Dat <- read_csv(args$tabla_estimados,
                col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                 estado = col_character(),
                                 fecha_estimado = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))

p1 <- Dat %>%
  ggplot(aes(x = date, y = median)) +

  facet_wrap(~ estado, ncol = 5) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), color = "blue", alpha = 0.2) +
  geom_line(size = 2, col = "blue") +
  geom_hline(yintercept = 1) +
  ylab("Promedio de contagios por enfermo de COVID-19 (R_t)") +
  # xlab("Fecha") +
  # AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top",
        axis.title = element_text(face = "bold", size = 12),
        plot.margin = margin(l = 20, r = 20, b = 20),
        strip.text = element_text(face = "bold"))
# p1
archivo <- file.path(args$dir_salida, "r_efectiva.png")
ggsave(archivo, p1, width = 7, height = 9.5, dpi = 75)
archivo <- file.path(args$dir_salida, "r_efectiva@2x.png")
ggsave(archivo, p1, width = 7, height = 9.5, dpi = 150)

