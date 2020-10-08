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

args <- list(serie_tiempo_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv.gz",
             dias_previos = 7,
             muncipios_lut = "../datos/util/municipios_lut_datos_abiertos.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Número de municipios...\n")

Tab <- read_csv(args$serie_tiempo_municipios,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 municipio = col_character(),
                                 clave = col_character(),
                                 .default = col_number()))

# Casos en últimos días
dat <- Tab %>%
  select(fecha, sintomas_acumulados, clave, municipio) %>%
  # print() %>%
  split(.$clave) %>%
  map_dfr(function(d, dias_previos = 3){
    d %>%
      mutate(casos_recientes = sintomas_acumulados - lag(sintomas_acumulados, dias_previos, default = 0))
  }, dias_previos = args$dias_previos) %>%
  # print(n = 100) %>%
  group_by(fecha) %>%
  summarise(n_municipios = sum(casos_recientes > 0)) %>%
  filter(fecha >= "2020-02-01")
final_datos <- max(dat$fecha)
p1 <- dat  %>%
  filter(fecha >= "2020-03-01") %>%
  ggplot(aes(x = fecha, y = n_municipios)) +
  geom_rect(aes(linetype = "Casos en estos días pueden aumentar"),
            xmin = final_datos - 15,
            xmax = final_datos,
            ymin = -Inf,
            ymax = Inf,
            fill = "pink") +
  scale_linetype_manual(values = c("Casos en estos días pueden aumentar" = 0),
                        name = "",
                        guide = guide_legend(override.aes = list(fill = c("pink")))) +
  
  geom_line(size = 3) +

  ylab("# muncipios con casos en semana previa") +
  xlab("Fecha de inicio de síntomas") +
  scale_y_continuous(labels = scales::comma,
                     breaks = function(lims){
                       seq(from = 0, to = lims[2], by = 100)
                     }) +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20, t = 10),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.background = element_blank())
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "n_municipios.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "n_municipios@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

