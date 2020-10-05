# (C) Copyright 2020 Sur Herrera Paredes

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

library(tidyverse)
args <- list(serie_tiempo_hosp = "../datos/hospitalizaciones/serie_tiempo_estados_hosp_irag.tsv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Ocupación hospitalaria nacional...\n")

Dat <- read_tsv("../datos/hospitalizaciones/serie_tiempo_estados_hosp_irag.tsv",
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 estado = col_character(),
                                .default = col_number()))


p1 <- Dat %>%
  group_by(fecha) %>%
  summarise(GEN_Disponibles = sum(IRAG_GEN_Disponibles),
            GEN_Ocupadas = sum(IRAG_GEN_Ocupadas),
            VENT_Disponibles = sum(IRAG_VENT_Disponibles),
            VENT_Ocupadas = sum(IRAG_VENT_Total),
            .groups = 'drop') %>%
  pivot_longer(-fecha, values_to = "Número", names_to = "Tipo de cama") %>%
  mutate(`Tipo de cama` = replace(`Tipo de cama`,
                                  `Tipo de cama` == "GEN_Disponibles",
                                  "General\n(disponibles)")) %>%
  
  mutate(`Tipo de cama` = replace(`Tipo de cama`,
                                  `Tipo de cama` == "GEN_Ocupadas",
                                  "General\n(ocupadas)")) %>%
  mutate(`Tipo de cama` = replace(`Tipo de cama`,
                                  `Tipo de cama` == "VENT_Disponibles",
                                  "Ventilador\n(disponibles)")) %>%
  mutate(`Tipo de cama` = replace(`Tipo de cama`,
                                  `Tipo de cama` == "VENT_Ocupadas",
                                  "Ventilador\n(ocupadas)")) %>%
  
  ggplot(aes(x = fecha, y = `Número`)) +
  geom_bar(aes(fill = `Tipo de cama`), width=1, stat = "identity") +
  # scale_fill_brewer(palette = "Paired") +
  scale_fill_manual(values = c("#ffff99", "#b15928", "#fb9a99", "#e31a1c")) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Número de camas IRAG") +
  xlab("Fecha") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.margin = margin(l = 20, r = 20, t = 10),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank())
# p1
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
