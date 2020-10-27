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

graficar_ocupacion_hospitalaria <- function(Dat, estados = NULL){
  if(!is.null(estados)){
    Dat <- Dat %>%
      filter(estado %in% estados) %>%
      select(-IRAG_GEN_Total, -IRAG_VENT_Total) %>%
      rename(GEN_Disponibles = IRAG_GEN_Disponibles,
             GEN_Ocupadas = IRAG_GEN_Ocupadas,
             VENT_Disponibles = IRAG_VENT_Disponibles,
             VENT_Ocupadas = IRAG_VENT_Ocupadas) %>%
      pivot_longer(cols = c(-fecha, -estado), values_to = "Número", names_to = "Tipo de cama")
  }else{
    Dat <- Dat %>%
      group_by(fecha) %>%
      summarise(GEN_Disponibles = sum(IRAG_GEN_Disponibles),
                GEN_Ocupadas = sum(IRAG_GEN_Ocupadas),
                VENT_Disponibles = sum(IRAG_VENT_Disponibles),
                VENT_Ocupadas = sum(IRAG_VENT_Total),
                .groups = 'drop') %>%
      pivot_longer(-fecha, values_to = "Número", names_to = "Tipo de cama")
  }
  
  p1 <- Dat %>%
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
    
    mutate(`Tipo de cama` = factor(`Tipo de cama`,
                                   levels = c("General\n(disponibles)",
                                              "General\n(ocupadas)",
                                              "Ventilador\n(ocupadas)",
                                              "Ventilador\n(disponibles)"))) %>%
    ggplot(aes(x = fecha, y = `Número`)) +
    geom_bar(aes(fill = `Tipo de cama`), width=1, stat = "identity") +
    # scale_fill_brewer(palette = "Paired") +
    # scale_fill_manual(values = c("#ffff99", "#b15928", "#e31a1c", "#fb9a99")) +
    scale_fill_manual(values = c("#fff7bc", "#ec7014", "#993404", "#fec44f")) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(breaks = parse_date(paste("01", 1:12, "2020", sep = "-"), format="%d-%m-%Y"),
                 date_labels = "%b") +
    ylab("Número de camas IRAG") +
    xlab("Fecha") +
    AMOR::theme_blackbox() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 10),
	  axis.text.x = element_text(angle = 90),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.margin = margin(l = 20, r = 20, t = 10),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA, colour = "black", size = 3),
          legend.position = "top",
          legend.text = element_text(size = 12),
          legend.background = element_blank())
  
  if(!is.null(estados)){
    p1 <- p1 +
      facet_wrap(~ estado, scales = "free_y")
  }
  
  p1
}

args <- list(serie_tiempo_hosp = "../datos/hospitalizaciones/serie_tiempo_estados_hosp_irag.tsv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Ocupación hospitalaria\n")

Dat <- read_tsv("../datos/hospitalizaciones/serie_tiempo_estados_hosp_irag.tsv",
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 estado = col_character(),
                                .default = col_number()))


p1 <- graficar_ocupacion_hospitalaria(Dat = Dat, estados = NULL)
# p1
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Graficar por estado
estados <- Dat$estado %>% unique %>% sort

p1 <- graficar_ocupacion_hospitalaria(Dat = Dat, estados = estados[1:8])
# p1
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados1.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados1@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_ocupacion_hospitalaria(Dat = Dat, estados = estados[9:16])
# p1
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados2.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados2@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_ocupacion_hospitalaria(Dat = Dat, estados = estados[17:24])
# p1
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados3.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados3@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_ocupacion_hospitalaria(Dat = Dat, estados = estados[25:32])
# p1
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados4.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "ocupacion_hospitalaria_estados4@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)






