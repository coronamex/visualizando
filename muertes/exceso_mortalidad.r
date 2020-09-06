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
rolling_mean <- tibbletime::rollify(mean, window = 7, na_value = NA)

graficar_exceso_mortalidad <- function(d, m){
  # Agrupar datos por mes
  d <- d %>%
    separate(fecha, into = c("A", "M", "D"), sep = "-") %>%
    group_by(A, M) %>%
    summarise(D = as.character(max(as.numeric(D))),
              muertes = sum(muertes),
              .groups = "drop") %>%
    mutate(fecha_dummy = paste("2020", M, D, sep = "-")) %>%
    mutate(fecha_dummy = parse_date(fecha_dummy, format = "%Y-%m-%d")) 
  
  # Calcular base
  base <- d %>%
    group_by(M) %>%
    summarise(muertes.med = median(muertes),
              muertes.sd = sd(muertes),
              .groups = 'drop') %>%
    mutate(base = muertes.med + 2 * floor(muertes.sd))
  
  # Datos recientes y calcular exceso
  m <- m %>%
    rename(A = anio, M = mes) %>%
    mutate(A = as.character(A),
           D = lubridate::ceiling_date(parse_date(paste(A, M, "01", sep = "-")),
                                       "month") - 1) %>%
    separate(D, into = c(NA, NA, "D"), sep = "-") %>%
    left_join(base, by = "M") %>%
    mutate(fecha_dummy = paste("2020", M, D, sep = "-")) %>%
    mutate(fecha_dummy = parse_date(fecha_dummy, format = "%Y-%m-%d"))  %>%
    mutate(exceso = muertes > base)
  
  # Combinar todas las defunciones
  d <- d %>%
    bind_rows(m %>%
                select(A, M, D, muertes, fecha_dummy))
  
  y_max <- max(d$muertes)
  
  # Calcular exceso de mortalidad
  extra <- m %>%
    filter(fecha_dummy >= "2020-03-01") %>%
    filter(exceso) %>%
    summarise(base = sum(base), muertes = sum(muertes))
  extra$exceso.num <- extra$muertes - extra$base
  extra$exceso.perc <- 100 * ((extra$muertes / extra$base) - 1)
  extra$exceso.num <- ifelse(extra$exceso.num < 0, 0, extra$exceso.num)
  extra$exceso.perc <- ifelse(extra$exceso.perc < 0, 0, extra$exceso.perc)
  extra$A <- "A"
  
  d %>%
    ggplot(aes(x = fecha_dummy, y = muertes, group = A)) +

    geom_ribbon(data = m %>%
                  mutate(base = ifelse(base > muertes, muertes, base)),
                aes(ymin = base, ymax = muertes),
                fill = "#fdae6b",
                alpha = 1, size = 0) +
    geom_line(aes(col = A, size = A)) +
    
    scale_color_manual(values = rev(grey.colors(n = 8, start = 0)),
                       name = "Año") +
    scale_size_manual(values = c(rep(1,7), 2),
                      name = "Año") +

    
    geom_text(data = extra,
              aes(label = paste0("+",
                                 scales::comma(exceso.num),
                                 "\n(+",
                                 round(exceso.perc),
                                 "%)\nexceso\nde\nfallecimientos"),
                  y = 0.8 * y_max), x = parse_date("2020-10-01"),
              col = "#fdae6b", fontface = "bold", size = 6, alpha = 1) +
    scale_y_continuous(labels = scales::comma,
                       breaks = function(limits){
                         seq(from = round(limits[1], -4),
                             to = limits[2], by = 1e4)
                       }, limits = c(0, y_max)) +
    ylab("Defunciones mensuales por todas las causas") +
    xlab("Mes de defunción") +
    AMOR::theme_blackbox() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 10),
          plot.margin = margin(l = 20, r = 20),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA, colour = "black", size = 6),
          legend.position = "top",
          legend.text = element_text(size = 12),
          legend.background = element_blank(),
          legend.key = element_blank())
}

args <- list(defs_entidad_prevs = "../mortalidad/catalogos_defs/defunciones_por_entidad.tsv",
             defs_entidad_rec = "../mortalidad/proyecto.li/def_entidades.tsv", 
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Exceso de mortalidad...\n")

Defs <- read_tsv(args$defs_entidad_prevs,
                 col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                  CVE_ENT = col_character(),
                                  .default = col_number()))

Li <- read_tsv(args$defs_entidad_rec,
               col_types = cols(CVE_ENT = col_character(),
                                mes = col_character(),
                                .default = col_number()))
# Li

# Elegir defunciones previas
# d <- Defs %>%
#   filter(CVE_ENT == "12") %>%
#   select(-CVE_ENT) %>%
#   transmute(fecha, muertes = def_residentes)
d <- Defs %>%
  filter(!(CVE_ENT %in% c("33", "34", "35"))) %>%
  group_by(fecha) %>%
  summarise(muertes = sum(def_residentes), .groups = "drop")
# d

# Defunciones recieonts
# m <- Li %>%
#   filter(CVE_ENT == "12") %>%
#   select(-CVE_ENT)
m <- Li %>%
  filter(CVE_ENT != "39") %>%
  group_by(anio, mes) %>%
  summarise(muertes = sum(muertes),
            .groups = "drop")
# m

p1 <- graficar_exceso_mortalidad(d = d, m = m)
# p1
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)





