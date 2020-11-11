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

#' Title
#'
#' @param Dat Tabla con todas las defunciones. Debe tener columnas
#' fecha, region y muertes
#'
#' @return
#' @export
calcular_exceso_mortalidad <- function(Dat){

  Dat <- Dat %>%
    separate(fecha, into = c("A", "M", "D"), sep = "-")
  
  # Calcular muertes esperadas, y cuantil 95
  base <- Dat %>%
    filter(A %in% c("2012", "2013", "2014", "2015",
                    "2016", "2017", "2018")) %>%
    group_by(M, D, region) %>%
    summarise(muertes.med = mean(muertes),
              muertes.sd = sd(muertes),
              .groups = 'drop') %>%
    mutate(base = qnorm(p = 0.95, mean = muertes.med, sd = muertes.sd))
    
  # Calcular exceso
  Dat <- Dat %>%
    left_join(base %>%
                mutate(A = "2020"),
              by = c("A", "M", "D", "region")) %>%
    mutate(fecha = paste(A, M, D, sep = "-")) %>%
    mutate(fecha = parse_date(fecha, format = "%Y-%m-%d"))  %>%
    mutate(dummy_fecha = paste("2020", M, D, sep = "-")) %>%
    mutate(dummy_fecha = parse_date(dummy_fecha, format = "%Y-%m-%d")) %>%
    mutate(exceso = muertes > base)
  
  # Calcular exceso de mortalidad
  extra <- Dat %>%
    filter(fecha >= "2020-03-01") %>%
    filter(exceso) %>%
    group_by(region) %>%
    summarise(base = sum(base),
              y_max = max(muertes),
              muertes = sum(muertes),
              .groups = "drop") %>%
    mutate(exceso.num = muertes - floor(base)) %>%
    mutate(exceso.perc = 100 * ((muertes / base) - 1)) %>%
    mutate(exceso.num = ifelse(exceso.num < 0, 0, exceso.num),
           exceso.perc = ifelse(exceso.perc < 0, 0, exceso.perc),
           A = "A")
  
  return(list(Dat = Dat, extra = extra))
}

# graficar_exceso_mortalidad <- function(d, m){
#   
#   l <- calcular_exceso_mortalidad(d = d, m = m) 
#   y_max <- max(l$d$muertes)
#   
#   l$d %>%
#     ggplot(aes(x = fecha, y = muertes, group = A)) +
#     geom_ribbon(data = l$m %>%
#                   mutate(base = ifelse(base > muertes, muertes, base)),
#                 aes(ymin = base, ymax = muertes),
#                 fill = "#fdae6b",
#                 alpha = 1, size = 0) +
#     geom_line(aes(col = A, size = A)) +
#     scale_color_manual(values = rev(grey.colors(n = 8, start = 0)),
#                        name = "Año") +
#     scale_size_manual(values = c(rep(1,7), 2),
#                       name = "Año") +
#     
#     geom_text(data = l$extra,
#               aes(label = paste0("+",
#                                  scales::comma(exceso.num),
#                                  "\n(+",
#                                  round(exceso.perc),
#                                  "%)\nexceso\nde\nfallecimientos"),
#                   y = 0.8 * y_max), x = parse_date("2020-03-01"),
#               col = "#fdae6b", fontface = "bold", size = 6, alpha = 1) +
#     scale_y_continuous(labels = scales::comma,
#                        breaks = function(limits){
#                          seq(from = round(limits[1], -4),
#                              to = limits[2], by = 1e3)
#                        }, limits = c(0, y_max)) +
#     scale_x_date(date_breaks = "1 month",
#                  date_labels = "%b") +
#     ylab("Defunciones por todas las causas") +
#     xlab("Mes de defunción") +
#     AMOR::theme_blackbox() +
#     theme(axis.title = element_text(size = 20),
#           axis.text = element_text(size = 10),
#           axis.text.x = element_text(vjust = 0.5),
#           plot.margin = margin(l = 20, r = 20),
#           panel.background = element_blank(),
#           panel.border = element_rect(fill=NA, colour = "black", size = 6),
#           legend.position = "top",
#           legend.text = element_text(size = 12),
#           legend.background = element_blank(),
#           legend.key = element_blank())
# }

graficar_exceso_mortalidad <- function(Dat, regiones,
                                       exceso.size = 6,
                                       exceso.fecha = "2020-03-01",
                                       lineas.sizes = c(1, 2),
                                       nrow = 3){

  # Seleccionar datos y calcular exceso de mortalidad
  Res <- Dat %>%
    filter(region %in% regiones) %>%
    calcular_exceso_mortalidad()
  
  p1 <- Res$Dat %>%
    ggplot(aes(x = dummy_fecha, y = muertes, group = A)) +
    geom_ribbon(data = Res$Dat %>%
                  filter(fecha >= "2020-03-01") %>%
                  mutate(base = ifelse(base > muertes, muertes, base)),
                aes(ymin = base, ymax = muertes),
                fill = "#fdae6b",
                alpha = 1, size = 0) +
    geom_line(aes(col = A, size = A)) +
    scale_color_manual(values = rev(grey.colors(n = 8, start = 0)),
                       name = "Año") +
    scale_size_manual(values = c(rep(lineas.sizes[1],7), lineas.sizes[2]),
                      name = "Año") +
    geom_text(data = Res$extra,
              aes(label = paste0("+",
                                 scales::comma(exceso.num, accuracy = 1),
                                 "\n(+",
                                 round(exceso.perc),
                                 "%)"),
                  y = 0.8 * y_max), x = parse_date(exceso.fecha),
              col = "#fdae6b", fontface = "bold",
              size = exceso.size,
              alpha = 1) +
    scale_y_continuous(labels = scales::comma,
                       limits = function(limits){
                         limits[1] <- 0
                         limits
                       }) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    ylab("Defunciones por todas las causas") +
    xlab("Mes de defunción") +
    AMOR::theme_blackbox() +
    theme(axis.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90, vjust = 0.5),
          plot.margin = margin(l = 20, r = 20),
          panel.background = element_blank(),
          panel.border = element_blank(),
          legend.position = "top",
          legend.text = element_text(size = 12),
          legend.background = element_blank(),
          legend.key = element_blank())
  
  if(length(regiones) > 1){
    p1 <- p1 +
      facet_wrap(~ region, scales = "free_y", nrow = nrow)
  }
  
  p1
}
#######################

args <- list(defs_entidad_prevs = "../mortalidad/catalogos_defs/defunciones_por_entidad.tsv",
             # defs_entidad_rec = "../mortalidad/proyecto.li/def_entidades.tsv",
             defs_entidad_rec = "../mortalidad/datos_abiertos/defunciones_por_entidad.csv",
             estados_lut = "../datos/util/estados_lut_datos_abiertos.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Exceso de mortalidad...\n")

Defs <- read_tsv(args$defs_entidad_prevs,
                 col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                  CVE_ENT = col_character(),
                                  .default = col_number()))

Pand <- read_csv(args$defs_entidad_rec,
                 col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                  CVE_ENT = col_character()))

Dat <- Defs %>%
  filter(!(CVE_ENT %in% c("33", "34", "35"))) %>%
  select(fecha, CVE_ENT, muertes = def_registradas) %>%
  bind_rows(Pand %>% rename(muertes = def_registradas))
Dat


p1 <- graficar_exceso_mortalidad(Dat = Dat %>%
                           group_by(fecha) %>%
                           summarise(muertes = sum(muertes),
                                     .groups = 'drop') %>%
                           mutate(region = "País"),
                         regiones = "País")
archivo <- file.path(args$dir_salida, "exceso_mortalidad_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

### Por estado
estados_lut <- read_csv(args$estados_lut,
                        col_names = FALSE,
                        col_types = cols(.default = col_character()))
stop_for_problems(estados_lut)
estados_lut <- set_names(estados_lut$X2, estados_lut$X1)
estados_lut <- estados_lut[1:32]


Dat <- Dat %>%
  mutate(region = as.character(estados_lut[CVE_ENT])) %>%
  select(-CVE_ENT)

p1 <- graficar_exceso_mortalidad(Dat = Dat,
                                 regiones = as.character(sort(estados_lut)[1:8]),
                                 exceso.size = 3,
                                 exceso.fecha = "2020-03-01",
                                 lineas.sizes = c(0.3, 0.3),
                                 nrow = 3)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados1.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados1@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_exceso_mortalidad(Dat = Dat,
                                 regiones = as.character(sort(estados_lut)[9:16]),
                                 exceso.size = 3,
                                 exceso.fecha = "2020-03-01",
                                 lineas.sizes = c(0.3, 0.3),
                                 nrow = 3)archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados2.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados2@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


p1 <- graficar_exceso_mortalidad(Dat = Dat,
                                 regiones = as.character(sort(estados_lut)[17:24]),
                                 exceso.size = 3,
                                 exceso.fecha = "2020-03-01",
                                 lineas.sizes = c(0.3, 0.3),
                                 nrow = 3)archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados3.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados3@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


p1 <- graficar_exceso_mortalidad(Dat = Dat,
                                 regiones = as.character(sort(estados_lut)[25:32]),
                                 exceso.size = 3,
                                 exceso.fecha = "2020-03-01",
                                 lineas.sizes = c(0.3, 0.3),
                                 nrow = 3)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados4.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados4@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

