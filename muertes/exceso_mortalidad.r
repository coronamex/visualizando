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
    scale_x_date(breaks = sort(unique(d$fecha_dummy))[-3],
                 labels = scales::date_format("%b")) +
    ylab("Defunciones mensuales por todas las causas") +
    xlab("Mes de defunción") +
    AMOR::theme_blackbox() +
    theme(axis.title = element_text(size = 20),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(vjust = 0.5),
          plot.margin = margin(l = 20, r = 20),
          panel.background = element_blank(),
          panel.border = element_rect(fill=NA, colour = "black", size = 6),
          legend.position = "top",
          legend.text = element_text(size = 12),
          legend.background = element_blank(),
          legend.key = element_blank())
}

exceso_mortalidad_region <- function(d, m, estados, nrow = 3){
  # Agrupar datos por mes
  d <- d %>%
    filter(estado %in% estados) %>%
    separate(fecha, into = c("A", "M", "D"), sep = "-") %>%
    group_by(estado, A, M) %>%
    summarise(D = as.character(max(as.numeric(D))),
              muertes = sum(muertes),
              .groups = "drop") %>%
    mutate(fecha_dummy = paste("2020", M, D, sep = "-")) %>%
    mutate(fecha_dummy = parse_date(fecha_dummy, format = "%Y-%m-%d")) 
  
  # Calcular base
  base <- d %>%
    group_by(estado, M) %>%
    summarise(muertes.med = median(muertes),
              muertes.sd = sd(muertes),
              .groups = 'drop') %>%
    mutate(base = muertes.med + 2 * floor(muertes.sd))
  
  # Datos recientes y calcular exceso
  m <- m %>%
    filter(estado %in% estados) %>%
    rename(A = anio, M = mes) %>%
    mutate(A = as.character(A),
           D = lubridate::ceiling_date(parse_date(paste(A, M, "01", sep = "-")),
                                       "month") - 1) %>%
    separate(D, into = c(NA, NA, "D"), sep = "-") %>%
    left_join(base, by = c("M", "estado")) %>%
    mutate(fecha_dummy = paste("2020", M, D, sep = "-")) %>%
    mutate(fecha_dummy = parse_date(fecha_dummy, format = "%Y-%m-%d"))  %>%
    mutate(exceso = muertes > base)
  
  # Combinar todas las defunciones
  d <- d %>%
    bind_rows(m %>%
                select(estado, A, M, D, muertes, fecha_dummy))
  
  # Calcular exceso de mortalidad
  extra <- m %>%
    filter(fecha_dummy >= "2020-03-01") %>%
    filter(exceso) %>%
    group_by(estado) %>%
    summarise(base = sum(base),
              muertes = sum(muertes),
              .groups = "drop") %>%
    mutate(exceso.num = muertes - base) %>%
    mutate(exceso.num = replace(exceso.num, exceso.num < 0, 0)) %>%
    mutate(exceso.perc = 100 * exceso.num / base) %>%
    mutate(A = "A") %>%
    left_join(d %>% group_by(estado) %>% summarise(y_max = max(muertes), .groups = "drop"),
              by = "estado")
  # extra
  # extra %>% arrange(desc(exceso.perc)) %>% print(n = 100)
  # extra %>% arrange(desc(exceso.num)) %>% print(n = 100)
  
  p1 <- d %>%
    ggplot(aes(x = fecha_dummy, y = muertes, group = A)) +
    geom_ribbon(data = m %>%
                  filter(fecha_dummy >= "2020-03-01") %>%
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
                                 "%)"),
                  y = 0.8 * y_max), x = parse_date("2020-10-01"),
              col = "#fdae6b", fontface = "bold", size = 3, alpha = 1) +
    scale_y_continuous(labels = scales::comma,
                       # breaks = function(limits){
                       #   seq(from = round(limits[1], -4),
                       #       to = limits[2], by = 1e4)
                       # },
                       limits = function(limits){
                         limits[1] <- 0
                         limits
                       }) +
    scale_x_date(breaks = sort(unique(d$fecha_dummy))[-3],
                 labels = scales::date_format("%b")) +
    ylab("Defunciones mensuales por todas las causas") +
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
  
  if(length(estados) > 1){
    p1 <- p1 +
      facet_wrap(~ estado, scales = "free_y", nrow = nrow)
  }
  
  p1
}


args <- list(defs_entidad_prevs = "../mortalidad/catalogos_defs/defunciones_por_entidad.tsv",
             defs_entidad_rec = "../mortalidad/proyecto.li/def_entidades.tsv", 
             estados_lut = "../datos/util/estados_lut_datos_abiertos.csv",
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
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
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

d <- Defs %>%
  filter(!(CVE_ENT %in% c("33", "34", "35"))) %>%
  group_by(CVE_ENT, fecha) %>%
  summarise(muertes = sum(def_residentes),
            .groups = "drop") %>%
  mutate(estado = as.character(estados_lut[CVE_ENT])) %>%
  select(-CVE_ENT)

m <- Li %>%
  filter(CVE_ENT != "39") %>%
  group_by(CVE_ENT, anio, mes) %>%
  summarise(muertes = sum(muertes),
            .groups = "drop") %>%
  mutate(estado = as.character(estados_lut[CVE_ENT])) %>%
  select(-CVE_ENT)

estados <- 

p1 <- exceso_mortalidad_region(d = d, m = m,
                               estados = as.character(sort(estados_lut)[1:8]))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados1.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados1@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- exceso_mortalidad_region(d = d, m = m,
                               estados = as.character(sort(estados_lut)[9:16]))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados2.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados2@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


p1 <- exceso_mortalidad_region(d = d, m = m,
                               estados = as.character(sort(estados_lut)[17:24]))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados3.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados3@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


p1 <- exceso_mortalidad_region(d = d, m = m,
                               estados = as.character(sort(estados_lut)[25:32]))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados4.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_mortalidad_estados4@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

