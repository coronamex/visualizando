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

### DESCONTINUADO: Desde que la DGE dejó de publicar información de casos importados

library(tidyverse)

args <- list(casos_positivos = "../datos/ssa_dge/2020-04-05/tabla_casos_confirmados.csv",
             dge_dir = "../datos/ssa_dge/",
             dir_salida = "../sitio_hugo/static/imagenes/")
estados_nombres_dge <- c(AGUASCALIENTES = "Aguascalientes",
                         `BAJA CALIFORNIA` = "Baja California",
                         `BAJA CALIFORNIA SUR` = "Baja California Sur",
                         "BAJA CALIFORNIA \nSUR" = "Baja California Sur",
                         `CAMPECHE` = "Campeche",
                         `CIUDAD DE MÉXICO` = "Ciudad de México",
                         `DISTRITO FEDERAL` = "Ciudad de México",
                         COAHUILA = "Coahuila",
                         COLIMA = "Colima",
                         CHIAPAS = "Chiapas",
                         CHIHUAHUA = "Chihuahua",
                         DURANGO = "Durango",
                         GUANAJUATO = "Guanajuato",
                         GUERRERO = "Guerrero",
                         HIDALGO = "Hidalgo",
                         JALISCO = "Jalisco",
                         `MÉXICO` = "México",
                         `MEXICO` = "México",
                         `MICHOACÁN` = "Michoacán",
                         `MICHOACAN` = "Michoacán",
                         MORELOS = "Morelos",
                         NAYARIT = "Nayarit",
                         `NUEVO LEÓN` = "Nuevo León",
                         `NUEVO LEON` = "Nuevo León",
                         OAXACA = "Oaxaca",
                         PUEBLA = "Puebla",
                         QUERETARO = "Querétaro",
                         `QUINTANA ROO` = "Quintana Roo",
                         `SAN LUIS POTOSÍ` = "San Luis Potosí",
                         `SAN LUIS POTOSI` = "San Luis Potosí",
                         SINALOA = "Sinaloa",
                         SONORA = "Sonora",
                         TABASCO = "Tabasco",
                         TAMAULIPAS = "Tamaulipas",
                         TLAXCALA = "Tlaxcala",
                         VERACRUZ = "Veracruz",
                         `YUCATÁN` = "Yucatán",
                         `YUCATAN` = "Yucatán",
                         ZACATECAS = "Zacatecas")

Tab <- read_csv(args$casos_positivos,
                col_types = cols(fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                 fecha_llegada = col_date(format = "%d/%m/%Y")))
Tab <- Tab %>%
  mutate(estado = as.character(estados_nombres_dge[match(estado, names(estados_nombres_dge))])) %>%
  filter(procedencia ==  "Contacto")  %>%
  filter(is.na(fecha_llegada)) %>%
  split(.$estado) %>%
  map_dfr(function(d){
    d %>%
      arrange(fecha_sintomas) %>%
      head(1)
  }) %>%
  arrange(fecha_sintomas)

Tab <- Tab %>%
  mutate(region = NA) %>%
  mutate(region = replace(region, estado %in% c("Baja California", "Baja California Sur", "Chihuahua", "Durango", "Sinaloa", "Sonora"), "Noroeste")) %>%
  mutate(region = replace(region, estado %in% c("Coahuila", "Nuevo León", "Tamaulipas"), "Noreste")) %>%
  mutate(region = replace(region, estado %in% c("Colima", "Jalisco", "Michoacán", "Nayarit"), "Oeste")) %>%
  mutate(region = replace(region, estado %in% c("Hidalgo", "Puebla", "Tlaxcala", "Veracruz"), "Este")) %>%
  mutate(region = replace(region, estado %in% c("Aguascalientes", "Guanajuato", "Querétaro", "San Luis Potosí", "Zacatecas"), "Centronorte")) %>%
  mutate(region = replace(region, estado %in% c("Ciudad de México", "México", "Morelos"), "Centrosur")) %>%
  mutate(region = replace(region, estado %in% c("Chiapas", "Guerrero", "Oaxaca"), "Suroeste")) %>%
  mutate(region = replace(region, estado %in% c("Campeche", "Quintana Roo", "Tabasco", "Yucatán"), "Sureste"))
Tab

fechas_dirs <- list.dirs(args$dge_dir, recursive = FALSE, full.names = TRUE)
# Usando datos hasta 2020-04-05
fechas_dirs <- fechas_dirs[ 1:which(str_detect(fechas_dirs, pattern = "2020-04-05")) ]
Detecciones <- fechas_dirs %>%
  map_dfr(function(dir, Tab, estados_lut){
    # dir <- fechas_dirs[18]
    # dir <- "../datos/ssa_dge//2020-03-18"
    cat(dir, "\n")
    archivo_tabla <- file.path(dir, "tabla_casos_confirmados.csv")
    fecha <- basename(dir) %>% as.Date(format = "%Y-%m-%d")
    # fecha
    if(file.exists(archivo_tabla)){
      tab <- read_csv(archivo_tabla,
                      col_types = cols(fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                       fecha_llegada = col_date(format = "%d/%m/%Y"))) %>%
        mutate(estado = str_remove(string = estado, pattern = "[*]$")) %>%
        mutate(estado = as.character(estados_lut[match(estado, names(estados_lut))]))
      
      if(any(is.na(tab$estado)))
        stop("ERROR", call. = TRUE)
      
      # Encontrar casos comunitarios por estado
      res <- tab %>%
        filter(procedencia ==  "Contacto")  %>%
        filter(is.na(fecha_llegada)) %>%
        split(.$estado) %>%
        map_dfr(function(d){
          d %>%
            arrange(fecha_sintomas) %>%
            head(1)
        }) %>%
        arrange(fecha_sintomas)
      
      # Añadir fecha
      res$fecha_deteccion <- fecha
      
      # # Encontrar apariciones de cada caso
      # res <- Tab %>%
      #   left_join(tab, by = c("estado", "sexo", "edad",
      #                         "fecha_sintomas", "procedencia",
      #                         "fecha_llegada")) %>%
      #   filter(!is.na(fecha_deteccion))
      
      return(res)
    }
  },Tab = Tab, estados_lut = estados_nombres_dge)
Detecciones <- Detecciones %>%
  split(.$estado) %>%
  map_dfr(function(d){
    d %>%
      select(estado, fecha_deteccion) %>%
      arrange(fecha_deteccion) %>%
      head(1)
  }) %>%
  arrange(fecha_deteccion)
Detecciones

Tab <- Tab %>%
  left_join(Detecciones, by = "estado")

dat <- Tab %>%
  select(estado, fecha_sintomas, fecha_deteccion) %>%
  arrange(fecha_sintomas) %>%
  mutate(estado = factor(estado, levels = unique(estado))) %>%
  pivot_longer(-estado, names_to = "grupo", values_to = "fecha")
dat$inicio <- min(dat$fecha)
dat$inicio[dat$grupo == "fecha_deteccion"] <- dat$fecha[dat$grupo == "fecha_sintomas"]
dat

p1 <- dat %>%
  mutate(grupo = factor(grupo, levels = c("fecha_sintomas", "fecha_deteccion"))) %>%
  ggplot(aes(x = fecha, y = estado, col = grupo)) +
  geom_vline(xintercept = as.Date("2020-03-23", "%Y-%m-%d"), col = "red", size = 2) +
  # facet_grid(region ~ ., scales = "free_y", space = "free_y", switch = "y") +
  geom_segment(aes(x=inicio, y = estado, xend = fecha, yend = estado)) +
  geom_point(size = 2) +
  xlab("Fecha") +
  scale_color_manual(values = c("black", "darkgrey"), name = "Transmisión\ncomunitaria",
                     labels = c("Primer caso", "Primer detectado")) +
  theme(panel.background = element_blank(),
        panel.grid.minor = element_line(color = "grey"),
        panel.border = element_rect(color = "black", size = 3, fill = NA),
        axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 0),
        axis.title.x = element_text(face = "bold", color = "black", size = 20),
        axis.title.y = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        plot.margin = margin(l = 20, r = 20))
p1
# archivo <- file.path("test.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "inicio_comunitarios.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "inicio_comunitarios@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# dat %>%
#   filter(grupo == "fecha_sintomas") %>%
#   arrange(fecha) %>%
#   print(n = 100)
# dat %>%
#   filter(grupo == "fecha_deteccion") %>%
#   arrange(fecha) %>%
#   print(n = 100)
# 
# dat %>%
#   mutate(retraso = as.numeric(as.Date("2020-03-23", "%Y-%m-%d") - fecha) + 1) %>%
#   filter(grupo == "fecha_sintomas") %>%
#   # filter(retraso > 0) %>%
#   arrange(retraso) %>%
#   mutate(min = min(retraso), max = max(retraso), mediana = median(retraso)) %>%
#   print(n = 100)
# 
# 
# dat %>%
#   mutate(retraso = as.numeric(as.Date("2020-03-23", "%Y-%m-%d") - fecha) + 1) %>%
#   filter(grupo == "fecha_deteccion") %>%
#   filter(retraso > 0) %>%
#   arrange(retraso) %>%
#   mutate(min = min(retraso), max = max(retraso), mediana = median(retraso)) %>%
#   print(n = 100)
Tab %>%
  mutate(retraso = as.numeric(fecha_deteccion - fecha_sintomas)) %>%
  select(estado, retraso) %>%
  arrange(retraso) %>%
  print(n=100) %>%
  summary
