# (C) Copyright 2021 Sur Herrera Paredes

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(tidyverse)
source("util/leer_datos_abiertos.r")

args <- list(datos_abiertos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             lut_zm = "../datos/util/zonas_metropolitanas_2015.csv",
             dias_recientes = 14,
             dias_ventana = 7,
             fecha_ref = "2021-01-20",
             fecha_inicio = "2021-03-01",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Mortalidad por edad y tipo de municipio...\n")
args$fecha_ref <- parse_date(args$fecha_ref, format = "%Y-%m-%d")
args$fecha_inicio <- parse_date(args$fecha_inicio, format = "%Y-%m-%d")
roll_media <- tibbletime::rollify(mean, window = args$dias_ventana, na_value = 0)

Dat <- leer_datos_abiertos(args$datos_abiertos, solo_confirmados = TRUE,
                           solo_fallecidos = TRUE, solo_laboratorio = FALSE)

# Agregar por edad y  fecha
Dat <- Dat %>%
  mutate(edad = NA) %>%
  mutate(edad = replace(EDAD, EDAD >= 60, "60+")) %>%
  mutate(edad = replace(edad, EDAD >= 50 & EDAD < 60, "50-59")) %>%
  mutate(edad = replace(edad, EDAD >= 40 & EDAD < 50, "40-49")) %>%
  mutate(edad = replace(edad, EDAD >= 30 & EDAD < 40, "30-39")) %>%
  mutate(edad = replace(edad, EDAD < 30, "0-29")) %>%
  group_by(edad, FECHA_DEF) %>%
  count(name = "muertes") %>%
  ungroup()

# Obtener referencia
Ref <- Dat %>%
  filter(FECHA_DEF == args$fecha_ref) %>%
  select(edad, muertes)

# Normalizar
Dat <- Dat %>%
  left_join(Ref, by = "edad", suffix = c("", "_ref")) %>%
  mutate(muertes_escala = muertes / muertes_ref) %>%
  select(edad, fecha = FECHA_DEF, muertes, muertes_escala)

# Expandir y ventana movil
Dat <- Dat %>%
  split(.$edad) %>%
  map_dfr(function(d, fecha_inicio = NA, fecha_final = NA, dias_ventana = 7){
    fecha_inicio <- fecha_inicio - dias_ventana
    n_dias <- as.numeric(fecha_final - fecha_inicio)
    
    e <- unique(d$edad)
    
    tibble(fecha = fecha_inicio + 0:n_dias) %>%
      full_join(d, by = "fecha") %>%
      mutate(muertes = replace_na(muertes, 0),
             muertes_escala = replace_na(muertes_escala, 0),
             edad = e) %>%
      arrange(fecha) %>%
      mutate(muertes_escala2 = roll_media(muertes_escala)) %>%
      mutate(fecha2 = fecha - round(dias_ventana / 2) + 1) %>%
      select(fecha2, edad, muertes_escala2) %>%
      rename(fecha = fecha2, muertes_escala = muertes_escala2) %>%
      filter(fecha >= fecha_inicio + round(dias_ventana / 2))
    
  }, fecha_inicio = min(Dat$fecha), fecha_final = max(Dat$fecha),
  dias_ventana = args$dias_ventana)

# Definir olas
Dat$ola <- NA
Dat$ola[ Dat$fecha >= "2020-04-01" & Dat$fecha <= as.Date("2020-04-01") + 192 ] <- "Ola 1"
Dat$ola[ Dat$fecha >= "2020-11-01" & Dat$fecha <= as.Date("2020-11-01") + 135 ] <- "Ola 2"
Dat$ola[ Dat$fecha >= "2021-06-01" & Dat$fecha <= as.Date("2021-06-01") + 180 ] <- "Ola 3"
Dat$ola[ Dat$fecha >= "2021-12-20" & Dat$fecha <= as.Date("2021-12-20") + 90 ] <- "Ola 4"

# Dat <- Dat2

dias_recientes <- tibble(ola = "Ola 4",
                         fecha_final = max(Dat$fecha),
                         fecha_reciente = max(Dat$fecha) - args$dias_recientes,
                         fecha = max(Dat$fecha),
                         muertes_escala = min(Dat$muertes_escala))
# Dat2 <- Dat
# args$fecha_final <- max(Dat$fecha)
# args$fecha_reciente <- args$fecha_final - args$dias_recientes
Dat <- Dat %>%
  filter(!is.na(ola))

# if(dias_recientes$fecha_reciente < max(Dat$fecha) & dias_recientes$fecha_final > max(Dat$fecha)){
#   dias_recientes$fecha_final <- max(Dat$fecha)
# }else if(dias_recientes$fecha_reciente > max(Dat$fecha)){
#   warn("CHECK that this works")
#   dias_recientes$ola <- "Sin ola"
# }

if(max(Dat$fecha) == dias_recientes$fecha_final){
  cat("\tTodavía estamos en la ola 4\n")
}else if(max(Dat$fecha) < dias_recientes$fecha_final && max(Dat$fecha) >= dias_recientes$fecha_reciente){
  cat("\tAlgunos días faltan...\n")
  dias_recientes$fecha_final <- max(Dat$fecha)
}else if(max(Dat$fecha) < dias_recientes$fecha_final && max(Dat$fecha) < dias_recientes$fecha_reciente){
  cat("\tYa pasamos la ola. CHECAR\n")
  dias_recientes$fecha_final <- NA
  dias_recientes$fecha_reciente <-NA
}else{
  STOP("ERROR")
}


p1 <- Dat %>%
  # filter(!is.na(ola)) %>%
  # filter(fecha >= fecha_inicio & fecha <= fecha_inicio + 200 ) %>%
  ggplot(aes(x = fecha, y = muertes_escala)) +
  facet_wrap(ola ~ ., scales = "free", nrow = 2)

if(!is.na(dias_recientes$fecha_final)){
  p1 <- p1 + 
    geom_rect(data = dias_recientes,
              aes(linetype = "Casos en estos días\npueden aumentar",
                  xmin = fecha_reciente,
                  xmax = fecha_final,
                  ymin = -Inf,
                  ymax = Inf),
              fill = "pink") +
    geom_vline(data = dias_recientes,
               aes(xintercept = fecha_reciente)) +
    scale_linetype_manual(values = c("Casos en estos días\npueden aumentar" = 0),
                          name = "",
                          guide = guide_legend(override.aes = list(fill = c("pink"))))
}

p1 <- p1 + 
  geom_line(aes(col = edad), size = 2) +
  scale_color_manual(values = c("#5e3c99", "#b2abd2", "#fdb863", "#8073ac", "#e66101")) +
  guides(col = guide_legend(nrow = 2)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_labels = "%b/%Y") +
  xlab("Fecha de defunción") +
  ylab("Mortalidad con respecto al\n20 de enero de 2021") +
  theme_classic() +
  theme(panel.background = element_blank(),
        plot.margin = margin(l = 20, r = 20),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 20, face = "bold", color = "black"),
        axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 90))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "mortalidad_edad.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "mortalidad_edad@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

