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
             max_ola1 = "2020-07-21",
             max_ola2 = "2021-01-20",
             fecha_inicio = "2021-03-01",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Mortalidad por edad y tipo de municipio...\n")
args$max_ola1 <- parse_date(args$max_ola1, format = "%Y-%m-%d")
args$max_ola2 <- parse_date(args$max_ola2, format = "%Y-%m-%d")
args$fecha_inicio <- parse_date(args$fecha_inicio, format = "%Y-%m-%d")

Dat <- leer_datos_abiertos(args$datos_abiertos, solo_confirmados = TRUE,
                           solo_fallecidos = TRUE, solo_laboratorio = FALSE)
# Dat2 <- Dat
# Dat <- Dat2

# Agregar por edad, fecha y municipio
Dat <- Dat %>%
  mutate(edad = NA) %>%
  mutate(edad = replace(EDAD, EDAD >= 60, "60+")) %>%
  mutate(edad = replace(edad, EDAD >= 50 & EDAD < 60, "50-59")) %>%
  mutate(edad = replace(edad, EDAD >= 40 & EDAD < 50, "40-49")) %>%
  mutate(edad = replace(edad, EDAD < 40, "0-39")) %>%
  mutate(mun_cve = paste0(ENTIDAD_RES, "_", MUNICIPIO_RES)) %>%
  group_by(mun_cve, edad, FECHA_DEF) %>%
  summarise(muertes = length(FECHA_DEF),
            .groups = 'drop')
# Dat %>%
#   filter(FECHA_DEF == "2021-01-20") %>%
#   select(-muertes) %>%
#   duplicated %>% any

# Leer lista de municipios en zonas metropolitanas
zm_muns <- read_csv(args$lut_zm)
zm_muns <- paste0(zm_muns$CVE_ENT, "_", zm_muns$CVE_MUN %>% str_sub(3,5) )


roll_media <- tibbletime::rollify(mean, window = 7, na_value = 0)

# # Graficar todas las muertes
# Dat %>%
#   group_by(FECHA_DEF) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop')  %>%
#   ggplot(aes(x = FECHA_DEF, y = defs)) +
#   geom_bar(stat = "identity") +
#   theme_classic()
# # Encontrar max ola 2
# Dat %>%
#   group_by(FECHA_DEF) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop')  %>%
#   filter(defs == max(defs))
# 
# # Encontrar max ola 1
# Dat %>%
#   group_by(FECHA_DEF) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop')  %>%
#   filter(FECHA_DEF < "2020-10-01") %>%
#   filter(defs == max(defs))




# Calcular por grupo de edad y tipomun, eliminar días recientes incompletos
Dat <- Dat %>%
  filter(FECHA_DEF < max(FECHA_DEF) - args$dias_recientes) %>%
  mutate(zm = c("no_zm", "zm")[ 1*(mun_cve %in% zm_muns) + 1 ]) %>%
  mutate(zm = "zm") %>%
  group_by(edad, FECHA_DEF, zm) %>%
  summarise(defs = sum(muertes),
            .groups = 'drop') %>%
  # filter(FECHA_DEF == "2021-01-20")
  
  # split(list(.$zm, .$edad)) %>%
  split(list(.$edad)) %>%
  map_dfr(function(d){
    days <- as.numeric(max(d$FECHA_DEF) - min(d$FECHA_DEF))
    tibble(FECHA_DEF = min(d$FECHA_DEF) + 0:days) %>%
      left_join(d, by = "FECHA_DEF") %>%
      mutate(edad = edad[1],
             zm = zm[1],
             defs = replace_na(defs, 0)) %>%
      mutate(muertes_ventana = roll_media(defs)) 
      # mutate(muertes_escala = muertes_ventana / max(muertes_ventana))
  })
# Dat  


n_dias <- as.numeric(max(Dat$FECHA_DEF) - args$max_ola2)
# n_dias

n_dias_grafica <- as.numeric(max(Dat$FECHA_DEF) - args$fecha_inicio)
# n_dias_grafica

p1 <- bind_rows(Dat %>%
            filter(FECHA_DEF >= args$max_ola1 & FECHA_DEF <= args$max_ola1 + n_dias) %>%
            group_by(edad, zm) %>%
            summarise(FECHA_DEF = FECHA_DEF,
                      muertes_escala = muertes_ventana / muertes_ventana[FECHA_DEF == args$max_ola1],
                      .groups = 'drop') %>%
            filter(FECHA_DEF > max(FECHA_DEF) - n_dias_grafica) %>%
            mutate(ola = 'Entre 1a y 2a "olas"'),
          Dat %>%
            filter(FECHA_DEF >= args$max_ola2 & FECHA_DEF <= args$max_ola2 + n_dias) %>%
            group_by(edad, zm) %>%
            summarise(FECHA_DEF = FECHA_DEF,
                      muertes_escala = muertes_ventana / muertes_ventana[FECHA_DEF == args$max_ola2],
                      .groups = 'drop') %>%
            filter(FECHA_DEF > max(FECHA_DEF) - n_dias_grafica) %>%
            mutate(ola = 'Entre 2a y 3a "olas"')) %>%
  mutate(zm = replace(zm, zm == "zm", "Zonas metropolitanas")) %>%
  mutate(zm = replace(zm, zm == "no_zm", "Zonas no metropolitanas")) %>%
  ggplot(aes(x = FECHA_DEF, y = muertes_escala)) +
  # facet_wrap(ola ~ zm, scales = "free") +
  facet_wrap(ola ~ ., scales = "free", nrow = 2) +
  geom_line(aes(col = edad), size = 2) +
  # scale_color_manual(values = c("#5e3c99", "#b2abd2", "#e66101")) +
  scale_color_manual(values = c("#5e3c99", "#b2abd2", "#fdb863", "#e66101")) +
  # scale_color_manual(values = c("#8073ac", "#e08214")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Fecha de defunción") +
  ylab("Fallecimientos como proporción del máximo") +
  theme_classic() +
  theme(plot.margin = margin(l = 20, r = 20),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(face = "bold"),
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "mortalidad_edad_tipomun.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "mortalidad_edad_tipomun@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


    


# fecha_inicio <- max(Dat$FECHA_DEF) - args$n_dias
# Dat %>%
#   mutate(zm = c("no_zm", "zm")[ 1*(mun_cve %in% zm_muns) + 1 ]) %>%
#   # select(zm) %>% table 
#   group_by(edad, FECHA_DEF, zm) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop') %>%
#   split(list(.$zm, .$edad)) %>%
#   map_dfr(function(d){
#     days <- as.numeric(max(d$FECHA_DEF) - min(d$FECHA_DEF))
#     tibble(FECHA_DEF = min(d$FECHA_DEF) + 0:days) %>%
#       left_join(d, by = "FECHA_DEF") %>%
#       mutate(edad = edad[1],
#              zm = zm[1],
#              defs = replace_na(defs, 0)) %>%
#       mutate(muertes_ventana = roll_media(defs)) %>%
#       mutate(muertes_escala = muertes_ventana / max(muertes_ventana))
#   }) %>%
#   filter(FECHA_DEF > fecha_inicio) %>%
#   # filter(FECHA_DEF >= "2020-07-15" & FECHA_DEF < "2020-10-01") %>%
#   ggplot(aes(x = FECHA_DEF, y = muertes_escala, group = edad)) +
#   facet_wrap(~zm) +
#   geom_line(aes(col = edad)) + 
#   # scale_y_log10() +
#   xlab("Fecha de defunción") +
#   ylab("Fallecimientos como proporción del máximo") +
#   AMOR::theme_blackbox()
  



