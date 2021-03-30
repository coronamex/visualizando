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

args <- list(mapa_shp = "../socioeconomicos/mapas/municipalities.shp",
             mapa_topojson = "../socioeconomicos/mapas/mx_tj.json",
             datos_municipios = "estimados/estadisticas_por_municipios.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Mapa mortalidad municipal...\n")

# Leer mapa
mun_sp <- rgdal::readOGR(args$mapa_topojson, layer = "municipalities")
mun_sp@data <- mun_sp@data %>%
  mutate(clave = paste(str_pad(state_code, width = 2, side = "left", pad = "0"),
                       str_pad(mun_code, width = 3, side = "left", pad = "0"),
                       sep = "_"))
mun_sp <- mun_sp %>% fortify(region = "clave")  %>%
  rename(clave = id)


Dat <- read_csv(args$datos_municipios,
                col_types = cols(clave = col_character(),
                                 dia_1 = col_date(format = "%Y-%m-%d"),
                                 dia_10 = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))

# Graficar mapa mortalidad municipal
p1 <- mun_sp %>%
  left_join(Dat %>%
              filter(clave != "15_012") %>% # Eliminando Atizapán (outliar)
              select(clave, mortalidad),
            by = "clave") %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mortalidad), color = NA) +
  scale_fill_gradient(low = "#feebe2", high = "#7a0177",
                      name = expression(frac("Fallecimientos",
                                             "100 mil habitantes"))) +
  theme_void() +
  coord_map() +
  theme(legend.position = "top",
        plot.margin = margin(t = 0, r = 0 , b = 0, l = 0))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "mapa_mortalidad_municipal.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "mapa_mortalidad_municipal@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


