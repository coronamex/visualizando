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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(tidyverse)

args <- list(cdi_base = "../socioeconomicos/cdi/cdi-base-indicadores-2015.csv",
             datos_municipios = "estimados/municipios_obs_esp.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Comunidades indígenas...\n")

# Leer CDI
Cdi <- read_csv(args$cdi_base,
                col_types = cols(INEGI = col_character(),
                                 ENT = col_character(),
                                 NOMENT = col_character(),
                                 MPO = col_character(),
                                 NOMMUN = col_character(),
                                 TIPO2015 = col_character(),
                                 NOMTIPO = col_character()))
stop_for_problems(Cdi)
Cdi <- Cdi %>%
  select(INEGI, ENT, NOMENT, MPO, NOMMUN, `GRADOMARGI 2015`, TPOBTOT, IPOB_INDI, NOMTIPO) %>%
  filter(NOMMUN != "Estados Unidos Mexicanos") %>%
  filter(NOMMUN != "Total Estatal") %>%
  mutate(clave = paste(ENT, MPO, sep = "_")) %>%
  mutate(prop_indigena = IPOB_INDI / TPOBTOT) %>%
  select(-INEGI, -ENT, -NOMENT, -MPO, -NOMMUN,
         -TPOBTOT, -IPOB_INDI,
         gradomargi_2015=`GRADOMARGI 2015`)
# Casos
Casos <- read_csv(args$datos_municipios,
                  col_types = cols(clave = col_character(),
                                   dia_1 = col_date(format = "%Y-%m-%d"),
                                   dia_10 = col_date(format = "%Y-%m-%d"),
                                   .default = col_double()))
# Unir
Dat <- Casos %>%
  left_join(Cdi, by = "clave")

# Unir pob y casos
p1 <- Dat %>%
  filter(!is.na(incidencia)) %>%
  ggplot(aes(x = prop_indigena, y = resid_incidencia) ) +
  geom_point(aes(col = gradomargi_2015), size = 2) +
  scale_color_brewer(palette = "Set1", 
                     name = "Grado de\nmarginación") +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = function(x) scales::percent(x = x, accuracy = 0.1)) +
  scale_y_continuous(labels = function(x){
    labs <- (10 ^ x)
    scales::number(labs, accuracy = 0.1)
  }) +
  xlab("Población indígena municipal") +
  ylab(expression(bold(atop("Exceso", "de casos") == frac("Casos observados en municipio","Casos esperados en municipio")))) +
  # ylab(expression(atop(a,f) == frac(b,c))) +
  guides(col = guide_legend(nrow=2, override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        plot.margin = margin(l = 20, r = 20),
        legend.key = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),)
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "pob_indigena_exceso_incidencia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "pob_indigena_exceso_incidencia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
