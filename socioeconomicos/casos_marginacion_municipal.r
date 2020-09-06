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
             serie_tiempo = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Marginacion casos...\n")

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
  select(ENT, NOMENT, MPO, NOMMUN, `GRADOMARGI 2015`, TPOBTOT) %>%
  filter(NOMMUN != "Estados Unidos Mexicanos") %>%
  filter(NOMMUN != "Total Estatal") %>%
  mutate(clave = paste(ENT, MPO, sep = "_")) %>%
  select(clave, marginacion_2015 = `GRADOMARGI 2015`, TPOBTOT) %>%
  mutate(marginacion_2015 = factor(marginacion_2015,
                                   levels = c("Muy bajo",
                                              "Bajo",
                                              "Medio",
                                              "Alto",
                                              "Muy alto")))
# Cdi

p1 <- Cdi %>%
  group_by(marginacion_2015) %>%
  summarise(pob = sum(TPOBTOT),
            .groups = "drop") %>%
  mutate(prop = pob / sum(pob)) %>%
  ggplot(aes(x = 1, y = pob)) +
  geom_bar(aes(fill = marginacion_2015), stat = "identity", position = "fill", width = 1) +
  scale_fill_brewer(palette = "RdPu") +
  guides(fill = FALSE) +
  ylab(label = "Porcentaje de población") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.line.y.right = element_line(),
        plot.margin = margin(l = 0, r = 20))
# p1


serie <- read_csv(args$serie_tiempo,
                  col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                   municipio = col_character(),
                                   clave = col_character(),
                                   .default = col_number()))
p2 <- serie %>%
  select(municipio, fecha, sintomas_nuevos, muertes_nuevas, clave) %>%
  left_join(Cdi %>%
              select(-TPOBTOT),
            by = "clave") %>%
  
  filter(fecha >= "2020-03-01") %>%
  filter(fecha < max(fecha) - 11) %>%
  
  split(.$fecha) %>%
  map_dfr(function(d){
    d <- d %>%
      filter(sintomas_nuevos > 0)
    
    casos <- rep(d$marginacion_2015, times = d$sintomas_nuevos)
    casos <- table(casos)
    
    muertes <- rep(d$marginacion_2015, times = d$muertes_nuevas)
    muertes <- table(muertes)
    
    levs <- names(casos)

    tibble(casos = as.numeric(casos),
           muertes = as.numeric(muertes),
           marginacion_2015 = factor(levs, levels = levs))
    
  }, .id = "fecha") %>%
  mutate(fecha = parse_date(fecha, format = "%Y-%m-%d")) %>%
  
  ggplot(aes(x = fecha, y = casos)) +
  geom_bar(aes(fill = marginacion_2015), stat = "identity", position = "fill", width = 1) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75)) +
  scale_fill_brewer(palette = "RdPu", name = "Grado de marginación\nmunicipal") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 3)) +
  ylab(label = "Porcentaje de casos confirmados") +
  xlab(label = "Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 0),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(face = "bold", size = 18),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12))
# p2

pp <- cowplot::plot_grid(p2, p1 +
                           aplot::ylim2(p2) +
                           scale_y_continuous(labels = scales::percent,
                                              position = "right"),
                         rel_widths = c(0.80, 0.20),
                         axis = "bt", align = "h")
# pp
# ggsave("test.png", pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_margmun.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_margmun@2x.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)
