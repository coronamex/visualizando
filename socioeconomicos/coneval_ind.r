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
library(broom)
library(geojsonio)

#' Title
#'
#' @param expresion 
#' @param Dat 
#'
#' @return
#' @export
#'
#' @examples
modelar <- function(expresion, Dat){
  expresion <- enexpr(expresion)
  
  Dat %>%
    nest(dat = -indicador) %>%
    mutate(modelo = dat %>%
             map(function(d){
               d_filtrado <- d %>%
                 mutate(y := !!expresion) %>%
                 filter(!is.infinite(y)) %>%
                 filter(valor > 0)
               f1 <- "y ~ log10(valor)"
               lm(f1, data = d_filtrado) %>% tidy %>%
                 filter(term != "(Intercept)") %>%
                 select(-term) %>%
                 mutate(expresion = deparse(expresion),
                        n_mun = nrow(d_filtrado))
               
             })) %>%
    unnest(modelo) %>%
    select(-dat) %>%
    mutate(q.value = p.adjust(p.value, 'fdr')) %>%
    arrange(p.value)
}

args <- list(indicadores = "../socioeconomicos/coneval/coneval_indicadores_pobreza_municipa_2015.csv",
             mapa_shp = "../socioeconomicos/mapas/municipalities.shp",
             datos_municipios = "estimados/municipios_obs_esp.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

# Leer mapa
mun_sp <- geojson_read(args$mapa_shp, what = "sp")
mun_sp@data$clave <- paste(mun_sp@data$CVE_ENT, mun_sp@data$CVE_MUN, sep = "_")
mun_sp <- tidy(mun_sp, region = "clave")

# Leer indicadores
Ind <- read_csv(args$indicadores,
                col_types = cols(clave_entidad = col_character(),
                                 entidad_federativa = col_character(),
                                 clave_municipio = col_character(),
                                 municipio = col_character(),
                                 .default = col_number()),
                na = c("", "NA", "n.d"))
Ind <- Ind %>%
  select(!ends_with("_pob"), -entidad_federativa, - municipio, -poblacion) %>%
  rename(pv = npnv) %>%
  mutate(pv = 100 - pv) %>% 
  mutate(mun = str_sub(clave_municipio, start = -3)) %>%
  mutate(clave_entidad = str_pad(string = clave_entidad, width = 2, side = "left", pad = "0")) %>%
  mutate(clave = paste(clave_entidad, mun, sep = "_")) %>%
  select(clave, everything(), -clave_entidad, -clave_municipio, -mun)

# Leer datos
Casos <- read_csv(args$datos_municipios)

# Unir datos e indicadores
Dat <- Casos %>%
  left_join(Ind, by = "clave")

# Graficar mapa exceso de casos
mun_sp <- mun_sp %>%
  left_join(Dat %>%
              select(clave, resid_incidencia),
            by = c(id = "clave"))
p1 <- mun_sp %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = resid_incidencia), color = NA) +
  scale_fill_gradient2(low = "#053061", high = "#67001f", mid = "#f7f7f7", midpoint = 0,
                       name = expression(frac("Casos observados en municipio",
                                              "Casos esperados en municipio")),
                       labels = function(x){
                         labs <- (10 ^ x)
                         scales::number(labs, accuracy = 0.1)
                       }) +
  theme_void() +
  coord_map() +
  theme(legend.position = "top",
        plot.margin = margin(t = 0, r = 0 , b = 0, l = 0))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_incidencia_mapa.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "exceso_incidencia_mapa@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


# Limpiar para análisis de indicadores
Dat <- Dat %>%
  pivot_longer(cols = c(-clave,
                        -dia_1, -dia_10, -brote_dias,
                        -incidencia, -mortalidad, -letalidad,
                        -resid_incidencia, -resid_mortalidad, -resid_letalidad),
               names_to = "indicador", values_to = "valor")

# indicadores_para_graficar <- c("pobreza", "ic_asalud", "ic_cv", "ic_segsoc")
indicadores_para_graficar <- c(pobreza_e = "Pobreza extrema",
                               vul_ing = "Ingreso menor a línea de bienestar",
                               ic_cv = "Vulnerabale por calidad y espacios de la vivienda",
                               carencias3 = "3 ó más carencias sociales")
# indicadores_para_graficar <- unique(Dat$indicador)
p1 <- Dat %>%
  filter(indicador %in% names(indicadores_para_graficar)) %>%
  mutate(indicador = as.vector(indicadores_para_graficar[indicador])) %>%
  ggplot(aes(x = valor, y = resid_incidencia)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point(size = 2, alpha = 0.1) +
  stat_smooth(method = "lm") +
  scale_x_log10(labels = function(x) scales::percent(x / 100, accuracy = 1)) +
  scale_y_continuous(labels = function(x){
    labs <- (10 ^ x)
    scales::number(labs, accuracy = 0.2)
  }) +
  xlab(label = "% de población municipal") +
  # ylab(expression(frac("Casos observados en municipio","Casos esperados en municipio"))) +
  ylab(expression(atop("Exceso", "de casos") == frac("Casos observados en municipio","Casos esperados en municipio"))) +
  theme_classic() +
  theme(legend.position = "top")
# axis.text.x = element_text(angle = 90))
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "coneval_exceso_incidencia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "coneval_exceso_incidencia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Dat %>%
#   filter(indicador %in% indicadores_para_graficar) %>%
#   ggplot(aes(x = valor, y = resid_mortalidad)) +
#   facet_wrap(~ indicador, scales = "free_x") +
#   geom_point(aes(col = poblacion / 1e4), size = 0.5) +
#   scale_color_gradient(low = "#f6e8c3", high = "#543005", trans = "log10",
#                        name = "Población\n(decenas de miles)", labels = scales::comma) +
#   stat_smooth(method = "lm") +
#   scale_x_log10(labels = function(x) scales::percent(x / 100)) +
#   scale_y_continuous(labels = function(x){
#     labs <- (10 ^ x)
#     scales::number(labs, accuracy = 0.01)
#   }) +
#   theme_classic() +
#   theme(legend.position = "top",
#         axis.text.x = element_text(angle = 90))



# Dat %>%
#   ggplot(aes(x = valor, y = resid_letalidad)) +
#   facet_wrap(~ indicador, scales = "free_x") +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   scale_x_log10() +
#   scale_x_log10(labels = function(x) scales::percent(x / 100)) +
#   scale_y_continuous(labels = function(x){
#     labs <- (10 ^ x) - 1
#     scales::percent(labs)
#   })

# Dat %>%
#   ggplot(aes(x = valor, y = resid_mortalidad - resid_incidencia)) +
#   facet_wrap(~ indicador, scales = "free_x") +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   scale_x_log10(labels = function(x) scales::percent(x / 100)) +
#   scale_y_continuous(labels = function(x){
#     labs <- (10 ^ x) - 1
#     scales::percent(labs)
#   })

# Dat %>%
#   ggplot(aes(x = valor, y = resid_letalidad - resid_mortalidad)) +
#   facet_wrap(~ indicador, scales = "free_x") +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   scale_x_log10(labels = function(x) scales::percent(x / 100))
#   # scale_y_continuous(labels = function(x){
#   #   labs <- (10 ^ x) - 1
#   #   scales::percent(labs)
#   # })

# Dat %>%
#   ggplot(aes(x = valor, y = resid_letalidad - resid_incidencia)) +
#   facet_wrap(~ indicador, scales = "free_x") +
#   geom_point() +
#   stat_smooth(method = "lm") +
#   scale_x_log10(labels = function(x) scales::percent(x / 100)) +
#   scale_y_continuous(labels = function(x){
#     labs <- (10 ^ x) - 1
#     scales::percent(labs)
#   })


# modelar(resid_incidencia, Dat = Dat)
# modelar(resid_mortalidad, Dat = Dat)
# modelar(resid_letalidad, Dat = Dat)
# modelar(resid_mortalidad - resid_incidencia, Dat = Dat)
# modelar(resid_letalidad - resid_mortalidad, Dat = Dat)
# modelar(resid_letalidad - resid_incidencia, Dat = Dat)

################