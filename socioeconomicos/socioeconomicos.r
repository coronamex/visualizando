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

args <- list(indicadores = "../socioeconomicos/coneval/coneval_indicadores_pobreza_municipa_2015.csv",
             serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv.gz",
             cdi_base = "../socioeconomicos/cdi/cdi-base-indicadores-2015.csv",
             dir_estimados = "estimados/",
             dir_salida = "../sitio_hugo/static/imagenes/")

# Leer indicadores
Ind <- read_csv(args$indicadores,
                col_types = cols(clave_entidad = col_character(),
                                 entidad_federativa = col_character(),
                                 clave_municipio = col_character(),
                                 municipio = col_character(),
                                 .default = col_number()),
                na = c("", "NA", "n.d"))
Ind <- Ind %>%
  select(!ends_with("_pob"), -entidad_federativa, - municipio) %>%
  rename(pv = npnv) %>%
  mutate(pv = 100 - pv) %>%
  mutate(mun = str_sub(clave_municipio, start = -3)) %>%
  mutate(clave_entidad = str_pad(string = clave_entidad, width = 2, side = "left", pad = "0")) %>%
  mutate(clave = paste(clave_entidad, mun, sep = "_")) %>%
  select(clave, everything(), -clave_entidad, -clave_municipio, -mun)

# Leer serie
Casos <- read_csv(args$serie_municipios,
                  col_types = cols(municipio = col_character(),
                                   fecha = col_date(format = "%Y-%m-%d"),
                                   clave = col_character())) %>%
  group_by(clave) %>%
  summarise(casos_totales = sum(sintomas_nuevos),
            muertes_totales = sum(muertes_nuevas),
            dia_1 = min(fecha[ sintomas_acumulados >= 1]),
            dia_10 = ifelse(sum(sintomas_nuevos) >= 10,
                            min(fecha[ sintomas_acumulados >= 10]),
                            NA),
            .groups = "drop")
class(Casos$dia_10) <- "Date" # ifelse elimina atributos
Casos <- Casos %>%
  mutate(brote_dias = as.numeric(dia_10 - dia_1))

# Calcular estadísticas y guardar
Casos <- Casos %>%
  left_join(Ind %>%
              select(clave, poblacion), by = "clave") %>%
  mutate(incidencia = 1e5 * (casos_totales / poblacion),
         mortalidad = 1e5 * (muertes_totales / poblacion))
archivo <- file.path(args$dir_estimados, "estadisticas_por_municipios.csv")
write_csv(Casos, archivo)

# Graficar
indicadores <- setdiff(colnames(Ind), c("clave", "poblacion"))
Dat <- Casos %>%
  left_join(Ind %>%
              select(-poblacion), by = "clave") %>%
  filter(muertes_totales >= 10) %>%
  filter(clave != "15_012") %>% # eliminando el outliar de Atizapán
  pivot_longer(cols = indicadores, names_to = "indicador",
               values_to = "percent") %>%
  mutate(percent = percent / 100)

p1 <- Dat %>%
  filter(!is.na(percent)) %>%
  filter(indicador %in% c("ic_asalud",
                          "pobreza",
                          "vul_ing",
                          "vul_car")) %>%
  mutate(indicador = replace(indicador, indicador == "ic_asalud",
                             "Carencia en acceso a salud")) %>%

  mutate(indicador = replace(indicador, indicador == "pobreza",
                             "Situación de pobreza")) %>%
  mutate(indicador = replace(indicador, indicador == "vul_car",
                             "Vulnerables por carencia social")) %>%
  mutate(indicador = replace(indicador, indicador == "vul_ing",
                             "Vulnerables por ingreso")) %>%

  ggplot(aes(x = percent, y = mortalidad)) +
  facet_wrap(~ indicador, scales = "free") +
  # geom_point(aes(size = poblacion), shape = 21, col = "black") +
  geom_point(aes(col = poblacion), shape = 19) +
  geom_smooth(method = "lm", se = TRUE, formula = "y ~ x",
              col = "black", fill = "black") +
  # scale_size(name = "Población\nmunicipal") +
  scale_x_log10(labels = scales::percent) +
  scale_y_log10() +
  scale_color_gradient2(trans = "log10", midpoint = log10(1e5),
                        name = "Población\nmunicipal",
                        labels = scales::comma) +
  xlab(label = "Porcentaje de la población municipal") +
  ylab("Muertes / 100 mil habitantes") +
  theme_classic()
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "coneval_exceso_incidencia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "coneval_exceso_incidencia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


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

Dat <- Casos %>%
  left_join(Cdi, by = "clave")

# Unir pob y casos
p1 <- Dat %>%
  filter(!is.na(prop_indigena)) %>%
  filter(muertes_totales >= 10) %>%
  filter(clave != "15_012") %>% # eliminando el outliar de Atizapán
  filter(poblacion <= 1e5) %>%
  ggplot(aes(x = prop_indigena, y = mortalidad) ) +
  geom_point(aes(col = NOMTIPO), size = 3) +
  scale_color_brewer(palette = "Set1",
                     name = "Grado de\nmarginación") +
  # scale_size(name = "Población\nmunicipal") +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = function(x) scales::percent(x = x, accuracy = 0.1)) +
  scale_y_log10() +
  xlab("Población indígena municipal") +
  ylab(expression(bold(frac("Fallecimientos","100 mil habitantes")))) +
  guides(col = guide_legend(nrow=2, override.aes = list(size = 3)),
         size = guide_legend(nrow=2)) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        plot.margin = margin(l = 20, r = 20),
        legend.key = element_blank(),
        axis.title.y = element_text(size = 16),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),)
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "pob_indigena_mortalidad.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "pob_indigena_mortalidad@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
