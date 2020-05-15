library(tidyverse)

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             densidad = "../socioeconomicos/cedrus/DENSIDAD-POB-MUNS-MEXICO.csv",
             indicadores = "../socioeconomicos/coneval/coneval_indicadores_pobreza_municipa_2015.csv",
             serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv")

# Leer densidad
Dens <- read_csv(args$densidad,
                 col_types = cols(NOM_MUN = col_character(),
                                                 CVMUN = col_character(),
                                                 .default = col_number()))
Dens <- Dens %>%
  select(NOM_MUN, CVMUN, POBTOT15, AREAKM, DENS15)
Dens


# Leer indicadores
Ind <- read_csv(args$indicadores,
                col_types = cols(clave_entidad = col_character(),
                                 entidad_federativa = col_character(),
                                 clave_municipio = col_character(),
                                 municipio = col_character(),
                                 .default = col_number()),
                na = c("", "NA", "n.d"))
Ind <- Ind %>%
  select(!ends_with("_pob"))

# Unir densidad e indicadores
Dat <- Dens %>%
  mutate(CVMUN = str_remove(string = CVMUN, pattern = "^0")) %>%
  rename(clave_municipio = CVMUN) %>%
  full_join(Ind, by = "clave_municipio") %>%
  select(-POBTOT15, -AREAKM,
         -NOM_MUN, -municipio, -entidad_federativa) %>%
  mutate(mun = str_sub(clave_municipio, start = -3)) %>%
  mutate(clave_entidad = str_pad(string = clave_entidad, width = 2, side = "left", pad = "0")) %>%
  mutate(clave = paste(clave_entidad, mun, sep = "_")) %>%
  select(clave, everything(), -clave_entidad, -clave_municipio, -mun)
Dat

# Leer datos
Serie <- read_csv(args$serie_municipios)
Casos <- Serie %>%
  group_by(clave) %>%
  summarise(casos_totales = sum(sintomas_nuevos),
            muertes_totales = sum(muertes_nuevas),
            dia_1 = min(fecha[ sintomas_acumulados >= 1]),
            dia_10 = min(fecha[ sintomas_acumulados >= 10])) %>%
  filter(casos_totales >= 10) %>%
  mutate(brote_dias = as.numeric(dia_10 - dia_1))
Casos


Dat <- Casos %>%
  left_join(Dat, by = "clave") %>%
  mutate(incidencia = 1e5 * (casos_totales + 1) / poblacion,
         mortalidad = 1e5 * (muertes_totales + 1) / poblacion,
         letalidad = (muertes_totales + 1)/ (casos_totales + 1)) %>%
  pivot_longer(cols = c(-clave, -casos_totales, -muertes_totales,
                        -dia_1, -dia_10, -brote_dias,
                        -incidencia, -mortalidad, -letalidad),
               names_to = "indicador", values_to = "valor")
Dat


Dat %>%
  ggplot(aes(x = valor, y = mortalidad)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point() +
  scale_y_log10()


Dat %>%
  nest(-indicador) %>%
  mutate(tidy = map(data, ~ broom::tidy(lm(log10(letalidad) ~ valor, data = .x)))) %>%
  unnest(tidy) %>%
  filter(term != "(Intercept)") %>%
  select(-data) %>%
  arrange(p.value)
  