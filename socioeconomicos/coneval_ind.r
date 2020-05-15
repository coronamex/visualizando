library(tidyverse)
library(broom)

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
Ind

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

# Unir datos COVID19 con indicadores
Dat <- Casos %>%
  left_join(Dat, by = "clave") %>%
  mutate(incidencia = 1e5 * (casos_totales) / (poblacion),
         mortalidad = 1e5 * (muertes_totales) / (poblacion),
         letalidad = (muertes_totales)/ (casos_totales))
  # mutate(incidencia = 1e5 * (casos_totales + 1) / (poblacion + 1),
  #        mortalidad = 1e5 * (muertes_totales + 1) / (poblacion + 1),
  #        letalidad = (muertes_totales + 1)/ (casos_totales + 1))

Dat


# Calcular residuales de datos COVID-19 controlados por población
variables <- c("incidencia", "mortalidad", "letalidad")
Dat <- bind_cols(Dat,
                 variables %>%
                   map_dfc(function(var, dat = Dat){
                     var <- ensym(var)
                     dat_filtrada <- dat %>% filter(!!var > 0)
                     new_var = paste0("resid_", var)
                     f1 <- paste("log10(", var, ") ~ log10(poblacion) + log10(DENS15)")
                     lm(f1, data = dat_filtrada) %>%
                       augment(newdata = dat) %>%
                       transmute(!!new_var := log10(!!var) - .fitted)
                   }))
Dat

# Limpiar para análisis de indicadores
Dat <- Dat %>%
  pivot_longer(cols = c(-clave, -casos_totales, -muertes_totales,
                        -dia_1, -dia_10, -brote_dias,
                        -incidencia, -mortalidad, -letalidad,
                        -poblacion, -DENS15,
                        -resid_incidencia, -resid_mortalidad, -resid_letalidad),
               names_to = "indicador", values_to = "valor")
Dat


Dat %>%
  ggplot(aes(x = valor, y = resid_incidencia)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10()
 
Dat %>%
  ggplot(aes(x = valor, y = resid_mortalidad)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10()

Dat %>%
  ggplot(aes(x = valor, y = resid_letalidad)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10()


Dat %>%
  ggplot(aes(x = valor, y = resid_mortalidad - resid_incidencia)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10()

Dat %>%
  ggplot(aes(x = valor, y = resid_letalidad - resid_mortalidad)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point() +
  stat_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_continuous(labels = function(x){
    labs <- (10 ^ x) - 1
    scales::percent(labs)
  })


modelar(resid_incidencia, Dat = Dat)
modelar(resid_mortalidad, Dat = Dat)
modelar(resid_letalidad, Dat = Dat)
modelar(resid_mortalidad - resid_incidencia, Dat = Dat)
modelar(resid_letalidad - resid_mortalidad, Dat = Dat)

