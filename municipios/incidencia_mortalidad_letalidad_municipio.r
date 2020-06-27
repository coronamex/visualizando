library(tidyverse)
library(broom)
# library(geojsonio)
source("util/leer_datos_abiertos.r")

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             densidad = "../socioeconomicos/cedrus/DENSIDAD-POB-MUNS-MEXICO.csv",
             indicadores = "../socioeconomicos/coneval/coneval_indicadores_pobreza_municipa_2015.csv",
             serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv",
             min_casos = 0,
             dir_salida = "../sitio_hugo/static/imagenes/",
             dir_estimados = "estimados/")

# Número de pruebas por municipio
n_pruebas <- leer_datos_abiertos(archivo = args$base_de_datos, solo_confirmados = FALSE, solo_fallecidos = FALSE)
n_pruebas <- n_pruebas %>%
  filter(RESULTADO %in% c("1", "2")) %>%
  group_by(ENTIDAD_RES, MUNICIPIO_RES) %>%
  summarise(n_pruebas = length(ID_REGISTRO)) %>%
  ungroup() %>%
  mutate(clave_municipio = paste(ENTIDAD_RES, MUNICIPIO_RES, sep = "")) %>%
  select(clave_municipio, n_pruebas) %>%
  mutate(clave_municipio = str_remove(clave_municipio, "^0"))

# Leer densidad
Dens <- read_csv(args$densidad,
                 col_types = cols(NOM_MUN = col_character(),
                                  CVMUN = col_character(),
                                  .default = col_number()))
Dens <- Dens %>%
  select(CVMUN, DENS15)

# Leer indicadores coneval para obtener poblacion
Ind <- read_csv(args$indicadores,
                col_types = cols(clave_entidad = col_character(),
                                 entidad_federativa = col_character(),
                                 clave_municipio = col_character(),
                                 municipio = col_character(),
                                 .default = col_number()),
                na = c("", "NA", "n.d"))
Ind <- Ind %>%
  select(clave_entidad, clave_municipio, poblacion)

# Leer datos
Casos <- read_csv(args$serie_municipios,
                  col_types = cols(municipio = col_character(),
                                   fecha = col_date(format = "%Y-%m-%d"),
                                   clave = col_character()))
Casos <- Casos %>%
  group_by(clave) %>%
  summarise(casos_totales = sum(sintomas_nuevos),
            muertes_totales = sum(muertes_nuevas),
            dia_1 = min(fecha[ sintomas_acumulados >= 1]),
            dia_10 = min(fecha[ sintomas_acumulados >= 10])) %>%
  filter(casos_totales >= args$min_casos) %>%
  mutate(brote_dias = as.numeric(dia_10 - dia_1))

# Unir densidad, indicadores y pruebas unir datos COVID19 con indicadores
Dat <- Dens %>%
  mutate(CVMUN = str_remove(string = CVMUN, pattern = "^0")) %>%
  rename(clave_municipio = CVMUN) %>%
  full_join(Ind, by = "clave_municipio") %>%
  full_join(n_pruebas, by = "clave_municipio") %>%
  mutate(mun = str_sub(clave_municipio, start = -3)) %>%
  mutate(clave_entidad = str_pad(string = clave_entidad, width = 2, side = "left", pad = "0")) %>%
  mutate(clave = paste(clave_entidad, mun, sep = "_")) %>%
  select(clave, everything(), -clave_entidad, -clave_municipio, -mun) %>%
  right_join(Casos, by = "clave") %>%
  # mutate(incidencia = casos_totales,
  #        mortalidad = muertes_totales,
  #        letalidad = (muertes_totales)/ (casos_totales))
  mutate(incidencia = 1e5 * (casos_totales) / (poblacion),
         mortalidad = 1e5 * (muertes_totales) / (poblacion),
         letalidad = (muertes_totales)/ (casos_totales))
# mutate(incidencia = 1e5 * (casos_totales + 1) / (poblacion + 1),
#        mortalidad = 1e5 * (muertes_totales + 1) / (poblacion + 1),
#        letalidad = (muertes_totales + 1)/ (casos_totales + 1))

# Calcular residuales de datos COVID-19 controlados por población, densidad y pruebas
variables <- c("incidencia", "mortalidad", "letalidad")
Dat <- bind_cols(Dat,
                 variables %>%
                   map_dfc(function(var, dat = Dat){
                     var <- ensym(var)
                     dat_filtrada <- dat %>% filter(!!var > 0)
                     new_var = paste0("resid_", var)
                     new_var2 = paste0("esperados_", var)
                     f1 <- paste("log10(", var, ") ~ log10(poblacion) + log10(DENS15) + log10(n_pruebas)")
                     lm(f1, data = dat_filtrada) %>%
                       augment(newdata = dat) %>%
                       # transmute(!!new_var := !!var - 10^(.fitted))
                       transmute(!!new_var := log10(!!var) - .fitted,
                                 !!new_var2 := 10^(.fitted))
                   }))
# Dat

p1 <- ggplot(Dat, aes(x = poblacion, y = casos_totales)) +
  geom_point(alpha = 0.2) +
  scale_y_log10(labels = function(x) scales::comma(x, accuracy = 1)) +
  scale_x_log10(labels = function(x) scales::comma(x, accuracy = 1)) + 
  ylab("Casos observados en municipio") +
  xlab("Población en municipio") +
  AMOR::theme_blackbox()
# p1

p2 <- ggplot(Dat, aes(x = n_pruebas, y = casos_totales)) +
  geom_point(alpha = 0.2) +
  scale_y_log10(labels = function(x) scales::comma(x, accuracy = 1)) +
  scale_x_log10(labels = function(x) scales::comma(x, accuracy = 1)) + 
  ylab("Casos observados en municipio") +
  xlab("# pruebas a residentes de municipio") +
  AMOR::theme_blackbox()
# p2

p3 <- ggplot(Dat, aes(x = DENS15, y = casos_totales)) +
  geom_point(alpha = 0.2) +
  scale_y_log10(labels = function(x) scales::comma(x, accuracy = 1)) +
  scale_x_log10(labels = function(x) scales::comma(x, accuracy = 1)) + 
  ylab("Casos observados en municipio") +
  xlab("Densidad poblacional en municipio") +
  AMOR::theme_blackbox()
# p3

p4 <- ggplot(Dat, aes(x = esperados_incidencia, y = incidencia)) +
  geom_point(alpha = 0.2) +
  scale_x_log10(labels = function(x) scales::comma(x, accuracy = 1)) +
  scale_y_log10(labels = function(x) scales::comma(x, accuracy = 1)) +
  geom_abline(intercept = 0, slope = 1, col = "darkblue", size = 3) +
  xlab("Casos esperados / 100 mil habitantes") +
  ylab("Casos observados / 100 mil habitantes") +
  # xlab(expression(frac("Casos esperados", "100 mil habitantes"))) +
  # ylab(expression(frac("Casos observados", "100 mil habitantes"))) +
  AMOR::theme_blackbox()
# p4

pp <- cowplot::plot_grid(p1, p2, p3, p4, nrow = 2) +
  theme(plot.margin = margin(l = 20, r = 20))
# pp
# ggsave("test.png", pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "municipios_obs_esp.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "municipios_obs_esp@2x.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)

archivo <- file.path(args$dir_estimados, "municipios_obs_esp.csv")
Dat %>%
  select(clave, dia_1, dia_10, brote_dias,
         incidencia, mortalidad, letalidad,
         resid_incidencia, resid_mortalidad, resid_letalidad) %>%
  write_csv(path = archivo)
