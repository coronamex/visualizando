library(tidyverse)
library(broom)
library(geojsonio)
source("util/leer_datos_abiertos.r")

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


args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             densidad = "../socioeconomicos/cedrus/DENSIDAD-POB-MUNS-MEXICO.csv",
             indicadores = "../socioeconomicos/coneval/coneval_indicadores_pobreza_municipa_2015.csv",
             serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv",
             mapa_shp = "../socioeconomicos/mapas/municipalities.shp",
             min_casos = 0)

# Leer mapa
mun_sp <- geojson_read(args$mapa_shp, what = "sp")
mun_sp@data$clave <- paste(mun_sp@data$CVE_ENT, mun_sp@data$CVE_MUN, sep = "_")

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
  select(NOM_MUN, CVMUN, POBTOT15, AREAKM, DENS15)

# Leer indicadores
Ind <- read_csv(args$indicadores,
                col_types = cols(clave_entidad = col_character(),
                                 entidad_federativa = col_character(),
                                 clave_municipio = col_character(),
                                 municipio = col_character(),
                                 .default = col_number()),
                na = c("", "NA", "n.d"))
Ind <- Ind %>%
  select(!ends_with("_pob")) %>%
  rename(pv = npnv) %>%
  mutate(pv = 100 - pv)

# Leer datos
Casos <- read_csv(args$serie_municipios)
Casos <- Casos %>%
  group_by(clave) %>%
  summarise(casos_totales = sum(sintomas_nuevos),
            muertes_totales = sum(muertes_nuevas),
            dia_1 = min(fecha[ sintomas_acumulados >= 1]),
            dia_10 = min(fecha[ sintomas_acumulados >= 10])) %>%
  filter(casos_totales >= args$min_casos) %>%
  mutate(brote_dias = as.numeric(dia_10 - dia_1))

# Unir densidad, indicadores y pruebas
Dat <- Dens %>%
  mutate(CVMUN = str_remove(string = CVMUN, pattern = "^0")) %>%
  rename(clave_municipio = CVMUN) %>%
  full_join(Ind, by = "clave_municipio") %>%
  full_join(n_pruebas, by = "clave_municipio") %>%
  select(-POBTOT15, -AREAKM,
         -NOM_MUN, -municipio, -entidad_federativa) %>%
  mutate(mun = str_sub(clave_municipio, start = -3)) %>%
  mutate(clave_entidad = str_pad(string = clave_entidad, width = 2, side = "left", pad = "0")) %>%
  mutate(clave = paste(clave_entidad, mun, sep = "_")) %>%
  select(clave, everything(), -clave_entidad, -clave_municipio, -mun)

# Unir datos COVID19 con indicadores
Dat <- Casos %>%
  left_join(Dat, by = "clave") %>%
  # mutate(incidencia = casos_totales,
  #        mortalidad = muertes_totales,
  #        letalidad = (muertes_totales)/ (casos_totales))
  mutate(incidencia = 1e5 * (casos_totales) / (poblacion),
         mortalidad = 1e5 * (muertes_totales) / (poblacion),
         letalidad = (muertes_totales)/ (casos_totales))
  
# mutate(incidencia = 1e5 * (casos_totales + 1) / (poblacion + 1),
  #        mortalidad = 1e5 * (muertes_totales + 1) / (poblacion + 1),
  #        letalidad = (muertes_totales + 1)/ (casos_totales + 1))
# Dat

p1 <- Dat %>%
  select(clave, casos_totales, poblacion, DENS15, n_pruebas) %>%
  pivot_longer(cols = c(-clave, -casos_totales),
               names_to = "indicador", values_to = "valor") %>%
  # select(clave, brote_dias, poblacion, DENS15, n_pruebas) %>%
  # pivot_longer(cols = c(-clave, -brote_dias),
  #              names_to = "indicador", values_to = "valor") %>%
  mutate(indicador = replace(indicador, indicador == "poblacion", "Población total")) %>%
  mutate(indicador = replace(indicador, indicador == "DENS15", "Densidad poblacional")) %>%
  mutate(indicador = replace(indicador, indicador == "n_pruebas", "Pruebas totales")) %>%
  ggplot(aes(x = valor, y = casos_totales)) +
  facet_wrap(~ indicador, scales = "free_x", nrow = 2) +
  geom_point() +
  scale_x_log10(labels = function(x) scales::comma(x, accuracy = 1)) +
  scale_y_log10(labels = function(x) scales::comma(x, accuracy = 1)) +
  ylab(label = "Casos totales por municipio") +
  # AMOR::theme_blackbox() +
  theme_classic() +
  theme(axis.title = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        # panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        strip.text = element_text(face = "bold"))
p1
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)

# Calcular residuales de datos COVID-19 controlados por población
variables <- c("incidencia", "mortalidad", "letalidad")
Dat <- bind_cols(Dat,
          variables %>%
            map_dfc(function(var, dat = Dat){
              var <- ensym(var)
              dat_filtrada <- dat %>% filter(!!var > 0)
              new_var = paste0("resid_", var)
              f1 <- paste("log10(", var, ") ~ log10(poblacion) + log10(DENS15) + log10(n_pruebas)")
              lm(f1, data = dat_filtrada) %>%
                augment(newdata = dat) %>%
                # transmute(!!new_var := !!var - 10^(.fitted))
                transmute(!!new_var := log10(!!var) - .fitted)
            }))
# summary(Dat$resid_mortalidad)
# summary(Dat$resid_letalidad)
# summary(Dat$resid_incidencia)
# hist(Dat$resid_mortalidad)
# hist(Dat$resid_letalidad)
# hist(Dat$resid_incidencia)
Dat
# Mapas
mun_sp <- tidy(mun_sp, region = "clave") %>%
  left_join(Dat %>%
              select(clave, casos_totales, muertes_totales, brote_dias,
                     resid_incidencia, resid_mortalidad, resid_letalidad),
            by = c(id = "clave"))
mun_sp
p1 <- mun_sp %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = resid_incidencia), color = NA) +
  # geom_polygon(aes(x = long, y = lat, group = group, fill = brote_dias), color = NA) +
  scale_fill_gradient2(low = "#053061", high = "#67001f", mid = "#f7f7f7", midpoint = 0,
                       name = expression(frac("Casos observados en municipio",
                                              "Casos esperados en municipio")),
                       labels = function(x){
                         labs <- (10 ^ x)
                         scales::number(labs, accuracy = 0.1)
                       }) +
  # scale_fill_gradient(low = "#f7f4f9", high = "#67001f") +
  theme_void() +
  coord_map() +
  theme(legend.position = "top",
        plot.margin = margin(t = 0, r = 0 , b = 0, l = 0))
# p1
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)



# Limpiar para análisis de indicadores
Dat <- Dat %>%
  pivot_longer(cols = c(-clave, -casos_totales, -muertes_totales,
                        -dia_1, -dia_10, -brote_dias,
                        -incidencia, -mortalidad, -letalidad,
                        -poblacion, -DENS15, -n_pruebas,
                        -resid_incidencia, -resid_mortalidad, -resid_letalidad),
               names_to = "indicador", values_to = "valor")
Dat

indicadores_para_graficar <- c("pobreza", "ic_asalud", "ic_cv", "ic_segsoc")
indicadores_para_graficar <- c("pobreza_e", "vul_ing", "ic_cv", "carencias3")
# indicadores_para_graficar <- unique(Dat$indicador)
p1 <- Dat %>%
  filter(indicador %in% indicadores_para_graficar) %>%
  ggplot(aes(x = valor, y = resid_incidencia)) +
  facet_wrap(~ indicador, scales = "free_x") +
  geom_point(aes(col = poblacion / 1e4), size = 2) +
  scale_color_gradient(low = "#f6e8c3", high = "#543005", trans = "log10",
                       name = "Población\n(decenas de miles)", labels = scales::comma) +
  # scale_radius(trans = "log2") +
  stat_smooth(method = "lm") +
  scale_x_log10(labels = function(x) scales::percent(x / 100)) +
  # scale_x_continuous(labels = function(x) scales::percent(x / 100)) +
  # scale_y_log10() +
  scale_y_continuous(labels = function(x){
    # labs <- (10 ^ x) - 1
    # scales::percent(labs)
    labs <- (10 ^ x)
    scales::number(labs, accuracy = 0.01)
  }) +
  ylab(expression(frac("Casos observados en municipio","Casos esperados en municipio"))) +
  # coord_cartesian() +
  theme_classic() +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 90))
p1
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "inicio_sintomas_por_fecha_nacional.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "inicio_sintomas_por_fecha_nacional@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
 
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


modelar(resid_incidencia, Dat = Dat)
modelar(resid_mortalidad, Dat = Dat)
modelar(resid_letalidad, Dat = Dat)
modelar(resid_mortalidad - resid_incidencia, Dat = Dat)
modelar(resid_letalidad - resid_mortalidad, Dat = Dat)
modelar(resid_letalidad - resid_incidencia, Dat = Dat)

################










