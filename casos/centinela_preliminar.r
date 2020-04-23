library(tidyverse)
args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             estados_lut = "../datos/util/estados_lut_datos_abiertos.csv",
             poblacion = "../datos/demograficos/pob_estado.tsv",
             centinela_official = "../datos/centinela/semana_14/tabla_estimados.csv",
             semana1_fecha = "2019-12-29" %>% parse_date(format = "%Y-%m-%d"),
             dias_suavizado = 7)


# Leer Centinela oficial
Cen_oficial <- read_csv(args$centinela_official,
                        col_types = cols(.default = col_number()))
stop_for_problems(Cen_oficial)
Cen_oficial <- Cen_oficial %>%
  mutate(fecha = args$semana1_fecha + 7*(semana),
         estimados_acumulados_nacional = cumsum(estimados_positivos_nacional)) %>% 
  mutate(dia = as.numeric(fecha - min(fecha))) %>%
  select(-semana)
  # transmute(fecha,
  #           dia = as.numeric(fecha - min(fecha)),
  #           casos_nuevos = estimados_positivos_nacional,
  #           casos_acumulados = estimados_acumulados_nacional)
Cen_oficial

# Leer poblaciones
pob <- read_tsv(args$poblacion,
                col_types = cols(estado = col_character(),
                                 .default = col_number()))
stop_for_problems(pob)
estados_lut <- read_csv(args$estados_lut,
                        col_names = FALSE,
                        col_types = cols(.default = col_character()))
stop_for_problems(estados_lut)
estados_lut <- set_names(estados_lut$X2, estados_lut$X1)

# Leer base de datos ssa
Dat <- read_csv(args$base_de_datos,
                col_types = cols(FECHA_ACTUALIZACION = col_date(format = "%Y-%m-%d"),
                                 FECHA_INGRESO = col_date(format = "%Y-%m-%d"),
                                 FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"),
                                 FECHA_DEF = col_character(),
                                 EDAD = col_number(),
                                 .default = col_character())) 
stop_for_problems(Dat)
Dat <- Dat %>%
  mutate(FECHA_DEF = parse_date(x = FECHA_DEF, format = "%Y-%m-%d", na = c("9999-99-99", "", "NA")),
         PAIS_NACIONALIDAD = parse_character(PAIS_NACIONALIDAD, na = c("99", "", "NA")),
         PAIS_ORIGEN = parse_character(PAIS_ORIGEN, na = c("97", "", "NA")))
Dat

# usmer_props <- Dat %>%
#   split(.$ENTIDAD_UM) %>%
#   map_dfr(function(d){
#     d %>%
#       split(.$FECHA_SINTOMAS) %>%
#       map_dfr(function(d){
#         d <- d %>%
#           filter(RESULTADO != "3") 
#         tibble(positivos_en_usmer = (sum(d$RESULTADO == "1" & d$ORIGEN == "1")) / sum(d$RESULTADO == "1"),
#                positivos_entre_posibles_usmer = (sum(d$RESULTADO == "1" & d$ORIGEN == "1")) / sum(d$ORIGEN == "1"))
#         
#       }, .id = "fecha")
#   }, .id = "estado") %>%
#   filter(!is.na(positivos_en_usmer)) %>%
#   filter(!is.na(positivos_entre_posibles_usmer))
# usmer_props %>%
#   print(n = 100)
# usmer_props %>%
#   filter(estado %in% c("01", "02")) %>%
#   ggplot(aes(x = fecha, y = positivos_entre_posibles_usmer, group = estado)) +
#   geom_line() +
#   geom_smooth()

Cen_oficial %>%
  mutate(prop_usmer = total_usmer / totales_nacional) %>%
  ggplot(aes(x = fecha, y = prop_usmer)) +
  geom_point()

# Seleccionar USMER
Dat <- Dat %>%
  filter(ORIGEN == "1") %>%
  select(ENTIDAD_UM, ENTIDAD_RES,
         MUNICIPIO_RES,
         TIPO_PACIENTE,
         FECHA_SINTOMAS,
         EDAD,
         RESULTADO)
Dat

# Estimación rápida por fecha síntomas, estado, género (10%/100% de ambulatorio/hospitalizado )

# Calcular agregados para estratificación.
# Falta desagregado por edad
Dat <- Dat %>%
  split(.$ENTIDAD_UM) %>%
  map_dfr(function(d){
    d %>%
      split(.$FECHA_SINTOMAS)  %>%
      map_dfr(function(d){
        tibble(positivos_leves = sum(d$TIPO_PACIENTE == "1" & d$RESULTADO == "1"),
               positivos_graves = sum(d$TIPO_PACIENTE == "2" & d$RESULTADO == "1"),
               negativos_leves = sum(d$TIPO_PACIENTE == "1" & d$RESULTADO == "2"),
               negativos_graves = sum(d$TIPO_PACIENTE == "2" & d$RESULTADO == "2"),
               sospechosos_leves = sum(d$TIPO_PACIENTE == "1" & d$RESULTADO == "3"),
               sospechosos_graves = sum(d$TIPO_PACIENTE == "2" & d$RESULTADO == "3"),
               n_pacientes = nrow(d))
      }, .id = 'fecha')
  }, .id = "estado") %>%
  mutate(fecha = parse_date(fecha, format = "%Y-%m-%d")) %>%
  arrange(estado, fecha)
Dat

# Suavizar
roll_mean <- tibbletime::rollify(function(x){
  x[is.na(x)] <- 0
  mean(x)
}, window = args$dias_suavizado, na_value = 0)
Dat <- Dat %>%
  split(.$estado) %>%
  map_dfr(function(d){
    region <- unique(d$estado)
    n_dias <- as.numeric(max(d$fecha) - min(d$fecha))
    
    tibble(fecha = (min(d$fecha) - args$dias_suavizado) + 0:(n_dias + args$dias_suavizado)) %>%
      left_join(d, by = "fecha") %>%
      mutate(estado = replace_na(estado, region)) %>%
      mutate(positivos_leves = roll_mean(positivos_leves),
             positivos_graves = roll_mean(positivos_graves),
             negativos_leves = roll_mean(negativos_leves),
             negativos_graves = roll_mean(negativos_graves),
             sospechosos_leves = roll_mean(sospechosos_leves),
             sospechosos_graves = roll_mean(sospechosos_graves),
             n_pacientes = roll_mean(n_pacientes)) %>%
      mutate(fecha = fecha - floor(args$dias_suavizado / 2)) %>%
      filter(n_pacientes > 0)
  })

# Estimados por fecha
Cen <- Dat %>%
  mutate(positividad_leves = positivos_leves / (positivos_leves + negativos_leves + 1),
         positividad_graves = positivos_graves / (positivos_graves + negativos_graves + 1)) %>%
  mutate(graves_estimados = positivos_graves + (sospechosos_graves * positividad_graves),
         leves_estimados = (positivos_leves + (sospechosos_leves * positividad_leves))) %>%
  select(fecha, estado, positividad_leves, positividad_graves, graves_estimados, leves_estimados) %>%
  mutate(positivos_estimados = graves_estimados + leves_estimados) %>%
  filter(positivos_estimados > 0)

# Normalizar por población y muestreo
Cen <- Cen %>%
  mutate(estado = as.vector(estados_lut[estado])) %>%
  left_join(pob %>%
              transmute(estado, pob = conapo_2020 / sum(conapo_2020)),
            by = "estado") %>%
  split(.$fecha) %>%
  map_dfr(function(d){
    pos_usmer_nac <- sum(d$positivos_estimados * d$pob) * 32
    # Alrededor de 7.5% casos posibles van a usmer a nivel nacional de manera 
    # consistente. Bajó como a 7 en últimas 2 semanas. Uso 7.25%
    tibble(casos_estimados = floor(pos_usmer_nac / 0.0725))
  }, .id = "fecha") %>%
  mutate(casos_acumulados_estimados = cumsum(casos_estimados),
         fecha = parse_date(fecha, format = "%Y-%m-%d")) 

dat <- Cen %>%
  select(fecha, casos_nuevos = casos_estimados, casos_acumulados = casos_acumulados_estimados) %>%
  mutate(estimado = "CoronaMex") %>%
  bind_rows(Cen_oficial %>%
              select(fecha, casos_nuevos = estimados_positivos_nacional, casos_acumulados = estimados_acumulados_nacional) %>%
              mutate(estimado = "SSA"))

p1 <- dat %>%
  filter(fecha >= "2020-02-15") %>%
  
  ggplot(aes(x = fecha, y = casos_acumulados,  group = estimado)) +
  geom_line(aes(col = estimado), size = 3) +
  scale_color_manual(values = c("#a6cee3", "#b2df8a"),
                       labels = c("Centinela\n(CoronaMex)", "Centinela\n(SSA)"), name = "") +
  geom_vline(xintercept = Sys.Date() - 15) +
  ylab("Casos acumulados estimados") +
  xlab("Fecha de inicio de síntomas") +
  scale_y_continuous(labels = scales::comma) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "sir_nacional_centinela.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional_centinela@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)



