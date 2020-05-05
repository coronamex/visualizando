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
library(deSolve)
source("casos/sir_funciones.r")
args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             estados_lut = "../datos/util/estados_lut_datos_abiertos.csv",
             poblacion = "../datos/demograficos/pob_estado.tsv",
             centinela_official = "../datos/centinela/semana_15/estimados_nacionales.csv",
             semana1_fecha = "2019-12-29" %>% parse_date(format = "%Y-%m-%d"),
             dias_suavizado = 7,
             dias_retraso = 15,
             dias_pronostico_max = 20,
             dir_estimados = "estimados/")


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

########### Rellenar centinela oficial con "factor de corrección" de la SSA

Cen_oficial <- tibble(fecha = min(Dat$FECHA_SINTOMAS) + (0:as.numeric(max(Dat$FECHA_SINTOMAS) - min(Dat$FECHA_SINTOMAS)))) %>%
    left_join(Dat %>%
              mutate(fecha = FECHA_SINTOMAS) %>%
              group_by(fecha) %>%
              summarise(casos_usmer = sum(RESULTADO == "1"))) %>%
  mutate(casos_usmer = replace_na(casos_usmer, 0)) %>%
  arrange(fecha) %>%
  mutate(acumulados_usmer = cumsum(casos_usmer)) %>%
  left_join(Cen_oficial %>%
              mutate(acumulados_usmer_oficial = cumsum(replace_na(positivos_usmer))),
            by = "fecha") %>%
  select(-total_usmer, -posibles_usmer, -estimados_positivos_nacional,
         -totales_nacional, -estimados_posibles_nacional, -pruebas_usmer,
         -positivos_usmer, -acumulados_usmer_oficial) %>%
  mutate(factor_ssa = estimados_acumulados_nacional / acumulados_usmer) %>%
  filter(fecha <= "2020-04-12" ) %>%
  filter(fecha > "2020-01-05") %>%
  # print(n = 1000) %>%
  mutate(factor_ssa_extendido = rep(factor_ssa[!is.na(factor_ssa)], each = 7)) %>%
  # print(n = 1000)
  mutate(acumulados_estimados = floor(factor_ssa_extendido * acumulados_usmer)) %>%
  select(fecha, dia, acumulados_estimados) %>%
  filter(acumulados_estimados > 0 ) %>%
  mutate(casos_estimados = acumulados_estimados - lag(acumulados_estimados, 1, default = 0),
         dia = as.numeric(fecha - min(fecha))) %>%
  print(n = 20)

  
#########

# Estimación rápida por fecha síntomas, estado, (10%/100% de ambulatorio/hospitalizado )
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
    # Bajó siginificativamente a 6.25% en semana quince, o sea que estoy
    # subestimando el número de casos real. Necesito incorporar un factor variable por
    # semana. Tal vez puedo estimarlo del bietín de influenza que parece llegar a semana
    # 16.
    tibble(casos_estimados = floor(pos_usmer_nac / 0.0725))
  }, .id = "fecha") %>%
  mutate(casos_acumulados_estimados = cumsum(casos_estimados),
         fecha = parse_date(fecha, format = "%Y-%m-%d")) 

dat <- Cen %>%
  select(fecha, casos_nuevos = casos_estimados, casos_acumulados = casos_acumulados_estimados) %>%
  mutate(estimado = "CoronaMex") %>%
  bind_rows(Cen_oficial %>%
              select(fecha, casos_nuevos = casos_estimados, casos_acumulados = acumulados_estimados) %>%
              mutate(estimado = "SSA"))

############# SEIR
T_inc <- c(4, 5, 6)
T_inf <- c(2, 3, 4)
# T_inc <- c(4)
# T_inf <- c(2)
pob <- 127792286

# Precalcular dias
Tab <- dat %>% filter(estimado == "SSA") %>%
  filter(casos_acumulados > 0) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))
Tab
fecha_inicio <- min(Tab$fecha)
fecha_final <- Sys.Date()
# n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias <- as.numeric(max(Tab$fecha) - fecha_inicio) + args$dias_pronostico_max
n_dias_ajuste <- min(as.numeric(fecha_final - fecha_inicio) - args$dias_retraso + 1, max(Tab$dia))
fechas_dias <- sort(n_dias_ajuste - seq(from = 15, by = 15, length.out = 2))
# fechas_dias <- sort(n_dias_ajuste - seq(from = 12, by = 10, length.out = 3))

# R_hat_cen_oficial <- encontrar_R_0(real = Tab, n_dias_ajuste = n_dias_ajuste,
#                                    dias_int = fechas_dias,
#                                    T_inc = T_inc, T_inf = T_inf, pob = pob)
load("R_hat_cen_oficial.rdat")
R_hat_cen_oficial

sims_cen_oficial <- simular_multiples_modelos(modelos = R_hat_cen_oficial,
                                  FUN = sir, real = Tab, pob = pob,
                                  n_dias = n_dias)
sims_cen_oficial <- sims_cen_oficial %>%
  mutate(estimado = "SSA", tipo="simulacion",
         fecha = fecha_inicio + dia)

# Precalcular dias
Tab <- dat %>% filter(estimado == "CoronaMex") %>%
  filter(casos_acumulados > 0) %>%
  filter(fecha >= "2020-02-15") %>%
  mutate(dia = as.numeric(fecha - min(fecha)))
fecha_inicio <- min(Tab$fecha)
fecha_final <- Sys.Date()
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- min(n_dias - args$dias_retraso + 1, max(Tab$dia))
fechas_dias <- sort(n_dias_ajuste - seq(from = 10, by = 10, length.out = 5))

R_hat_cen_coronamex <- encontrar_R_0(real = Tab, n_dias_ajuste = n_dias_ajuste,
                                     dias_int = fechas_dias,
                                     T_inc = T_inc, T_inf = T_inf, pob = pob)
R_hat_cen_coronamex
sims_cen_coronames <- simular_multiples_modelos(modelos = R_hat_cen_coronamex,
                                                FUN = sir, real = Tab, pob = pob,
                                                n_dias = n_dias)
sims_cen_coronames <- sims_cen_coronames %>%
  mutate(estimado = "CoronaMex", tipo = "simulacion",
         fecha = fecha_inicio + dia)

# Unir simulaciones
sims <- sims_cen_coronames  %>%
  select(fecha, casos_acumulados, estimado, tipo, modelo) %>%
  bind_rows(sims_cen_oficial %>%
              select(fecha, casos_acumulados, estimado, tipo, modelo))
sims
# sims <- sims_cen_oficial %>%
#               select(fecha, casos_acumulados, estimado, tipo, modelo)
# sims

#############

# Unir centinela y sims
p1 <- dat %>%
  select(fecha, casos_acumulados, estimado) %>%
  mutate(tipo = "centinela",
         modelo = "real") %>%
  bind_rows(sims) %>%
  mutate(grupo = paste(estimado, tipo , modelo, sep = "."),
         grupo_col = paste(estimado, tipo, sep = ".")) %>%
  
  filter(fecha >= "2020-02-15") %>%
  # filter(!(tipo == "simulacion" & estimado == "SSA")) %>%
  
  ggplot(aes(x = fecha, y = casos_acumulados,  group = grupo)) +
  geom_line(aes(col = grupo_col, size = tipo)) +
  scale_color_manual(values = c("#a6cee3", "#1f78b4",
                                "#b2df8a", "#33a02c"),
                       labels = c("Centinela\n(CoronaMex)", "CoronaMex\n+\nSEIR",
                                  "Centinela\n(SSA)", "SSA\n+\nSEIR"),
                     name = "") +
  scale_size_manual(values = c(3, 0.2), guide = FALSE) +
  geom_vline(xintercept = Sys.Date() - 15) +
  annotate("text", label = "Fin ajuste de curva",
           x = Sys.Date() - args$dias_retraso - 1.5,
           y = 200000, angle = 90,
           size = 6) +
  ylab("Casos acumulados estimados") +
  xlab("Fecha de inicio de síntomas") +
  scale_y_continuous(labels = scales::comma,
                     breaks = function(lims){seq(from = 0, to = lims[2], by = 25000)}) +
  # scale_y_log10() +
  guides(color = guide_legend(override.aes = list(size = 3))) +
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
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "sir_nacional_centinela.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional_centinela@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_estimados, "centinela_seir_estimados.csv")
write_csv(p1$data %>%
            select(-grupo, - grupo_col) %>%
            mutate(fecha_estimacion = Sys.Date()), archivo)

########### R_hat

# R_hat <- bind_rows(R_hat_cen_coronamex %>%
#             map_dfr(~ tibble(R0 = c(.x$R_0, .x$R_0 * .x$efectos_int),
#                              dias = c(0, .x$tiempos_int),
#                              modelo = .x$modelo)) %>%
#             mutate(fecha = parse_date("2020-02-15") + dias,
#                    grupo = "CoronaMex") %>%
#             mutate(modelo = paste(modelo, grupo, sep = ".")),
#           R_hat_cen_oficial %>%
#             map_dfr(~ tibble(R0 = c(.x$R_0, .x$R_0 * .x$efectos_int),
#                              dias = c(0, .x$tiempos_int),
#                              modelo = .x$modelo)) %>%
#             mutate(fecha = parse_date("2020-02-17") + dias,
#                    grupo = "SSA") %>%
#             mutate(modelo = paste(modelo, grupo, sep = ".")))
# R_hat
#  
# p1 <- R_hat %>%
#   ggplot(aes(x = fecha, y = R0, group = modelo)) +
#   geom_line(aes(col = grupo), size = 2) +
#   scale_color_manual(values = c("#1f78b4", "#33a02c"),
#                      labels = c("Centinela\n(CoronaMex)", "CoronaMex\n+\nSEIR",
#                                 "Centinela\n(SSA)", "SSA\n+\nSEIR"),
#                      name = "") +
#   ylab("Promedio de infectados por enfermo (R_t)") +
#   xlab("Fecha") +
#   guides(color = guide_legend(override.aes = list(size = 3))) +
#   AMOR::theme_blackbox() +
#   theme(panel.background = element_blank(),
#         panel.border = element_rect(fill = NA, color = "black", size = 3),
#         legend.position = "top",
#         legend.text = element_text(size = 14),
#         legend.key = element_blank(),
#         axis.title = element_text(size = 20),
#         axis.text = element_text(size = 10, color = "black"),
#         plot.margin = margin(l = 20, r = 20))
# p1
# # ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
# archivo <- file.path(args$dir_salida, "centinela_nacional_r0.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "centinela_nacional_r0@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


# simular_multiples_modelos(modelos = R_hat_cen_coronamex,
#                           FUN = sir, real = Tab, pob = pob,
#                           n_dias = 400) %>%
#   mutate(fecha = fecha_inicio + dia) %>%
#   split(.$modelo) %>%
#   map_dfr(function(d){
#     d %>%
#       arrange(fecha) %>%
#       mutate(casos_nuevos = casos_acumulados - lag(casos_acumulados))
#   }) %>%
#   ggplot(aes(x = fecha, y = casos_nuevos, group = modelo)) +
#   geom_line()
