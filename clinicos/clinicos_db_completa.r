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

library(tidyverse)
source("util/leer_datos_abiertos.r")

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/",
             fecha_parteaguas = "2021-04-01")

cat("Leer base de datos...\n")
# Lee base de datos
Dat <- leer_datos_abiertos(archivo = args$base_de_datos,
                           solo_confirmados = TRUE,
                           solo_fallecidos = FALSE,
                           solo_laboratorio = FALSE,
                           version = "adivinar")


cat("Tiempo entre síntomas y defunción...\n")
p1 <- Dat %>%
  filter(!is.na(FECHA_DEF)) %>%
  select(FECHA_DEF, FECHA_SINTOMAS) %>%
  mutate(numero_dias = as.numeric(FECHA_DEF - FECHA_SINTOMAS)) %>%
  filter(numero_dias >= 0) %>%
  filter(numero_dias <= 50) %>%
  ggplot(aes(x = numero_dias)) +
  geom_histogram(bins = 15) +
  geom_vline(aes(xintercept = median(numero_dias))) +
  scale_x_continuous(breaks = function(lims){seq(from = 0, to = lims[2], by = 5)}) +
  scale_y_continuous(labels = scales::comma) +
  ylab("Número de defunciones") +
  xlab("Días entre inicio de síntomas y defunción") +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
# p1
archivo <- file.path(args$dir_salida, "tiempo_defuncion.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "tiempo_defuncion@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)  

###########################################
cat("Casos y defunciones por edad...\n")

# Seleccionar fechas y redondear edades
p1 <- Dat %>%
  select(FECHA_SINTOMAS,
         EDAD,
         FECHA_DEF) %>%
  mutate(EDAD = floor(EDAD / 10) * 10) %>%
  mutate(EDAD = replace(EDAD, EDAD >= 70, 70)) %>%  
  
  # Crear tabla por fecha y por grupo de edad
  group_by(FECHA_SINTOMAS, EDAD) %>%
  summarise(Casos = length(EDAD),
            Defunciones = sum(!is.na(FECHA_DEF)),
            .groups = 'drop') %>%
  pivot_longer(cols = c(-FECHA_SINTOMAS, -EDAD),
               values_to = "sintomas_nuevos",
               names_to = "grupo") %>%
  filter(sintomas_nuevos > 0) %>%

  filter(FECHA_SINTOMAS >= "2020-03-01") %>%
  filter(FECHA_SINTOMAS < max(FECHA_SINTOMAS) - 11) %>%
  # arrange(desc(FECHA_SINTOMAS)) %>%
  # print()
  
  # Graficar
  ggplot(aes(x = FECHA_SINTOMAS, y = sintomas_nuevos)) +
  facet_wrap(~ grupo, ncol = 1) +
  geom_bar(aes(fill = as.character(EDAD)), stat = "identity", position = "fill", width = 1) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75)) +
  scale_fill_brewer(palette = "PuOr", name = "Edad",
                    labels = c("0-9 años",
                               "10-19 años",
                               "20-29 años",
                               "30-39 años",
                               "40-49 años",
                               "50-59 años",
                               "60-69 años",
                               "70 ó más años")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") + 
  
  ylab(label = "Porcentaje de pacientes") +
  xlab(label = "Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12))
# p1 
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_def_por_edad.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_def_por_edad@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

#######################################

# # LUT de nombres de variables
# rr_lut <- set_names(c("Habla lengua indígena", "Diabetes", "EPOC", "Asma", "Inmnosupresión",
#                       "Hipertensión", "Otro", "Enfermedad cardiovascular", "Obesidad",
#                       "Insuficiencia renal crónica",
#                       "Tabaquismo", "Embarazo", "Hombre", "10 años más", "Indígena"),
#                     c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
#                       "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
#                       "TABAQUISMO", "EMBARAZO", "SEXO", "EDAD", "INDIGENA"))
# 
# # Eliminar casos recientes
# Dat <- Dat %>%
#   filter(FECHA_SINTOMAS >= args$fecha_parteaguas) %>%
#   filter(FECHA_SINTOMAS < Sys.Date() - 15 | !is.na(FECHA_DEF))
# 
# 
# # Seleccionar datos y convertir variables a indicadores
# d <- Dat %>%
#   mutate(DEF = 1*(!is.na(FECHA_DEF)),
#          TIPO_PACIENTE = replace(TIPO_PACIENTE, TIPO_PACIENTE == "99", NA)) %>%
#   # select(TIPO_PACIENTE) %>% table(useNA = "a")
#   mutate(HOSP = 1*(TIPO_PACIENTE == "2")) %>%
#   select(names(rr_lut), DEF, TIPO_PACIENTE, HOSP, ENTIDAD_UM, SECTOR, ID_REGISTRO) %>%
#   
#   # select(-FECHA_ACTUALIZACION, -FECHA_INGRESO, -FECHA_SINTOMAS, -FECHA_DEF,
#   #        -ORIGEN, -TIPO_PACIENTE,
#   #        -ENTIDAD_NAC, -ENTIDAD_RES, -MIGRANTE, -PAIS_NACIONALIDAD, -PAIS_ORIGEN,
#   #        -INTUBADO, -NEUMONIA, -UCI, -OTRO_CASO, -RESULTADO,
#   #        -MUNICIPIO_RES, -NACIONALIDAD) 
#   pivot_longer(cols = c(-ENTIDAD_UM, -DEF, -HOSP, -EDAD, -SEXO, -EMBARAZO, -SECTOR, -ID_REGISTRO),
#                names_to = "factor_riesgo", values_to = "valor") %>%
#   mutate(SEXO = replace(SEXO, SEXO %in% c("97", "98", "99"), NA),
#          EMBARAZO = replace(EMBARAZO, EMBARAZO %in% c("98", "99"), NA),
#          SECTOR = replace(SECTOR, SECTOR %in% c("99"), NA),
#          valor = replace(valor, valor %in% c("97", "98", "99"), NA)) %>%
#   mutate(SEXO = 1*(SEXO == "2"),
#          EMBARAZO = 1*(EMBARAZO == "1"),
#          valor = 1*(valor == "1")) %>%
#   pivot_wider(id_cols = c(ENTIDAD_UM, DEF, HOSP, EDAD, SEXO, EMBARAZO, SECTOR, ID_REGISTRO),
#               names_from = factor_riesgo, values_from = valor) %>%
#   select(-ID_REGISTRO) %>%
#   drop_na
# 
# d <- d %>%
#   mutate(EDAD = EDAD / 10)
# 
# cat("Calculando riesgos relativos...\n")
# # Sólo efectos fijos
# m1 <- glm(DEF ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG + INDIGENA +
#             DIABETES + EPOC + ASMA + INMUSUPR +
#             HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
#             OBESIDAD + RENAL_CRONICA + TABAQUISMO +
#             ENTIDAD_UM + SECTOR,
#           data = d, family = binomial(link = "logit"))
# # summary(m1)
# m2 <- glm(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +  INDIGENA +
#             DIABETES + EPOC + ASMA + INMUSUPR +
#             HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
#             OBESIDAD + RENAL_CRONICA + TABAQUISMO +
#             ENTIDAD_UM + SECTOR,
#           data = d, family = binomial(link = "logit"))
# # summary(m2)
# 
# p1 <- list(Muerte = m1,
#            `Hospitalización` = m2) %>%
#   map_dfr(broom::tidy, .id = "resultado") %>%
#   filter(term %in% names(rr_lut)) %>%
#   mutate(riesgo_inferior = estimate + qnorm(p = 0.025) * std.error,
#          riesgo_superior = estimate + qnorm(p = 0.975) * std.error) %>%
#   transmute(resultado,
#             factor_riesgo = as.character(rr_lut[term]),
#             riesgo_relativo = exp(estimate),
#             riesgo_inferior = exp(riesgo_inferior),
#             riesgo_superior = exp(riesgo_superior)) %>%
#   arrange(riesgo_inferior) %>%
#   mutate(factor_riesgo = factor(factor_riesgo, levels = unique(factor_riesgo))) %>%
#   
#   # print(n = 100) %>%
#   
#   ggplot(aes(x = riesgo_relativo, y = factor_riesgo, col = resultado)) +
#   geom_vline(xintercept = 1, color = "darkgrey") +
#   geom_errorbarh(aes(xmin = riesgo_inferior, xmax = riesgo_superior), position = position_dodge(width = 1)) +
#   geom_point(position = position_dodge(width = 1)) +
#   scale_color_manual(values = c("#b35806", "#1b9e77"), name = "") +
#   annotate("text",
#            label = "Sin diferencia de riesgo",
#            x = 0.85,
#            y = 11,
#            col = "darkgrey",
#            angle = 90,
#            size = 6) +
#   ylab("Factores de riesgo") +
#   xlab("Riesgo relativo\n(Desde abril 2021)") +
#   guides(color = guide_legend(nrow = 2)) +
#   AMOR::theme_blackbox() +
#   theme(axis.text.x = element_text(angle = 90),
#         panel.background = element_blank(),
#         panel.border = element_rect(fill = NA, color = "black", size = 3),
#         legend.position = "top",
#         legend.text = element_text(size = 14),
#         legend.background = element_blank(),
#         legend.box.background = element_blank(),
#         legend.key = element_blank(),
#         axis.title = element_text(size = 20),
#         axis.text = element_text(size = 10, color = "black"),
#         plot.margin = margin(l = 20, r = 20))
# p1
# # ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
# archivo <- file.path(args$dir_salida, "riesgos_relativos.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "riesgos_relativos@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
