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
library(tidyverse)
library(brms)
source("util/leer_datos_abiertos.r")

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/",
             fecha_inicio = "2021-02-01")

cat("Leer base de datos...\n")
# Lee base de datos
Dat <- leer_datos_abiertos(archivo = args$base_de_datos,
                           solo_confirmados = TRUE,
                           solo_fallecidos = FALSE,
                           solo_laboratorio = FALSE,
                           version = "adivinar")
cat("RR por edad y mes...\n")

# LUT de nombres de variables
rr_lut <- set_names(c("Habla lengua indígena", "Diabetes", "EPOC", "Asma", "Inmnosupresión",
                      "Hipertensión", "Otro", "Enfermedad cardiovascular", "Obesidad",
                      "Insuficiencia renal crónica",
                      "Tabaquismo", "Embarazo", "Hombre", "10 años más", "Indígena"),
                    c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
                      "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
                      "TABAQUISMO", "EMBARAZO", "SEXO", "EDAD", "INDIGENA"))

# Eliminar casos muy recients recientes, y muy viejos
Dat <- Dat %>%
  filter(FECHA_SINTOMAS > args$fecha_inicio) %>%
  # filter(FECHA_SINTOMAS >= args$fecha_parteaguas) %>%
  filter(FECHA_SINTOMAS < Sys.Date() - 15 | !is.na(FECHA_DEF))
# Dat

# Seleccionar datos, convertir variables a indicadores,
# procesar edad, etc
d4 <- Dat %>%
  mutate(DEF = 1*(!is.na(FECHA_DEF)),
         TIPO_PACIENTE = replace(TIPO_PACIENTE, TIPO_PACIENTE == "99", NA),
         Mes = format(FECHA_SINTOMAS, "%m-%Y")) %>%
  mutate(HOSP = 1*(TIPO_PACIENTE == "2")) %>%
  select(names(rr_lut), DEF, TIPO_PACIENTE, HOSP, ENTIDAD_UM, SECTOR, ID_REGISTRO, Mes) %>%
  pivot_longer(cols = c(-ENTIDAD_UM, -DEF, -HOSP, -EDAD, -SEXO, -EMBARAZO, -SECTOR, -ID_REGISTRO, -Mes),
               names_to = "factor_riesgo", values_to = "valor") %>%
  mutate(SEXO = replace(SEXO, SEXO %in% c("97", "98", "99"), NA),
         EMBARAZO = replace(EMBARAZO, EMBARAZO %in% c("98", "99"), NA),
         SECTOR = replace(SECTOR, SECTOR %in% c("99"), NA),
         valor = replace(valor, valor %in% c("97", "98", "99"), NA)) %>%
  mutate(SEXO = 1*(SEXO == "2"),
         EMBARAZO = 1*(EMBARAZO == "1"),
         valor = 1*(valor == "1")) %>%
  pivot_wider(id_cols = c(ENTIDAD_UM, DEF, HOSP, EDAD, SEXO, EMBARAZO, SECTOR, ID_REGISTRO, Mes),
              names_from = factor_riesgo, values_from = valor) %>%
  select(-ID_REGISTRO) %>%
  drop_na %>%
  mutate(EDAD = 10 * (EDAD %/% 10)) %>%
  mutate(EDAD = replace(EDAD, EDAD >=80, 80)) %>%
  select(DEF, EDAD, Mes, SEXO, EMBARAZO,
         HABLA_LENGUA_INDIG, DIABETES, EPOC, ASMA,
         INMUSUPR, HIPERTENSION, CARDIOVASCULAR, OBESIDAD,
         RENAL_CRONICA, TABAQUISMO, INDIGENA, OTRA_COM) %>%
  group_by(EDAD, Mes, SEXO, EMBARAZO,
           HABLA_LENGUA_INDIG, DIABETES, EPOC, ASMA,
           INMUSUPR, HIPERTENSION, CARDIOVASCULAR, OBESIDAD,
           RENAL_CRONICA, TABAQUISMO, INDIGENA, OTRA_COM) %>%
  summarise(defs = sum(DEF),
            n_ind = length(DEF),
            .groups = 'drop') %>%
  mutate(EDAD = factor(EDAD, levels = sort(unique(EDAD)), ordered = TRUE))
# d4

m4 <- brm(defs | trials(n_ind) ~ mo(EDAD)*Mes + SEXO + EMBARAZO +
            HABLA_LENGUA_INDIG + INDIGENA +
            DIABETES + EPOC + ASMA + INMUSUPR +
            HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
            OBESIDAD + RENAL_CRONICA + TABAQUISMO,
          data = d4,
          chains = 4, cores = 4, warmup = 500, iter = 1500,
          family = binomial(link = "logit"))
summary(m4)
# save(m4, file = "m1.rr_edad_mes.rdat")
res <- conditional_effects(m4, "EDAD:Mes", prob = 0.95)
res <- res$`EDAD:Mes`
res <- res %>%
  as_tibble() %>%
  transmute(Edad = effect1__,
            Mes = effect2__,
            Estimate = exp(estimate__),
            lower = exp(lower__),
            upper = exp(upper__)) %>%
  mutate(Mes = parse_date(as.character(Mes), format = "%m-%Y")) 

p1 <- res %>%
  mutate(grupo = Edad > 40) %>%
  ggplot(aes(x = Edad, y = Estimate, col = Mes, group = Mes)) +
  # facet_wrap(~ grupo, scales = "free") +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  scale_y_continuous(name = "Riesgo relativo de\nfallecer por COVId-19") +
  scale_x_discrete(name = "Edad (años)", labels = c("0-9", "10-19", "20-29",
                                                    "30-39", "40-49", "50-59",
                                                    "60-69", "70-79", "80+")) +
  guides(color = guide_legend(title = "Mes\n(2021)")) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14, angle = 0, hjust = 1),
        legend.title = element_text(face = "bold", size = 16),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.margin = margin(l = 20, r = 20))
# p1
archivo <- file.path(args$dir_salida, "rr_edad_mes.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "rr_edad_mes@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
