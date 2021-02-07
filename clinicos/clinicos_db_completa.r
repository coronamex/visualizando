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
# library(epitools)
# library(lme4)
library(brms)
source("util/leer_datos_abiertos.r")

<<<<<<< HEAD:clinicos/clinicos_db_completa.r
args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")

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
  
  ylab(label = "Porcentaje de pacientes") +
  xlab(label = "Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
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
=======
#' Probabilidades posteriorers regresión logística riesgos
#'
#' @param d_pred
#' @param model
#' @param coef_ii
#' @param re_ii
#'
#' @return
#' @export
#'
#' @examples
pred_brms_logistic <- function(d_pred, model, coef_ii = 1:15, re_ii = 16:17){
  # d_pred <- d_pred_template
  # model <- m6
  # coef_ii <- 1:15
  # re_ii <- 16:17

  # Extraer posterior de modelo brms
  post <- posterior_samples(m6)[,c(coef_ii, re_ii)] %>%
    as_tibble()
  # post

  # Re ajustar indices
  coef_ii <- 1:length(coef_ii)
  re_ii <- (length(coef_ii) + 1):(length(coef_ii) + length(re_ii))

  # Cambiar "efectos aleatorios" por predicción
  for(i in re_ii){
    post[,i] <- rnorm(n = nrow(post), mean = 0, sd = post[,i] %>% unlist %>% as.numeric())
  }

  # Calcular predictor lineal para cada muestra de la posterior
  pred_post <- apply(post, 1, function(coef, d_pred){
    # coef <- post[1,] %>% as.numeric()
    # Formula sin efectos aleatorios
    X <- model.matrix(~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                        DIABETES + EPOC + ASMA + INMUSUPR +
                        HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                        OBESIDAD + RENAL_CRONICA + TABAQUISMO,
                      data = d_pred)
    # Coeficientes efectos "fijos"
    beta <- coef[coef_ii]
    # Ecuación normal más efectos aleatorios
    y <- X %*% matrix(beta, ncol = 1) + sum(coef[re_ii])

    y
  }, d_pred = d_pred) %>%
    apply(., 1, quantile, probs = c(0.1, 0.5, 0.9)) %>%
    t %>%
    as_tibble() %>%
    rename(p_def_q10 = '10%',
           p_def_q50 = '50%',
           p_def_q90 = '90%') %>%
    mutate_at(.vars = c("p_def_q10", "p_def_q50", "p_def_q90"), .funs = logistic)

  return(pred_post)
}



logistic <- function(x){
  1 / (1 + exp(-x))
}

args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz")
cat("Calculando riesgos relativos...\n")
>>>>>>> 7b808e8fb3fdb5524280bd95de7b641ff79d40ee:clinicos/riesgos_relativos.r

#######################################

# LUT de nombres de variables
rr_lut <- set_names(c("Habla lengua indígena", "Diabetes", "EPOC", "Asma", "Inmnosupresión",
                      "Hipertensión", "Otro", "Enfermedad cardiovascular", "Obesidad",
                      "Insuficiencia renal crónica",
                      "Tabaquismo", "Embarazo", "Hombre", "10 años más", "Indígena"),
                    c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
                      "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
                      "TABAQUISMO", "EMBARAZO", "SEXO", "EDAD", "INDIGENA"))

# Eliminar casos recientes
Dat <- Dat %>%
  filter(FECHA_SINTOMAS < Sys.Date() - 15 | !is.na(FECHA_DEF))

# Seleccionar datos y convertir variables a indicadores
d <- Dat %>%
  mutate(DEF = 1*(!is.na(FECHA_DEF)),
         TIPO_PACIENTE = replace(TIPO_PACIENTE, TIPO_PACIENTE == "99", NA)) %>%
  # select(TIPO_PACIENTE) %>% table(useNA = "a")
  mutate(HOSP = 1*(TIPO_PACIENTE == "2")) %>%
  select(names(rr_lut), DEF, TIPO_PACIENTE, HOSP, ENTIDAD_UM, SECTOR, ID_REGISTRO) %>%
  
  # select(-FECHA_ACTUALIZACION, -FECHA_INGRESO, -FECHA_SINTOMAS, -FECHA_DEF,
  #        -ORIGEN, -TIPO_PACIENTE,
  #        -ENTIDAD_NAC, -ENTIDAD_RES, -MIGRANTE, -PAIS_NACIONALIDAD, -PAIS_ORIGEN,
  #        -INTUBADO, -NEUMONIA, -UCI, -OTRO_CASO, -RESULTADO,
  #        -MUNICIPIO_RES, -NACIONALIDAD) 
  pivot_longer(cols = c(-ENTIDAD_UM, -DEF, -HOSP, -EDAD, -SEXO, -EMBARAZO, -SECTOR, -ID_REGISTRO),
               names_to = "factor_riesgo", values_to = "valor") %>%
  mutate(SEXO = replace(SEXO, SEXO %in% c("97", "98", "99"), NA),
         EMBARAZO = replace(EMBARAZO, EMBARAZO %in% c("98", "99"), NA),
         SECTOR = replace(SECTOR, SECTOR %in% c("99"), NA),
         valor = replace(valor, valor %in% c("97", "98", "99"), NA)) %>%
  mutate(SEXO = 1*(SEXO == "2"),
         EMBARAZO = 1*(EMBARAZO == "1"),
         valor = 1*(valor == "1")) %>%
  pivot_wider(id_cols = c(ENTIDAD_UM, DEF, HOSP, EDAD, SEXO, EMBARAZO, SECTOR, ID_REGISTRO),
              names_from = factor_riesgo, values_from = valor) %>%
  select(-ID_REGISTRO) %>%
  drop_na

d <- d %>%
  mutate(EDAD = EDAD / 10)

cat("Calculando riesgos relativos...\n")
# Sólo efectos fijos
m1 <- glm(DEF ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG + INDIGENA +
            DIABETES + EPOC + ASMA + INMUSUPR +
            HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
            OBESIDAD + RENAL_CRONICA + TABAQUISMO +
            ENTIDAD_UM + SECTOR,
          data = d, family = binomial(link = "logit"))
# summary(m1)
m2 <- glm(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +  INDIGENA +
            DIABETES + EPOC + ASMA + INMUSUPR +
            HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
            OBESIDAD + RENAL_CRONICA + TABAQUISMO +
            ENTIDAD_UM + SECTOR,
          data = d, family = binomial(link = "logit"))
# summary(m2)
#
# # Efectos aleatorios para entidad y sectrp
# m3 <- lme4::glmer(DEF ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
#                     DIABETES + EPOC + ASMA + INMUSUPR +
#                     HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
#                     OBESIDAD + RENAL_CRONICA + TABAQUISMO +
#                     (1|ENTIDAD_UM) + (1|SECTOR),
#                   data = d, family = binomial(link = "logit"),
#                   verbose = TRUE)
# summary(m3)
#
# m4 <- lme4::glmer(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
#                     DIABETES + EPOC + ASMA + INMUSUPR +
#                     HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
#                     OBESIDAD + RENAL_CRONICA + TABAQUISMO +
#                     (1|ENTIDAD_UM) + (1|SECTOR),
#                   data = d, family = binomial(link = "logit"),
#                   verbose = TRUE)
# summary(m4)
#
#
# # predict.fun <- function(m4) {
# #   predict(m4, newdata = d[1,], re.form = NA)   # This is predict.merMod
# #   # fixef(m4)
# # }
# # m.boots <- bootMer(m1, predict.fun, nsim = 10, verbose = TRUE, .progress = "txt")
#
m5 <- brms::brm(DEF ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                  DIABETES + EPOC + ASMA + INMUSUPR +
                  HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                  OBESIDAD + RENAL_CRONICA + TABAQUISMO +
                  (1|ENTIDAD_UM) + (1|SECTOR),
                data = d, family = brms::bernoulli(link = "logit"),
                inits = "0", chains = 1, cores = 1, iter = 500,
                prior = brms::prior(normal(0,1), class = 'b') +
                  brms::prior(exponential(2), class = 'sd'))
summary(m5)
#
# m6 <- brms::brm(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
#                   DIABETES + EPOC + ASMA + INMUSUPR +
#                   HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
#                   OBESIDAD + RENAL_CRONICA + TABAQUISMO +
#                   (1|ENTIDAD_UM) + (1|SECTOR),
#                 data = d, family = brms::bernoulli(link = "logit"),
#                 inits = "0", chains = 1, cores = 1, iter = 500,
#                 prior = brms::prior(normal(0,1), class = 'b'))
# summary(m6)
#
# # save(m1,m2,m3,m4,m5,m6, file = "mortality_logit.rdat")
#
d_pred_template <- tibble(EDAD.real = rep(seq(from = 20, to = 70, by = 5), each = 2),
                          SEXO = rep(0:1, times = 11),
                          EMBARAZO = 0,
                          HABLA_LENGUA_INDIG = 0,
                          DIABETES = 0,
                          EPOC = 0,
                          ASMA = 0,
                          INMUSUPR = 0,
                          HIPERTENSION = 0,
                          OTRA_COM = 0,
                          CARDIOVASCULAR = 0,
                          OBESIDAD = 0,
                          RENAL_CRONICA = 0,
                          TABAQUISMO = 0) %>%
  mutate(EDAD = (EDAD.real - edad_mu) / edad_sd )
d_pred_template

# mcmc_plot(m6, type = "trace")

load("mortality_logit.rdat")

f_riesgo <- c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
              "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
              "TABAQUISMO")

Res <- pred_brms_logistic(d_pred = d_pred_template, model = m5, coef_ii = 1:15, re_ii = 16:17) %>%
  bind_cols(d_pred_template %>%
              select(EDAD.real, SEXO)) %>%
  mutate(SEXO = as.character(SEXO),
         f_riesgo = "Sin factores de riesgo")

for(fr in f_riesgo){
  d_pred <- d_pred_template
  d_pred[fr] <- 1

  res <- pred_brms_logistic(d_pred = d_pred, model = m5, coef_ii = 1:15, re_ii = 16:17) %>%
    bind_cols(d_pred %>%
                select(EDAD.real, SEXO)) %>%
    mutate(SEXO = as.character(SEXO),
           f_riesgo = fr)

  Res <- Res %>%
    bind_rows(res)
}

Res
Res %>%
  ggplot(aes(x = EDAD.real, group = SEXO, col = SEXO, fill = SEXO)) +
  facet_wrap(. ~ f_riesgo, ncol = 3) +
  geom_line(aes(y = p_def_q50)) +
  geom_ribbon(aes(ymin = p_def_q10, ymax = p_def_q90), alpha = 0.2) +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 50) +
  theme_classic()


summary(m5)


ggsave("test.png", mcmc_plot(m5, type = "pairs"), width = 25, height = 25)





p1 <- list(Muerte = m1,
           `Hospitalización` = m2) %>%
  map_dfr(broom::tidy, .id = "resultado") %>%
  filter(term %in% names(rr_lut)) %>%
  mutate(riesgo_inferior = estimate + qnorm(p = 0.025) * std.error,
         riesgo_superior = estimate + qnorm(p = 0.975) * std.error) %>%
  transmute(resultado,
            factor_riesgo = as.character(rr_lut[term]),
            riesgo_relativo = exp(estimate),
            riesgo_inferior = exp(riesgo_inferior),
            riesgo_superior = exp(riesgo_superior)) %>%
  arrange(riesgo_inferior) %>%
  mutate(factor_riesgo = factor(factor_riesgo, levels = unique(factor_riesgo))) %>%
  
  # print(n = 100) %>%

  ggplot(aes(x = riesgo_relativo, y = factor_riesgo, col = resultado)) +
  geom_vline(xintercept = 1, color = "darkgrey") +
  geom_errorbarh(aes(xmin = riesgo_inferior, xmax = riesgo_superior), position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(width = 1)) +
  scale_color_manual(values = c("#b35806", "#1b9e77"), name = "") +
  annotate("text",
           label = "Sin diferencia de riesgo",
           x = 0.85,
           y = 11,
           col = "darkgrey",
           angle = 90,
           size = 6) +
  ylab("Factores de riesgo") +
  xlab("Riesgo relativo") +
  guides(color = guide_legend(nrow = 2)) +
  AMOR::theme_blackbox() +
  theme(axis.text.x = element_text(angle = 90),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "riesgos_relativos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "riesgos_relativos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
<<<<<<< HEAD:clinicos/clinicos_db_completa.r
=======


# Encontrar casos "sin" factores de riesgo
# d %>%
#   filter(EMBARAZO == 0 & HABLA_LENGUA_INDIG == 0 &
#            DIABETES == 0 & EPOC == 0 & ASMA == 0 &
#            INMUSUPR == 0 & HIPERTENSION == 0 &
#            OTRA_COM == 0 & CARDIOVASCULAR == 0 &
#            OBESIDAD == 0 & RENAL_CRONICA == 0 &
#            TABAQUISMO == 0) %>%
#   filter(DEF == 1)
#   # filter(SEXO == 1) %>% select(EDAD) %>% summary
#   # select(EDAD) %>% summary
#   # select(SEXO) %>% table
>>>>>>> 7b808e8fb3fdb5524280bd95de7b641ff79d40ee:clinicos/riesgos_relativos.r
