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
  filter(FECHA_SINTOMAS > args$fecha_inicio) %>%
  # filter(FECHA_SINTOMAS >= args$fecha_parteaguas) %>%
  filter(FECHA_SINTOMAS < Sys.Date() - 15 | !is.na(FECHA_DEF))
Dat

# # Seleccionar datos y convertir variables a indicadores
# d <- Dat %>%
#   mutate(DEF = 1*(!is.na(FECHA_DEF)),
#          TIPO_PACIENTE = replace(TIPO_PACIENTE, TIPO_PACIENTE == "99", NA),
#          Mes = format(FECHA_SINTOMAS, "%m-%Y")) %>%
#   # select(TIPO_PACIENTE) %>% table(useNA = "a")
#   mutate(HOSP = 1*(TIPO_PACIENTE == "2")) %>%
#   select(names(rr_lut), DEF, TIPO_PACIENTE, HOSP, ENTIDAD_UM, SECTOR, ID_REGISTRO, Mes) %>%
#   
#   # select(-FECHA_ACTUALIZACION, -FECHA_INGRESO, -FECHA_SINTOMAS, -FECHA_DEF,
#   #        -ORIGEN, -TIPO_PACIENTE,
#   #        -ENTIDAD_NAC, -ENTIDAD_RES, -MIGRANTE, -PAIS_NACIONALIDAD, -PAIS_ORIGEN,
#   #        -INTUBADO, -NEUMONIA, -UCI, -OTRO_CASO, -RESULTADO,
#   #        -MUNICIPIO_RES, -NACIONALIDAD) 
#   pivot_longer(cols = c(-ENTIDAD_UM, -DEF, -HOSP, -EDAD, -SEXO, -EMBARAZO, -SECTOR, -ID_REGISTRO, -Mes),
#                names_to = "factor_riesgo", values_to = "valor") %>%
#   mutate(SEXO = replace(SEXO, SEXO %in% c("97", "98", "99"), NA),
#          EMBARAZO = replace(EMBARAZO, EMBARAZO %in% c("98", "99"), NA),
#          SECTOR = replace(SECTOR, SECTOR %in% c("99"), NA),
#          valor = replace(valor, valor %in% c("97", "98", "99"), NA)) %>%
#   mutate(SEXO = 1*(SEXO == "2"),
#          EMBARAZO = 1*(EMBARAZO == "1"),
#          valor = 1*(valor == "1")) %>%
#   pivot_wider(id_cols = c(ENTIDAD_UM, DEF, HOSP, EDAD, SEXO, EMBARAZO, SECTOR, ID_REGISTRO, Mes),
#               names_from = factor_riesgo, values_from = valor) %>%
#   select(-ID_REGISTRO) %>%
#   drop_na %>%
#   mutate(EDAD = round(EDAD, -1))
# 
# d
# 
# ftable(DEF ~ Mes, d)

# m1 <- brm(DEF ~ mo(EDAD)*Mes + SEXO, data = d, chains = 4, cores = 4, warmup = 500, iter = 1000)
# m2 <- brm(DEF ~ mo(EDAD)*Mes + SEXO, data = d, chains = 4, cores = 4, warmup = 500, iter = 1000, family = bernoulli(link = "logit"))

# d
# d <- d %>%
#   select(DEF, EDAD, Mes, ENTIDAD_UM, SEXO, EMBARAZO,
#          SECTOR, HABLA_LENGUA_INDIG, DIABETES, EPOC, ASMA,
#          INMUSUPR, HIPERTENSION, CARDIOVASCULAR, OBESIDAD,
#          RENAL_CRONICA, TABAQUISMO, INDIGENA, OTRA_COM) %>%
#   group_by(EDAD, Mes, ENTIDAD_UM, SEXO, EMBARAZO,
#            SECTOR, HABLA_LENGUA_INDIG, DIABETES, EPOC, ASMA,
#            INMUSUPR, HIPERTENSION, CARDIOVASCULAR, OBESIDAD,
#            RENAL_CRONICA, TABAQUISMO, INDIGENA, OTRA_COM) %>%
#   summarise(defs = sum(DEF),
#             n_ind = length(DEF),
#             .groups = 'drop') %>%
#     mutate(EDAD = factor(EDAD, levels = sort(unique(d$EDAD)), ordered = TRUE)) 
# d
# 
# m3 <- brm(defs | trials(n_ind) ~ mo(EDAD)*Mes + SEXO + EMBARAZO +
#             HABLA_LENGUA_INDIG + INDIGENA +
#             DIABETES + EPOC + ASMA + INMUSUPR +
#             HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
#             OBESIDAD + RENAL_CRONICA + TABAQUISMO +
#             ENTIDAD_UM + SECTOR, data = d,
#           chains = 4, cores = 4, warmup = 500, iter = 1000,
#           family = binomial(link = "logit"))
# summary(m3)
# conditional_effects(m3, "EDAD:Mes")



# Seleccionar datos y convertir variables a indicadores
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
  mutate(EDAD = round(EDAD, -1)) %>%
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
  mutate(EDAD = factor(EDAD, levels = sort(unique(d$EDAD)), ordered = TRUE))
d4

m4 <- brm(defs | trials(n_ind) ~ mo(EDAD)*Mes + SEXO + EMBARAZO +
            HABLA_LENGUA_INDIG + INDIGENA +
            DIABETES + EPOC + ASMA + INMUSUPR +
            HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
            OBESIDAD + RENAL_CRONICA + TABAQUISMO,
          data = d4,
          chains = 4, cores = 4, warmup = 500, iter = 1000,
          family = binomial(link = "logit"))
summary(m4)
conditional_effects(m4, "EDAD:Mes")




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
  xlab("Riesgo relativo\n(Desde abril 2021)") +
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
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "riesgos_relativos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "riesgos_relativos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
