library(tidyverse)
library(brms)
source("util/leer_datos_abiertos.r")

args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz")

# Leer casos confirmados
Dat <- leer_datos_abiertos(archivo = args$base_de_datos,
                           solo_confirmados = TRUE, solo_fallecidos = FALSE)
# Eliminar casos recientes
Dat <- Dat %>%
  filter(FECHA_SINTOMAS < Sys.Date() - 15 | !is.na(FECHA_DEF))


################## Regresión logística múltiple multinivel ############
# names(Dat)

# Seleccionar datos y convertir variables a indicadores
d <- Dat %>%
  mutate(DEF = 1*(!is.na(FECHA_DEF)),
         TIPO_PACIENTE = replace(TIPO_PACIENTE, TIPO_PACIENTE == "99", NA)) %>%
  # select(TIPO_PACIENTE) %>% table(useNA = "a")
  mutate(HOSP = 1*(TIPO_PACIENTE == "2"))  %>%
  select(-FECHA_ACTUALIZACION, -FECHA_INGRESO, -FECHA_SINTOMAS, -FECHA_DEF,
         -ORIGEN, -TIPO_PACIENTE,
         -ENTIDAD_NAC, -ENTIDAD_RES, -MIGRANTE, -PAIS_NACIONALIDAD, -PAIS_ORIGEN,
         -INTUBADO, -NEUMONIA, -UCI, -OTRO_CASO, -RESULTADO,
         -MUNICIPIO_RES, -NACIONALIDAD) %>%
  pivot_longer(cols = c(-ENTIDAD_UM, -DEF, -HOSP, -EDAD, -SEXO, -EMBARAZO, -SECTOR, -ID_REGISTRO),
               names_to = "factor_riesgo", values_to = "valor") %>%
  mutate(SEXO = replace(SEXO, SEXO %in% c("97", "98", "99"), NA),
         EMBARAZO = replace(EMBARAZO, EMBARAZO %in% c("98", "99"), NA),
         SECTOR = replace(SECTOR, SECTOR %in% c("99"), NA),
         valor = replace(valor, valor %in% c("97", "98", "99"), NA),) %>%
  mutate(SEXO = 1*(SEXO == "2"),
         EMBARAZO = 1*(EMBARAZO == "1"),
         valor = 1*(valor == "1")) %>%
  pivot_wider(id_cols = c(ENTIDAD_UM, DEF, HOSP, EDAD, SEXO, EMBARAZO, SECTOR, ID_REGISTRO),
              names_from = factor_riesgo, values_from = valor) %>%
  select(-ID_REGISTRO) %>%
  drop_na
edad_mu <- mean(d$EDAD)
edad_sd <- sd(d$EDAD)
edad_mu
edad_sd

d <- d %>%
  mutate(EDAD = scale(EDAD) %>% as.numeric())

m.hosp <- brms::brm(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                     DIABETES + EPOC + ASMA + INMUSUPR +
                     HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                     OBESIDAD + RENAL_CRONICA + TABAQUISMO +
                     (1|ENTIDAD_UM) + (1|SECTOR),
                   data = d, family = brms::bernoulli(link = "logit"),
                   inits = "0", chains = 4, cores = 4,
                   iter = 3000,
                   warmup = 2000,
                   prior = brms::prior(normal(0,1), class = 'b') +
                     brms::prior(exponential(2), class = 'sd') +
                     brms::prior(normal(-1, 2), class = "Intercept"))
save(m.hosp, edad_mu, edad_sd, file = "m.hosp.rdat")