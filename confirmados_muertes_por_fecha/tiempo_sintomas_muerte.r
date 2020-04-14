library(tidyverse)

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

# Lee base de datos
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

# Seleccionar defunciones confirmadas
dat <- Dat %>%
  filter(!is.na(FECHA_DEF)) %>%
  filter(RESULTADO == "1") %>%
  select(SECTOR, SEXO, FECHA_SINTOMAS, FECHA_DEF, HABLA_LENGUA_INDI, EDAD) %>%
  mutate(numero_dias = as.numeric(FECHA_DEF - FECHA_SINTOMAS),
         sector = "Público",
         sexo = "Hombre") %>%
  mutate(sector = replace(sector, SECTOR == "99", NA)) %>%
  mutate(sector = replace(sector, SECTOR == "9", "Privado")) %>%
  mutate(sexo = replace(sexo, SEXO == "1", "Mujer")) %>%
  mutate(sexo = replace(sexo, SEXO == "99", NA)) %>%
  mutate(sesenta_o_mas = EDAD >= 60) %>%
  select(-SECTOR, -SEXO, -EDAD) %>%
  filter(numero_dias >= 0)
  # arrange(numero_dias) %>%
  # filter(HABLA_LENGUA_INDI == "1")
  # print(n = 500)
dat


dat <- Dat %>%
  filter(!is.na(FECHA_DEF)) %>%
  filter(RESULTADO == "1") %>%
  # select(SECTOR, SEXO, FECHA_SINTOMAS, FECHA_DEF, HABLA_LENGUA_INDI, EDAD) %>%
  mutate(numero_dias = as.numeric(FECHA_DEF - FECHA_SINTOMAS),
         tiempo_sintomas = as.numeric(Sys.Date() - FECHA_SINTOMAS)) %>%
  filter(numero_dias >= 0) %>%
  select(-FECHA_ACTUALIZACION, -ENTIDAD_NAC, -OTRO_CASO, -RESULTADO, -MIGRANTE, -PAIS_NACIONALIDAD, -PAIS_ORIGEN, -FECHA_DEF,
         -FECHA_INGRESO, -FECHA_SINTOMAS, -MUNICIPIO_RES , -ENTIDAD_RES, -UCI, -INTUBADO, -ORIGEN, -TIPO_PACIENTE,
         -ENTIDAD_UM, -NACIONALIDAD) 
dat
m1 <- lm(numero_dias ~ ., data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR - DIABETES, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR - DIABETES
         -OTRA_CON, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR - DIABETES
         -OTRA_CON - OBESIDAD, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR - DIABETES
         -OTRA_CON - OBESIDAD - EMBARAZO, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR - DIABETES
         -OTRA_CON - OBESIDAD - EMBARAZO - TABAQUISMO, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR - DIABETES
         -OTRA_CON - OBESIDAD - EMBARAZO - TABAQUISMO - SECTOR, data = dat)
summary(m1)
anova(m1)
drop1(m1, test = "F")

m1 <- lm(numero_dias ~ . - HABLA_LENGUA_INDI - CARDIOVASCULAR - SEXO
         - EPOC - HIPERTENSION - EDAD - INMUSUPR - DIABETES
         -OTRA_CON - OBESIDAD - EMBARAZO - TABAQUISMO - NEUMONIA, data = dat)
# summary(m1)
anova(m1)
drop1(m1, test = "F")

colnames(dat)
dat <- dat %>%
  mutate(DIABETES = DIABETES == "1",
         EPOC = EPOC == "1",
         ASMA = ASMA == "1",
         INMUSUPR = INMUSUPR == "1",
         HIPERTENSION = HIPERTENSION == "1",
         OTRA_CON = OTRA_CON == "1",
         CARDIOVASCULAR = CARDIOVASCULAR =="1",
         OBESIDAD = OBESIDAD == "1",
         RENAL_CRONICA = RENAL_CRONICA == "1",
         TABAQUISMO = TABAQUISMO == "1") %>%
  mutate(num_condiciones = DIABETES + EPOC + ASMA + INMUSUPR + HIPERTENSION + OTRA_CON + CARDIOVASCULAR + OBESIDAD + RENAL_CRONICA + TABAQUISMO)

ggplot(dat, aes(x = num_condiciones, y = numero_dias )) +
  geom_point()

m2 <- lm(numero_dias ~ num_condiciones + tiempo_sintomas, data = dat)
summary(m2)
anova(m2)
drop1(m2, test = "F")

p1 <- ggplot(dat, aes(x = numero_dias)) +
  # facet_wrap(~ RENAL_CRONICA, ncol = 1) + 
  # facet_grid(ASMA ~ ., scales = "free_y") +
  geom_histogram(bins = 15) +
  geom_vline(aes(xintercept = median(numero_dias))) +
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
p1
archivo <- file.path(args$dir_salida, "tiempo_defuncion.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "tiempo_defuncion@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)  

