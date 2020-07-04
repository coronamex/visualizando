library(tidyverse)
# library(epitools)
library(lme4)
source("util/leer_datos_abiertos.r")

# logistic <- function(x){
#   1 / (1 + exp(-x))
# }

args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz")

rr_lut <- set_names(c("Habla lengua indígena", "Diabetes", "EPOC", "Asma", "Inmnosupresión",
                      "Hipertensión", "Otro", "Enfermedad cardiovascular", "Obesidad",
                      "Insuficiencia renal crónica",
                      "Tabaquismo", "Embarazo", "Hombre"),
                    c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
                      "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
                      "TABAQUISMO", "EMBARAZO", "SEXO"))

# Leer casos confirmados
Dat <- leer_datos_abiertos(archivo = args$base_de_datos,
                           solo_confirmados = TRUE, solo_fallecidos = FALSE)
# Eliminar casos recientes
Dat <- Dat %>%
  filter(FECHA_SINTOMAS < Sys.Date() - 15 | !is.na(FECHA_DEF))


################## Regresión logística múltiple multinivel ############
names(Dat)

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
d
edad_mu <- mean(d$EDAD)
edad_sd <- sd(d$EDAD)
edad_mu
edad_sd

d <- d %>%
  mutate(EDAD = scale(EDAD) %>% as.numeric())
d

# Sólo efectos fijos
m1 <- glm(DEF ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
            DIABETES + EPOC + ASMA + INMUSUPR +
            HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
            OBESIDAD + RENAL_CRONICA + TABAQUISMO +
            ENTIDAD_UM + SECTOR,
          data = d, family = binomial(link = "logit"))
summary(m1)
m2 <- glm(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
            DIABETES + EPOC + ASMA + INMUSUPR +
            HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
            OBESIDAD + RENAL_CRONICA + TABAQUISMO +
            ENTIDAD_UM + SECTOR,
          data = d, family = binomial(link = "logit"))
summary(m2)

# Efectos aleatorios para entidad y sectrp
m3 <- lme4::glmer(DEF ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                    DIABETES + EPOC + ASMA + INMUSUPR +
                    HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                    OBESIDAD + RENAL_CRONICA + TABAQUISMO +
                    (1|ENTIDAD_UM) + (1|SECTOR),
                  data = d, family = binomial(link = "logit"),
                  verbose = TRUE)
summary(m3)

m4 <- lme4::glmer(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                    DIABETES + EPOC + ASMA + INMUSUPR +
                    HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                    OBESIDAD + RENAL_CRONICA + TABAQUISMO +
                    (1|ENTIDAD_UM) + (1|SECTOR),
                  data = d, family = binomial(link = "logit"),
                  verbose = TRUE)
summary(m4)


# predict.fun <- function(m4) {
#   predict(m4, newdata = d[1,], re.form = NA)   # This is predict.merMod
#   # fixef(m4)
# }
# m.boots <- bootMer(m1, predict.fun, nsim = 10, verbose = TRUE, .progress = "txt")

m5 <- brms::brm(DEF ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                  DIABETES + EPOC + ASMA + INMUSUPR +
                  HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                  OBESIDAD + RENAL_CRONICA + TABAQUISMO +
                  (1|ENTIDAD_UM) + (1|SECTOR),
                data = d, family = brms::bernoulli(link = "logit"),
                inits = "0", chains = 1, cores = 1, iter = 500,
                prior = brms::prior(normal(0,1), class = 'b'))
summary(m5)

m6 <- brms::brm(HOSP ~ EDAD + SEXO + EMBARAZO + HABLA_LENGUA_INDIG +
                  DIABETES + EPOC + ASMA + INMUSUPR +
                  HIPERTENSION + OTRA_COM + CARDIOVASCULAR +
                  OBESIDAD + RENAL_CRONICA + TABAQUISMO +
                  (1|ENTIDAD_UM) + (1|SECTOR),
                data = d, family = brms::bernoulli(link = "logit"),
                inits = "0", chains = 1, cores = 1, iter = 500,
                prior = brms::prior(normal(0,1), class = 'b'))
summary(m6)
  
####################################

# Elegir comorbilidades a estudiar
comorb <- c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
            "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
            "TABAQUISMO", "EMBARAZO", "SEXO")
comorb <- set_names(comorb, comorb)

# Seleccionar columnas y recodificar columna SEXO para que
# hombre = 1 (conveniente para graficar)
Dat <- Dat[,c(comorb, "TIPO_PACIENTE", "UCI", "FECHA_DEF", "EDAD")] %>%
  mutate(SEXO = replace(SEXO, SEXO == "1", "f")) %>%
  mutate(SEXO = replace(SEXO, SEXO == "2", "h")) %>%
  mutate(SEXO = replace(SEXO, SEXO == "h", "1")) %>%
  mutate(SEXO = replace(SEXO, SEXO == "f", "2"))

Dat

Res <- Dat %>%
  pivot_longer(cols = c(-TIPO_PACIENTE, -UCI, -FECHA_DEF, -EDAD),
               names_to = "factor_riesgo", values_to = "valor") %>%
  split(.$factor_riesgo) %>%
  map_dfr(function(d){
    # d <- d$TABAQUISMO
    # d
    d <- d %>%
      mutate(TIPO_PACIENTE = 1 * (TIPO_PACIENTE == "2"),
             FECHA_DEF = 1 * !is.na(FECHA_DEF),
             UCI = replace(UCI, UCI %in% c("97", "98", "99"), NA),
             valor = replace(valor, valor %in% c("97", "98", "99"), NA)) %>%
      mutate(UCI = 1 * (UCI == "1"),
             valor = 1 * (valor == "1"))
    # d
     
    m1 <- glm(TIPO_PACIENTE ~ valor + EDAD, family = binomial(link = "logit"), data = d)
    m2 <- glm(UCI ~ valor + EDAD, family = binomial(link = "logit"), data = d)
    m3 <- glm(FECHA_DEF ~ valor + EDAD, family = binomial(link = "logit"), data = d)

    broom::tidy(m1) %>%
      bind_cols(broom::confint_tidy(m1, conf.level = 0.95)) %>%
      mutate(resultado = "Hospitalización") %>%
      bind_rows(broom::tidy(m2) %>%
                  bind_cols(broom::confint_tidy(m2, conf.level = 0.95)) %>%
                  mutate(resultado = "Cuidados Intensivos")) %>%
      bind_rows(broom::tidy(m3) %>%
                  bind_cols(broom::confint_tidy(m3, conf.level = 0.95)) %>%
                  mutate(resultado = "Muerte"))
    
  }, .id = "factor_riesgo")

# Graficar
p1 <- Res %>%
  filter(resultado != "Cuidados Intensivos") %>%
  
  filter(term == "valor") %>%
  select(-term) %>%
  # arrange(p.value) %>%
  # print(n) %>%
  transmute(factor_riesgo, resultado,
            riesgo_relativo = exp(estimate),
            riesgo_inferior = exp(conf.low),
            riesgo_mayor = exp(conf.high)) %>%
  arrange(riesgo_inferior) %>%
  # print(n = 100)
  mutate(factor_riesgo = as.vector(rr_lut[factor_riesgo])) %>%
  # mutate(factor_riesgo = factor(factor_riesgo, levels = unique(factor_riesgo)),
  #        resultado = factor(resultado, levels = c("Hospitalización", "Cuidados Intensivos", "Muerte"))) %>%
  mutate(factor_riesgo = factor(factor_riesgo, levels = unique(factor_riesgo)),
         resultado = factor(resultado, levels = c("Hospitalización", "Muerte"))) %>%
  
  
  ggplot(aes(y = riesgo_relativo, x = factor_riesgo, col = resultado)) +
  geom_hline(yintercept = 1, color = "darkgrey") +
  geom_errorbar(aes(ymin = riesgo_inferior, ymax = riesgo_mayor), position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(width = 1)) +
  # scale_color_manual(values = c("#b35806", "#542788", "#1b9e77"), name = "") +
  scale_color_manual(values = c("#b35806", "#1b9e77"), name = "") +
  annotate("text",
           label = "Sin diferencia de riesgo",
           x = "Hipertensión",
           y = 0.9,
           col = "darkgrey",
           angle = 90,
           size = 6) +
  coord_flip() +
  xlab("Factores de riesgo") +
  ylab("Riesgo relativo") +
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


