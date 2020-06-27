library(tidyverse)
library(epitools)
source("util/leer_datos_abiertos.r")

logit <- function(x){
  1 / (1 + exp(-x))
}

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

comorb <- c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
            "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
            "TABAQUISMO", "EMBARAZO", "SEXO")
comorb <- set_names(comorb, comorb)

# Dat <- Dat[,c(comorb, "TIPO_PACIENTE", "UCI", "FECHA_DEF")] %>%
#   mutate(SEXO = replace(SEXO, SEXO == "1", "f")) %>%
#   mutate(SEXO = replace(SEXO, SEXO == "2", "h"))
Dat <- Dat[,c(comorb, "TIPO_PACIENTE", "UCI", "FECHA_DEF", "EDAD")] %>%
  mutate(SEXO = replace(SEXO, SEXO == "1", "f")) %>%
  mutate(SEXO = replace(SEXO, SEXO == "2", "h")) %>%
  mutate(SEXO = replace(SEXO, SEXO == "h", "1")) %>%
  mutate(SEXO = replace(SEXO, SEXO == "f", "2"))

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
Res %>%
  print(n = 200)
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


