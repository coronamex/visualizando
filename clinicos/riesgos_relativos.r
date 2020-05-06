library(tidyverse)
library(epitools)

args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv")

# Leer casos confirmados
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
         PAIS_ORIGEN = parse_character(PAIS_ORIGEN, na = c("97", "", "NA"))) %>%
  filter(RESULTADO == "1")
Dat <- Dat %>%
  filter(FECHA_SINTOMAS < Sys.Date() - 15 | !is.na(FECHA_DEF))

comorb <- c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
            "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
            "TABAQUISMO", "EMBARAZO", "SEXO")
comorb <- set_names(comorb, comorb)

Dat <- Dat[,c(comorb, "TIPO_PACIENTE", "UCI", "FECHA_DEF")] %>%
  mutate(SEXO = replace(SEXO, SEXO == "1", "f")) %>%
  mutate(SEXO = replace(SEXO, SEXO == "2", "h"))

RR_hosp <- comorb %>%
  map_dfr(function(variable, Dat, respuesta = "TIPO_PACIENTE"){
    dat <- tibble(exposicion = Dat[[variable]],
                  respuesta = Dat[[respuesta]])
    dat <- dat %>%
      mutate(exposicion = replace(exposicion, exposicion %in% c("98", "99", "97"), NA),
             respuesta = replace(respuesta, respuesta %in% c("99", "98"), NA)) %>%
      mutate(exposicion = replace(exposicion, exposicion == "2", "0"),
             respuesta = replace(respuesta, respuesta %in% c("1", "97"), "0")) %>%
      mutate(respuesta = replace(respuesta, respuesta %in% c("2"), "1")) %>%
      ftable
    if(any(dim(dat) != c(2,2))){
      stop("ERROR")
    }
    res <- riskratio(dat)
    tibble(parametro = colnames(res$measure),
           valor = res$measure[2,])
    
  }, Dat = Dat, .id = "comorb") %>%
  pivot_wider(names_from = "parametro", values_from = "valor")
# RR_hosp %>%
#   arrange(lower)

RR_uci <- comorb %>%
  map_dfr(function(variable, Dat, respuesta = "TIPO_PACIENTE"){
    dat <- tibble(exposicion = Dat[[variable]],
                  respuesta = Dat[[respuesta]])
    dat <- dat %>%
      mutate(exposicion = replace(exposicion, exposicion %in% c("98", "99", "97"), NA),
             respuesta = replace(respuesta, respuesta %in% c("99", "98"), NA)) %>%
      mutate(exposicion = replace(exposicion, exposicion == "2", "0"),
             respuesta = replace(respuesta, respuesta %in% c("1", "97"), "0")) %>%
      mutate(respuesta = replace(respuesta, respuesta %in% c("2"), "1")) %>%
      ftable
    if(any(dim(dat) != c(2,2))){
      stop("ERROR")
    }
    res <- riskratio(dat)
    tibble(parametro = colnames(res$measure),
           valor = res$measure[2,])
    
  }, Dat = Dat, respuesta = "UCI", .id = "comorb") %>%
  pivot_wider(names_from = "parametro", values_from = "valor")
# RR_uci %>%
#   arrange(lower)



RR_def <- comorb %>%
  map_dfr(function(variable, Dat, respuesta){
    dat <- tibble(exposicion = Dat[[variable]],
                  respuesta = Dat[[respuesta]])
    dat <- dat %>%
      mutate(exposicion = replace(exposicion, exposicion %in% c("98", "99", "97"), NA)) %>%
      mutate(exposicion = replace(exposicion, exposicion == "2", "0"),
             respuesta = !is.na(respuesta)) %>%
      ftable
    if(any(dim(dat) != c(2,2))){
      stop("ERROR")
    }
    res <- riskratio(dat)
    tibble(parametro = colnames(res$measure),
           valor = res$measure[2,])
    
  }, Dat = Dat, respuesta = "FECHA_DEF", .id = "comorb") %>%
  pivot_wider(names_from = "parametro", values_from = "valor")
# RR_def %>%
#   arrange(lower)

rr_lut <- set_names(c("Habla lengua indígena", "Diabetes", "EPOC", "Asma", "Inmnosupresión",
                      "Hipertensión", "Otro", "Enfermedad cardiovascular", "Obesidad",
                      "Insuficiencia renal crónica",
                      "Tabaquismo", "Embarazo", "Hombre"),
                    c("HABLA_LENGUA_INDIG", "DIABETES", "EPOC", "ASMA", "INMUSUPR",
                      "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR", "OBESIDAD", "RENAL_CRONICA",
                      "TABAQUISMO", "EMBARAZO", "SEXO"))
RR_hosp
RR_hosp$Riesgo <- "Hospitalización"
RR_uci$Riesgo <- "Cuidados Intensivos"
RR_def$Riesgo <- "Muerte"
p1 <- RR_hosp %>%
  bind_rows(RR_uci) %>%
  bind_rows(RR_def) %>%
  arrange(lower) %>%
  mutate(comorb = as.vector(rr_lut[comorb])) %>%
  mutate(comorb = factor(comorb, levels = unique(comorb)),
         Riesgo = factor(Riesgo, levels = c("Hospitalización", "Cuidados Intensivos", "Muerte"))) %>%
  ggplot(aes(y = estimate, x = comorb, col = Riesgo)) +
  geom_hline(yintercept = 1, color = "darkgrey") +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 1)) +
  geom_point(position = position_dodge(width = 1)) +
  scale_color_manual(values = c("#b35806", "#542788", "#1b9e77"), name = "") +
  annotate("text",
           label = "Sin diferencia de riesgo",
           x = "Hipertensión",
           y = 0.935,
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
p1
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "riesgos_relativos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "riesgos_relativos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)



