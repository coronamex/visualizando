library(tidyverse)

args <- list(dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             estados_lut = "../datos/util/estados_lut_datos_abiertos.csv",
             poblacion = "../datos/demograficos/pob_estado.tsv")

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

# Seleccionar USMER
Dat <- Dat %>%
  filter(ORIGEN == "1") %>%
  # filter(RESULTADO != "3") %>%
  select(-FECHA_ACTUALIZACION, -ENTIDAD_NAC, -ENTIDAD_RES, -MUNICIPIO_RES,
         -NEUMONIA, -NACIONALIDAD, -EMBARAZO, -HABLA_LENGUA_INDIG, -DIABETES,
         -EPOC, -ASMA, -INMUSUPR, - HIPERTENSION, -OTRA_COM, -RENAL_CRONICA,
         -CARDIOVASCULAR, -OBESIDAD, -TABAQUISMO, -MIGRANTE,
         -PAIS_NACIONALIDAD, -PAIS_ORIGEN)
Dat
table(Dat$SECTOR)
table(Dat$TIPO_PACIENTE)

# Estimación rápida por fecha síntomas, estado, género (10%/100% de ambulatorio/hospitalizado )

Centinela <- Dat %>%
  split(.$ENTIDAD_UM) %>%
  map_dfr(function(d){
    # d <- Dat %>%
    #   filter(ENTIDAD_UM == "01")
    d %>%
      arrange(FECHA_SINTOMAS) %>%
      # filter(RESULTADO == "1") %>%
      # print(n=300) %>%
      split(.$FECHA_SINTOMAS) %>%
      map_dfr(function(d){
        tibble(casos_leves = sum(d$TIPO_PACIENTE == "1" & d$RESULTADO == "1"),
               casos_graves = sum(d$TIPO_PACIENTE == "2" & d$RESULTADO == "1"),
               leves_totales = sum(d$TIPO_PACIENTE == "1"),
               leves_prueba =  sum(d$TIPO_PACIENTE == "1" & d$RESULTADO != "3"),
               pruebas = sum(d$RESULTADO != 3))
      }, .id = "fecha_sintomas")
  }, .id = "estado")
Centinela
Centinela <- Centinela %>%
  mutate(factor_leves = (leves_totales + 1) / (leves_prueba + 1)) %>%
  mutate(factor_leves = replace_na(factor_leves, 10)) %>%
  mutate(factor_leves = replace(factor_leves, is.infinite(factor_leves), 10)) %>%
  mutate(casos_leves_est = factor_leves*casos_leves) %>%
  mutate(casos_nuevos_est = casos_leves_est + casos_graves,
         casos_probables = pruebas - casos_leves - casos_graves) %>%
  mutate(estado = as.vector(estados_lut[estado])) %>%
  select(estado, fecha_sintomas, casos_nuevos_est, casos_probables) %>%
  left_join(pob %>%
              transmute(estado, pob = conapo_2020 / sum(conapo_2020)),
            by = "estado") %>%
  # filter(fecha_sintomas == "2020-03-28") %>%
  # mutate(test = casos_nuevos_est * pob) %>%
  # select(test) %>%
  # sum
  group_by(fecha_sintomas) %>%
  summarise(est_nacional = sum(casos_nuevos_est * pob * 32 * 20)) %>%
  mutate(fecha_sintomas = parse_date(fecha_sintomas, format = "%Y-%m-%d")) %>%
  arrange(fecha_sintomas) %>%
  mutate(acum_nacional_est = floor(cumsum(est_nacional)),
         est_nacional = floor(est_nacional))
Centinela

Centinela %>%
  filter(fecha_sintomas == "2020-04-03")

p1 <- Centinela %>%
  pivot_longer(-fecha_sintomas,
               names_to = "grupo",
               values_to = "estimados") %>%
  ggplot(aes(x = fecha_sintomas, y = estimados,  col = grupo)) +
  geom_line(size = 3) +
  scale_color_discrete(labels = c("Casos acumulados\nestimados", "Casos nuevos\nestimados"), name = "") +
  geom_vline(xintercept = Sys.Date() - 16) +
  # annotate("text", label = "Fin ajuste de curva",
  #          x = Sys.Date() - args$dias_retraso - 1.5,
  #          y = 300000, angle = 90,
  #          size = 6) +
  # geom_vline(xintercept = args$fecha1, col = "red") +
  # annotate("text", label = "Medidas de mitigación",
  #          x = args$fecha1 - 1.5,
  #          y = 300000, angle = 90,
  #          size = 6) +
  # scale_color_manual(values = c("#e7298a", "#66a61e")) +
  # scale_size_manual(values = c(2, 0.1)) +
  # guides(size = FALSE) +
  ylab("Casos nacionales estimados") +
  xlab("Fecha de inicio de síntomas") +
  scale_y_continuous(labels = scales::comma) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        legend.box.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "sir_nacional_centinela.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "sir_nacional_centinela@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

Dat %>%
  filter(RESULTADO == "1") %>%
  filter(!is.na(FECHA_DEF))
