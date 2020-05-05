library(tidyverse)


args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             cdi_base = "cdi-base-indicadores-2015.csv",
             serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv")

# Leer casos confirmados
# Dat <- read_csv(args$base_de_datos,
#                 col_types = cols(FECHA_ACTUALIZACION = col_date(format = "%Y-%m-%d"),
#                                  FECHA_INGRESO = col_date(format = "%Y-%m-%d"),
#                                  FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"),
#                                  FECHA_DEF = col_character(),
#                                  EDAD = col_number(),
#                                  .default = col_character())) 
# stop_for_problems(Dat)
# Dat <- Dat %>%
#   mutate(FECHA_DEF = parse_date(x = FECHA_DEF, format = "%Y-%m-%d", na = c("9999-99-99", "", "NA")),
#          PAIS_NACIONALIDAD = parse_character(PAIS_NACIONALIDAD, na = c("99", "", "NA")),
#          PAIS_ORIGEN = parse_character(PAIS_ORIGEN, na = c("97", "", "NA"))) %>%
#   filter(RESULTADO == "1")
# Dat

# Leer CDI
Cdi <- read_csv(args$cdi_base)
stop_for_problems(Cdi)
Cdi <- Cdi %>%
  select(INEGI, ENT, NOMENT, MPO, NOMMUN, `GRADOMARGI 2015`, TPOBTOT, IPOB_INDI, NOMTIPO) %>%
  filter(NOMMUN != "Estados Unidos Mexicanos") %>%
  filter(NOMMUN != "Total Estatal")
Cdi

# Leer serie municipios
Casos <- read_csv(args$serie_municipios)
stop_for_problems(Casos)
Casos <- Casos %>%
  group_by(municipio, clave) %>%
  summarize(casos_acumulados = max(sintomas_acumulados),
            muertes_acumuladas = max(muertes_acumuladas)) %>%
  ungroup %>%
  filter(casos_acumulados > 10)
Casos

# Unir pob y casos
p1 <- Casos %>%
  mutate(INEGI = str_remove(clave, "_")) %>%
  left_join(Cdi, by = "INEGI") %>%
  select(-MPO, -ENT, - INEGI, -NOMENT) %>%
  mutate(incidencia = (1e5) * (casos_acumulados / TPOBTOT),
         mortalidad = (1e5) * (muertes_acumuladas / TPOBTOT),
         prop_indigena = IPOB_INDI / TPOBTOT) %>%
  select(-casos_acumulados, -muertes_acumuladas, -NOMMUN, -TPOBTOT, -IPOB_INDI) %>%
  select(everything(), gradomargi_2015 = `GRADOMARGI 2015`) %>%
  pivot_longer(cols = c(-municipio, -clave, -gradomargi_2015, -NOMTIPO, -prop_indigena),
               names_to = "indicador", values_to = "valor") %>%
  
  print() %>%
  
  ggplot(aes(x = prop_indigena, y = valor) ) +
  facet_wrap(~ indicador, scales = "free_y") +
  geom_point(aes(shape = NOMTIPO, col = gradomargi_2015)) +
  scale_shape_discrete(name = "") +
  geom_smooth(method = "lm") +
  scale_y_log10() +
  scale_x_log10(labels = scales::percent) +
  ylab(label = "por cada 100 mil habitantes") +
  guides(col = guide_legend(nrow=2),
         shape = guide_legend(nrow = 2)) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top")
p1
ggsave("test.png", p1, width = 10, height = 6, dpi = 150)  



