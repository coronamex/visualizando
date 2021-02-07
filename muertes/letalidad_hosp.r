library(tidyverse)


args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv",
             lut_estados = "../datos/util/estados_lut_datos_abiertos.csv")

lut_estados <- read_csv(args$lut_estados, col_names = FALSE)
stop_for_problems(lut_estados)
lut_estados <- set_names(lut_estados$X2, lut_estados$X1)

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
Dat


Dat <- Dat %>%
  group_by(ENTIDAD_UM) %>%
  summarize(muertes = sum(!is.na(FECHA_DEF)),
            casos = length(unique(ID_REGISTRO)),
            hospitalizaciones = sum(TIPO_PACIENTE == "2"),
            muertes_hospital = sum(TIPO_PACIENTE == "2" & !is.na(FECHA_DEF)))
# GGally::ggpairs(Dat %>% select(-ENTIDAD_UM))

Dat %>%
  transmute(estado = as.vector(lut_estados[ENTIDAD_UM]),
            letalidad = muertes / casos,
            letalidad_hosp = muertes_hospital / hospitalizaciones,
            prop_hosp = hospitalizaciones / casos,
            prop_muertes_host = muertes_hospital / muertes) %>%
  # select(-estado, -ENTIDAD_UM) %>%
  # mutate(muertes = log2(muertes),
  #        casos = log2(casos),
  #        hospitalizaciones = log2(hospitalizaciones),
  #        muertes_hospital = log2(muertes_hospital)) %>%
  # GGally::ggpairs()
  pivot_longer(-estado, names_to = "indicador", values_to = "valor") %>%
  mutate(estado = factor(estado, levels = rev(unique(estado)))) %>%
  
  ggplot(aes(x = valor, y = estado)) +
  facet_grid(~ indicador, scales = "free_x") +
  geom_bar(stat = "identity") +
  scale_x_continuous(labels = scales::percent)
