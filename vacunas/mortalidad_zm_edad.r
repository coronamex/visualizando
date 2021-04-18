library(tidyverse)
source("util/leer_datos_abiertos.r")


args <- list(datos_abiertos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             lut_zm = "../datos/util/zonas_metropolitanas_2015.csv")

Dat <- leer_datos_abiertos(args$datos_abiertos, solo_confirmados = TRUE,
                           solo_fallecidos = TRUE, solo_laboratorio = FALSE)

# Agregar por edad, fecha y municipio
Dat <- Dat %>%
  mutate(edad = NA) %>%
  mutate(edad = replace(EDAD, EDAD >= 60, "60+")) %>%
  mutate(edad = replace(edad, EDAD < 60, "0-59")) %>%
  mutate(mun_cve = paste0(ENTIDAD_RES, "_", MUNICIPIO_RES)) %>%
  group_by(mun_cve, edad, FECHA_DEF) %>%
  summarise(muertes = length(FECHA_DEF),
            .groups = 'drop')

# Leer lista de municipios en zonas metropolitanas
zm_muns <- read_csv(args$lut_zm)
zm_muns <- paste0(zm_muns$CVE_ENT, "_", zm_muns$CVE_MUN %>% str_sub(3,5) )


roll_media <- tibbletime::rollify(mean, window = 7, na_value = 0)
Dat
Dat %>%
  mutate(zm = c("no_zm", "zm")[ 1*(mun_cve %in% zm_muns) + 1 ]) %>%
  # select(zm) %>% table 
  group_by(edad, FECHA_DEF, zm) %>%
  summarise(defs = sum(muertes),
            .groups = 'drop') %>%
  split(list(.$zm, .$edad)) %>%
  map_dfr(function(d){
    days <- as.numeric(max(d$FECHA_DEF) - min(d$FECHA_DEF))
    tibble(FECHA_DEF = min(d$FECHA_DEF) + 0:days) %>%
      left_join(d, by = "FECHA_DEF") %>%
      mutate(edad = edad[1],
             zm = zm[1],
             defs = replace_na(defs, 0)) %>%
      mutate(muertes_ventana = roll_media(defs)) %>%
      mutate(muertes_escala = muertes_ventana / max(muertes_ventana))
  }) %>%
  filter(FECHA_DEF >= "2021-03-01") %>%
  # filter(FECHA_DEF >= "2020-07-15" & FECHA_DEF < "2020-10-01") %>%
  ggplot(aes(x = FECHA_DEF, y = muertes_escala, group = edad)) +
  facet_wrap(~zm) +
  geom_line(aes(col = edad)) + 
  # scale_y_log10() +
  xlab("Fecha de defunción") +
  ylab("Fallecimientos como proporción del máximo") +
  AMOR::theme_blackbox()
  



