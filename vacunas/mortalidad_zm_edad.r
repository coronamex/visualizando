library(tidyverse)
source("util/leer_datos_abiertos.r")


args <- list(datos_abiertos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             lut_zm = "../datos/util/zonas_metropolitanas_2015.csv",
             dias_recientes = 14,
             max_ola1 = "2020-07-21",
             max_ola2 = "2021-01-20",
             n_dias = 45)
args$max_ola1 <- parse_date(args$max_ola1, format = "%Y-%m-%d")
args$max_ola2 <- parse_date(args$max_ola2, format = "%Y-%m-%d")

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

# # Graficar todas las muertes
# Dat %>%
#   group_by(FECHA_DEF) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop')  %>%
#   ggplot(aes(x = FECHA_DEF, y = defs)) +
#   geom_bar(stat = "identity") +
#   theme_classic()
# # Encontrar max ola 2
# Dat %>%
#   group_by(FECHA_DEF) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop')  %>%
#   filter(defs == max(defs))
# 
# # Encontrar max ola 1
# Dat %>%
#   group_by(FECHA_DEF) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop')  %>%
#   filter(FECHA_DEF < "2020-10-01") %>%
#   filter(defs == max(defs))




# Calcular por grupo de edad y tipomun, eliminar días recientes incompletos
Dat <- Dat %>%
  filter(FECHA_DEF < max(FECHA_DEF) - args$dias_recientes) %>%
  mutate(zm = c("no_zm", "zm")[ 1*(mun_cve %in% zm_muns) + 1 ]) %>%
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
      mutate(muertes_ventana = roll_media(defs)) 
      # mutate(muertes_escala = muertes_ventana / max(muertes_ventana))
  })
Dat  


n_dias <- as.numeric(max(Dat$FECHA_DEF) - args$max_ola2)
n_dias



bind_rows(Dat %>%
            filter(FECHA_DEF >= args$max_ola1 & FECHA_DEF <= args$max_ola1 + n_dias) %>%
            group_by(edad, zm) %>%
            summarise(FECHA_DEF = FECHA_DEF,
                      muertes_escala = muertes_ventana / muertes_ventana[FECHA_DEF == args$max_ola1],
                      .groups = 'drop') %>%
            filter(FECHA_DEF > max(FECHA_DEF) - args$n_dias) %>%
            mutate(ola = '1a. "ola"'),
          Dat %>%
            filter(FECHA_DEF >= args$max_ola2 & FECHA_DEF <= args$max_ola2 + n_dias) %>%
            group_by(edad, zm) %>%
            summarise(FECHA_DEF = FECHA_DEF,
                      muertes_escala = muertes_ventana / muertes_ventana[FECHA_DEF == args$max_ola2],
                      .groups = 'drop') %>%
            filter(FECHA_DEF > max(FECHA_DEF) - args$n_dias) %>%
            mutate(ola = '2a. "ola"')) %>%
  mutate(zm = replace(zm, zm == "zm", "Zonas metropolitanas")) %>%
  mutate(zm = replace(zm, zm == "no_zm", "Zonas no metropolitanas")) %>%
  ggplot(aes(x = FECHA_DEF, y = muertes_escala)) +
  facet_wrap(ola ~ zm, scales = "free") +
  geom_line(aes(col = edad), size = 2) +
  scale_color_manual(values = c("#8073ac", "#e08214")) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Fecha de defunción") +
  ylab("Fallecimientos como proporción del máximo") +
  theme_classic()
  


    


# fecha_inicio <- max(Dat$FECHA_DEF) - args$n_dias
# Dat %>%
#   mutate(zm = c("no_zm", "zm")[ 1*(mun_cve %in% zm_muns) + 1 ]) %>%
#   # select(zm) %>% table 
#   group_by(edad, FECHA_DEF, zm) %>%
#   summarise(defs = sum(muertes),
#             .groups = 'drop') %>%
#   split(list(.$zm, .$edad)) %>%
#   map_dfr(function(d){
#     days <- as.numeric(max(d$FECHA_DEF) - min(d$FECHA_DEF))
#     tibble(FECHA_DEF = min(d$FECHA_DEF) + 0:days) %>%
#       left_join(d, by = "FECHA_DEF") %>%
#       mutate(edad = edad[1],
#              zm = zm[1],
#              defs = replace_na(defs, 0)) %>%
#       mutate(muertes_ventana = roll_media(defs)) %>%
#       mutate(muertes_escala = muertes_ventana / max(muertes_ventana))
#   }) %>%
#   filter(FECHA_DEF > fecha_inicio) %>%
#   # filter(FECHA_DEF >= "2020-07-15" & FECHA_DEF < "2020-10-01") %>%
#   ggplot(aes(x = FECHA_DEF, y = muertes_escala, group = edad)) +
#   facet_wrap(~zm) +
#   geom_line(aes(col = edad)) + 
#   # scale_y_log10() +
#   xlab("Fecha de defunción") +
#   ylab("Fallecimientos como proporción del máximo") +
#   AMOR::theme_blackbox()
  



