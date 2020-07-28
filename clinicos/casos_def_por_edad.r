library(tidyverse)
source("util/leer_datos_abiertos.r")

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Casos y defunciones por edad...\n")

Dat <- leer_datos_abiertos(archivo = args$base_de_datos,
                           solo_confirmados = TRUE,
                           solo_fallecidos = FALSE)

# Seleccionar fechas y redondear edades
Dat <- Dat %>%
  select(FECHA_SINTOMAS,
         EDAD,
         FECHA_DEF) %>%
  mutate(EDAD = floor(EDAD / 10) * 10) %>%
  mutate(EDAD = replace(EDAD, EDAD >= 70, 70))

p1 <- bind_rows(Dat %>%
            # filter(!is.na(FECHA_DEF)) %>%
            group_by(FECHA_SINTOMAS, EDAD) %>%
            summarise(sintomas_nuevos = length(EDAD)) %>%
            ungroup() %>%
            mutate(grupo = "Casos"),
          Dat %>%
            filter(!is.na(FECHA_DEF)) %>%
            group_by(FECHA_SINTOMAS, EDAD) %>%
            summarise(sintomas_nuevos = length(EDAD)) %>%
            ungroup() %>%
            mutate(grupo = "Defunciones")) %>%
  
  filter(FECHA_SINTOMAS >= "2020-03-01") %>%
  filter(FECHA_SINTOMAS < max(FECHA_SINTOMAS) - 11) %>%
  # arrange(desc(FECHA_SINTOMAS)) %>%
  # print()
  
  ggplot(aes(x = FECHA_SINTOMAS, y = sintomas_nuevos)) +
  facet_wrap(~ grupo, ncol = 1) +
  geom_bar(aes(fill = as.character(EDAD)), stat = "identity", position = "fill", width = 1) +
  geom_hline(yintercept = c(0.25, 0.5, 0.75)) +
  scale_fill_brewer(palette = "PuOr", name = "Edad",
                    labels = c("0-9 años",
                               "10-19 años",
                               "20-29 años",
                               "30-39 años",
                               "40-49 años",
                               "50-59 años",
                               "60-69 años",
                               "70 ó más años")) +
  scale_y_continuous(labels = scales::percent) +
  
  ylab(label = "Porcentaje de pacientes") +
  xlab(label = "Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 12))
# p1 
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_def_por_edad.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_def_por_edad@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


# p1$data %>%
#   filter(grupo == "Casos") %>%
#   filter(FECHA_SINTOMAS >= "2020-04-15" & FECHA_SINTOMAS < "2020-05-01") %>%
#   group_by(EDAD) %>%
#   summarize(casos = sum(sintomas_nuevos)) %>%
#   mutate(perc = casos / sum(casos))
# 
# p1$data %>%
#   filter(grupo == "Casos") %>%
#   filter(FECHA_SINTOMAS >= "2020-07-01") %>%
#   group_by(EDAD) %>%
#   summarize(casos = sum(sintomas_nuevos)) %>%
#   mutate(perc = casos / sum(casos))

