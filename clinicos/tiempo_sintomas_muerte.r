library(tidyverse)
source("util/leer_datos_abiertos.r")

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")

# Lee base de datos
Dat <- leer_datos_abiertos(archivo = args$base_de_datos, solo_confirmados = TRUE, solo_fallecidos = TRUE)
dat <- Dat %>%
  select(FECHA_DEF, FECHA_SINTOMAS) %>%
  mutate(numero_dias = as.numeric(FECHA_DEF - FECHA_SINTOMAS))
dat

p1 <- ggplot(dat, aes(x = numero_dias)) +
  # facet_wrap(~ RENAL_CRONICA, ncol = 1) + 
  # facet_grid(ASMA ~ ., scales = "free_y") +
  geom_histogram(bins = 15) +
  geom_vline(aes(xintercept = median(numero_dias))) +
  scale_x_continuous(breaks = function(lims){seq(from = 0, to = lims[2], by = 5)}) +
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
# p1
archivo <- file.path(args$dir_salida, "tiempo_defuncion.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "tiempo_defuncion@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)  

