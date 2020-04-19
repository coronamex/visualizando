library(tidyverse)

args <- list(serie_tiempo_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv",
             dias_previos = 7,
             muncipios_lut = "../datos/util/municipios_lut_datos_abiertos.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

Tab <- read_csv(args$serie_tiempo_municipios,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 municipio = col_character(),
                                 clave = col_character(),
                                 .default = col_number()))

# Casos en últimos días
dat <- Tab %>%
  select(fecha, sintomas_acumulados, clave, municipio) %>%
  # print() %>%
  split(.$clave) %>%
  map_dfr(function(d, dias_previos = 3){
    d %>%
      mutate(casos_recientes = sintomas_acumulados - lag(sintomas_acumulados, dias_previos, default = 0))
  }, dias_previos = args$dias_previos) %>%
  # print(n = 100) %>%
  group_by(fecha) %>%
  summarise(n_municipios = sum(casos_recientes > 0))
p1 <- dat  %>%
  ggplot(aes(x = fecha, y = n_municipios)) +
  geom_rect(aes(xmin = max(fecha) - 15, xmax = max(fecha),
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  geom_line(size = 3) +
  annotate("text",
           x = Sys.Date() - 8,
           y = 150,
           label = 'italic("Faltan casos\npor reportar\nen estos días")',
           hjust = "middle",
           parse = TRUE) +
  
  ylab("# muncipios con casos en semana previa") +
  xlab("Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank())
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "n_municipios.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "n_municipios@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

