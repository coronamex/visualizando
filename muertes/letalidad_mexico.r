library(tidyverse)
args <- list(serie_tiempo = "../datos/datos_abiertos/serie_tiempo_nacional_confirmados.csv",
             dias_retraso = 9,
             dir_salida = "../sitio_hugo/static/imagenes/")


Dat <- read_csv(args$serie_tiempo,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))
Dat


p1 <- Dat %>%
  mutate(letalidad_retr = muertes_acumuladas / lag(sintomas_acumulados, args$dias_retraso, default = 0)) %>%
  mutate(letalidad_inst = muertes_acumuladas / sintomas_acumulados) %>%
  filter(muertes_acumuladas > 0) %>%
  select(fecha, letalidad_inst, letalidad_retr) %>%
  pivot_longer(-fecha, names_to = "estimado", values_to = "letalidad") %>%
  # print(n = 100)
  
  ggplot(aes(x = fecha, y = letalidad, group = estimado)) +
  geom_line(aes(col = estimado), size = 3) +
  scale_color_manual(values = c("#00BCF4","#7CAE00"),
                     labels = c("Letalidad\ninstantánea", "Letalidad\ncon retraso")) +
  scale_y_continuous(labels = scales::percent) +
  ylab("Letalidad") +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "letalidad_mexico.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "letalidad_mexico@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
