library(tidyverse)

args <- list(poblacion = "../datos/demograficos/pob_estado.tsv",
             tasa_min = 0,
             dir_salida = "../sitio_hugo/static/imagenes/",
             max_dias = 10,
             serie_tiempo_estados = "../datos/datos_abiertos/serie_tiempo_estados_um_confirmados.csv")

# Leer poblaciones
pob <- read_tsv(args$poblacion,
                col_types = cols(estado = col_character(),
                                 .default = col_number()))
stop_for_problems(pob)

Dat <- read_csv(args$serie_tiempo_estados,
                col_types = cols(estado = col_character(),
                                 fecha = col_date(format = "%Y-%m-%d"),
                                 .default = col_number()))
stop_for_problems(Dat)

pal <- colorRampPalette(colors = rev(c("#a50026",
                                   "#d73027",
                                   "#f46d43",
                                   "#fdae61",
                                   "#fee090",
                                   "#ffffbf",
                                   "#e0f3f8",
                                   "#abd9e9",
                                   "#74add1",
                                   "#4575b4",
                                   "#313695")))

# Para graficar subconjunto de estados
estados <- NULL
# estados <- "Guanajuato"
p1 <- Dat %>% 
  left_join(pob %>% select(estado, pob = conapo_2020), 
            by = "estado") %>%
  mutate(casos_100mil = sintomas_acumulados / (pob/1e5)) %>%
  split(.$estado) %>%
  map_dfr(function(d, tasa_min){
    if(max(d$casos_100mil >= tasa_min) || d$estado[1] %in% estados){
      return(d)
    }
  }, tasa_min = args$tasa_min) %>%
  select(estado, casos_100mil, fecha) %>%
  filter(fecha > max(fecha) - args$max_dias) %>%
  mutate(estado = factor(estado, levels = rev(unique(estado)))) %>%
  ggplot(aes(x = fecha, y = estado)) +
  geom_tile(aes(fill = casos_100mil), width = 0.8, height = 0.8) +
  scale_fill_gradient2(low = pal(11)[1], mid = pal(11)[6],
                       high = pal(11)[11], midpoint = 10,
                       name = expression(frac(Casos, "100 mil habitantes"))) +
  scale_x_date(breaks = unique(Dat$fecha), labels = function(x){strftime(x, format = "%b %d")}) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        plot.margin = margin(l = 20, r = 20, b = 20))
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_100mil_estados.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_100mil_estados@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Añadri mortalidad por estado
p2 <- situacion_estados %>%
  filter(estado %in% levels(p1$data$estado)) %>%
  mutate(estado = factor(estado, levels = levels(p1$data$estado))) %>%
  ggplot(aes(y = estado, x = mortalidad)) +
  geom_bar(stat = "identity", fill = "brown") +
  # geom_bar(aes(fill=muertes_acumuladas), stat = "identity") +
  scale_x_continuous(position = "top") +
  xlab(label = expression(frac(Muertes, "100 mil habitantes"))) +
  AMOR::theme_blackbox() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        plot.margin = margin(l = -7, r = 20, b = 20),
        panel.background = element_blank(),
        panel.border = element_rect(size = 1.5, color = "black", fill = NA),
        axis.title.x = element_text(size = 10))
g1 <- cowplot::plot_grid(plotlist=list(p1,
                                       p2),
                         ncol=2, align='h', axis = 'tb',
                         rel_widths = c(2,1))
g1
ggsave("test.png", g1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_100mil_estados.jpeg")
