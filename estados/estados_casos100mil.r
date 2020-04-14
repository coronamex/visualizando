library(tidyverse)

args <- list(poblacion = "../datos/demograficos/pob_estado.tsv",
             tasa_min = 0,
             dir_salida = "../sitio_hugo/static/imagenes/",
             max_dias = 10,
             serie_tiempo_estados = "../datos/ssa_dge/serie_tiempo_estados_casos.csv")

# Leer poblaciones
pob <- read_tsv(args$poblacion,
                col_types = cols(estado = col_character(),
                                 .default = col_number()))
stop_for_problems(pob)
# pob

Dat <- read_csv(args$serie_tiempo_estados,
                col_types = cols(estado = col_character(),
                                 casos_acumulados = col_number(),
                                 fecha = col_date(format = "%Y-%m-%d"),
                                 casos_nuevos = col_number()))
stop_for_problems(Dat)
# Dat
# Añadir datos de población
# pal <- colorRampPalette(colors = c("#f7fcfd", "#e5f5f9",
#                                    "#ccece6", "#99d8c9",
#                                    "#66c2a4", "#41ae76",
#                                    "#238b45", "#006d2c",
#                                    "#00441b"))
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
  mutate(casos_100mil = casos_acumulados / (pob/1e5)) %>%
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
                       high = pal(11)[11], midpoint = 4,
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
