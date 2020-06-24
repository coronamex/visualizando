library(tidyverse)

args <- list(poblacion = "../datos/demograficos/pob_estado.tsv",
             tasa_min = 0,
             dir_salida = "../sitio_hugo/static/imagenes/",
             max_dias = 10,
             dias_activos = 14,
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
Dat <- Dat %>%
  left_join(pob %>% select(estado, pob = conapo_2020), 
            by = "estado") %>%
  mutate(casos_100mil = sintomas_acumulados / (pob/1e5)) %>%
  split(.$estado) %>%
  map_dfr(function(d, tasa_min){
    d <- d %>%
      mutate(casos_recientes = sintomas_acumulados - lag(sintomas_acumulados, args$dias_activos, default = 0)) %>%
      mutate(recientes = casos_recientes / (pob/1e5))
    
    if(max(d$sintomas_acumulados >= tasa_min) || d$estado[1] %in% estados){
      return(d)
    }
  }, tasa_min = args$tasa_min) %>%
  select(estado, acumulados = casos_100mil, recientes, fecha) %>%
  filter(fecha > max(fecha) - args$max_dias) %>%
  pivot_longer(cols = c(-estado, -fecha), names_to = "grupo", values_to = "casos_100mil") %>%
  filter(!(grupo == "recientes" & fecha < max(fecha))) %>%
  mutate(estado = factor(estado, levels = rev(unique(estado))))
# Dat

p1 <- Dat %>%
  filter(grupo == "acumulados") %>%
  
  ggplot(aes(x = fecha, y = estado)) +
  geom_tile(aes(fill = casos_100mil), width = 0.8, height = 0.8) +
  # facet_grid(~ grupo, scales = "free_x", space = "free_x") +
  scale_fill_gradient2(low = pal(11)[1], mid = pal(11)[6],
                       high = pal(11)[11], midpoint = 20,
                       name = expression(frac("Casos totales", "100 mil habitantes"))) +
  scale_x_date(breaks = unique(Dat$fecha), labels = function(x){strftime(x, format = "%b %d")}) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90),
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        plot.margin = margin(l = 20))

p2 <- Dat %>%
  filter(grupo == "recientes") %>%
  
  ggplot(aes(x = casos_100mil, y = estado)) +
  # geom_bar(stat = "identity") +
  geom_col() +
  geom_vline(xintercept = 20, col = "red", size = 1) +
  xlab(label = expression(frac("Casos recientes", "100 mil habitantes"))) +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "top",
        plot.margin = margin(r = 20))

pp <- cowplot::plot_grid(p1, p2 + aplot::ylim2(p1), axis = "bt", align = "h", rel_widths = c(0.8, 0.2))
# ggsave("test.png", pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_100mil_estados.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_100mil_estados@2x.png")
ggsave(archivo, pp, width = 7, height = 6.7, dpi = 150)
