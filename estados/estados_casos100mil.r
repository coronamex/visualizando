library(tidyverse)

args <- list(dge_dir = "../datos/ssa_dge/",
             poblacion = "../datos/demograficos/pob_estado.tsv",
             tasa_min = 0,
             dir_salida = "../sitio_hugo/static/imagenes/",
             max_dias = 10,
             archivo_estados = "../datos/ssa_dge/datos_mapa.csv")

# Leer poblaciones
pob <- read_tsv(args$poblacion)
pob

# situacion_estados <- read_csv(args$archivo_estados)
# estados_nombres_situacion <- set_names(sort(situacion_estados$estado), sort(situacion_estados$estado))
# estados_nombres_situacion["Queretaro"] <- "Querétaro"
# situacion_estados <- situacion_estados %>%
#   mutate(estado = as.character(estados_nombres_situacion[match(estado, names(estados_nombres_situacion))]))
# situacion_estados <- situacion_estados %>% left_join(pob, by = "estado") %>%
#   select(estado, muertes_acumuladas, poblacion_2015) %>%
#   mutate(mortalidad = 100000*muertes_acumuladas/poblacion_2015)

fechas_dirs <- list.dirs(args$dge_dir, recursive = FALSE, full.names = TRUE)
Dat <- fechas_dirs %>%
  map_dfr(function(fecha_dir){
    # fecha_dir <- "../datos/ssa_dge/2020-04-06/"
    archivo_tabla <- file.path(fecha_dir, "tabla_casos_confirmados.csv")
    if(file.exists(archivo_tabla)){
      Tab <- read_csv(archivo_tabla,
                      col_types = cols(estado = col_character(),
                                       sexo = col_character(),
                                       edad = col_number(),
                                       fecha_sintomas = col_date(format = "%Y-%m-%d"),
                                       procedencia = col_character(),
                                       fecha_llegada = col_date(format = "%Y-%m-%d")))
      stop_for_problems(Tab)
      fecha <- basename(fecha_dir) %>% as.Date()
      acum_estado <- table(Tab$estado)
      res <- tibble(estado = names(acum_estado),
                    casos_acumulados = as.numeric(acum_estado),
                    fecha = fecha)
      
      return(res)
    }
  })

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
  left_join(pob, by = "estado") %>%
  mutate(casos_100mil = casos_acumulados / (poblacion_2015/1e5)) %>%
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
