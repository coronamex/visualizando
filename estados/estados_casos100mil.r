library(tidyverse)

args <- list(dge_dir = "../datos/ssa_dge/",
             poblacion = "../datos/demograficos/2015_pob_estado.tsv",
             tasa_min = 0,
             dir_salida = "../sitio_hugo/static/imagenes/",
             max_dias = 10,
             archivo_estados = "../datos/ssa_dge/datos_mapa.csv")
estados_nombres_dge <- c(AGUASCALIENTES = "Aguascalientes",
                         `BAJA CALIFORNIA` = "Baja California",
                         `BAJA CALIFORNIA SUR` = "Baja California Sur",
                         "BAJA CALIFORNIA \nSUR" = "Baja California Sur",
                         `CAMPECHE` = "Campeche",
                         `CIUDAD DE MÉXICO` = "Ciudad de México",
                         `DISTRITO FEDERAL` = "Ciudad de México",
                         COAHUILA = "Coahuila",
                         COLIMA = "Colima",
                         CHIAPAS = "Chiapas",
                         CHIHUAHUA = "Chihuahua",
                         DURANGO = "Durango",
                         GUANAJUATO = "Guanajuato",
                         GUERRERO = "Guerrero",
                         HIDALGO = "Hidalgo",
                         JALISCO = "Jalisco",
                         `MÉXICO` = "México",
                         `MEXICO` = "México",
                         `MICHOACÁN` = "Michoacán",
                         `MICHOACAN` = "Michoacán",
                         MORELOS = "Morelos",
                         NAYARIT = "Nayarit",
                         `NUEVO LEÓN` = "Nuevo León",
                         `NUEVO LEON` = "Nuevo León",
                         OAXACA = "Oaxaca",
                         PUEBLA = "Puebla",
                         QUERETARO = "Querétaro",
                         `QUINTANA ROO` = "Quintana Roo",
                         `SAN LUIS POTOSÍ` = "San Luis Potosí",
                         `SAN LUIS POTOSI` = "San Luis Potosí",
                         SINALOA = "Sinaloa",
                         SONORA = "Sonora",
                         TABASCO = "Tabasco",
                         TAMAULIPAS = "Tamaulipas",
                         TLAXCALA = "Tlaxcala",
                         VERACRUZ = "Veracruz",
                         `YUCATÁN` = "Yucatán",
                         `YUCATAN` = "Yucatán",
                         ZACATECAS = "Zacatecas")

# Leer poblaciones
pob <- read_tsv(args$poblacion)
estados_nombres_pob <- set_names(sort(pob$estado), sort(pob$estado))
estados_nombres_pob["Coahuila de Zaragoza"] <- "Coahuila"
estados_nombres_pob["Estado de México"] <- "México"
pob <- pob %>%
  mutate(estado = as.character(estados_nombres_pob[match(estado, names(estados_nombres_pob))]))

situacion_estados <- read_csv(args$archivo_estados)
estados_nombres_situacion <- set_names(sort(situacion_estados$estado), sort(situacion_estados$estado))
estados_nombres_situacion["Queretaro"] <- "Querétaro"
situacion_estados <- situacion_estados %>%
  mutate(estado = as.character(estados_nombres_situacion[match(estado, names(estados_nombres_situacion))]))
situacion_estados <- situacion_estados %>% left_join(pob, by = "estado") %>%
  select(estado, muertes_acumuladas, poblacion_2015) %>%
  mutate(mortalidad = 100000*muertes_acumuladas/poblacion_2015)

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
                                       fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                       procedencia = col_character()))
      fecha <- basename(fecha_dir) %>% as.Date()
      acum_estado <- table(Tab$estado)
      res <- tibble(estado = names(acum_estado),
                    casos_acumulados = as.numeric(acum_estado),
                    fecha = fecha)
      
      return(res)
    }
  })
# Dat <- res
# Renombrar estados
Dat <- Dat %>%
  mutate(estado = as.character(estados_nombres_dge[match(estado, names(estados_nombres_dge))]))
# Dat %>% print(n=100)

# Añadir datos de población
pal <- colorRampPalette(colors = c("#f7fcfd", "#e5f5f9",
                                   "#ccece6", "#99d8c9",
                                   "#66c2a4", "#41ae76",
                                   "#238b45", "#006d2c",
                                   "#00441b"))
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
                       high = pal(11)[11], midpoint = 3,
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
