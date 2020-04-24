library(tidyverse)

args <- list(min_casos = 60,
             dias_ventana = 7,
             # tabla_mx = "../datos/ssa_dge_2020-04-19//reportes_diarios.csv",
             tabla_mx = "../datos/datos_abiertos/serie_tiempo_nacional_fecha_confirmacion.csv",
             serie_tiempo_casos_mundo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
             serie_tiempo_muertes_mundo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

lut_paises <- set_names(c("EEUU", "España", "Italia",
                          "Irán", "China", "Francia",
                          "Brasil", "Corea del Sur",
                          "Japón"),
                        c("US", "Spain", "Italy", 
                          "Iran", "China", "France",
                          "Brazil", "Korea, South",
                          "Japan"))

# Leer daots México
datos_mx <- read_csv(args$tabla_mx,
                     col_types = cols(fecha = col_date(format = "%Y-%m-%d")))
stop_for_problems(datos_mx)
datos_mx$pais <- "México"
datos_mx

# Leer casos mundo
casos_mundo <- read_csv(args$serie_tiempo_casos_mundo,
                        col_types = cols("Province/State" = col_character(),
                                         "Country/Region" = col_character(),
                                         .default = col_number()))
stop_for_problems(casos_mundo)
casos_mundo <- casos_mundo %>%
  filter(`Country/Region` %in% names(lut_paises)) %>%
  filter(is.na(`Province/State`)) %>%
  mutate(pais = `Country/Region`) %>%
  mutate(pais = lut_paises[pais]) %>%
  select(-Lat, -Long, -"Province/State", -"Country/Region") %>%
  pivot_longer(-pais, names_to = "fecha", values_to = "casos_acumulados") %>%
  mutate(fecha = parse_date(fecha, format = "%m/%d/%y")) 

# Leer muertes mundo
muertes_mundo <- read_csv(args$serie_tiempo_muertes_mundo,
                        col_types = cols("Province/State" = col_character(),
                                         "Country/Region" = col_character(),
                                         .default = col_number()))
stop_for_problems(muertes_mundo)
muertes_mundo <- muertes_mundo %>%
  filter(`Country/Region` %in% names(lut_paises)) %>%
  filter(is.na(`Province/State`)) %>%
  mutate(pais = `Country/Region`) %>%
  mutate(pais = lut_paises[pais]) %>%
  select(-Lat, -Long, -"Province/State", -"Country/Region") %>%
  pivot_longer(-pais, names_to = "fecha", values_to = "muertes_acumuladas") %>%
  mutate(fecha = parse_date(fecha, format = "%m/%d/%y")) 

# Combinar casos y muertes
datos_mundo <- casos_mundo %>%
  full_join(muertes_mundo, by = c("pais", "fecha"))

# Calcular casos por día
datos_mundo <- datos_mundo %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d <- d %>%
      arrange(fecha) %>%
      mutate(casos_nuevos = casos_acumulados - lag(casos_acumulados, 1, default = 0),
             muertes_nuevas = muertes_acumuladas - lag(muertes_acumuladas, 1, default = 0))
    
    d
  })
datos_mundo

# Combinar datos
Dat <- datos_mundo %>%
  bind_rows(datos_mx %>%
              select(pais, fecha,
                     casos_acumulados, muertes_acumuladas,
                     casos_nuevos, muertes_nuevas))
Dat

# Casos acumukados
p1 <- Dat %>%
  filter(pais != "China") %>%
  filter(casos_acumulados >= args$min_casos) %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d$dia <- as.numeric(d$fecha - min(d$fecha) + 1)
    d
  }) %>%
  ggplot(aes(x = dia, y = casos_acumulados, group = pais)) +
  geom_line(aes(col = pais, size = pais)) +
  scale_color_brewer(palette = "Paired", name = "") +
  scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "") +
  scale_y_log10(labels = scales::comma) +
  ylab("Total de casos confirmados") +
  xlab("Días desde el caso 60") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "casos_acumulados_por_dia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_acumulados_por_dia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- Dat %>%
  filter(pais != "China") %>%
  filter(casos_acumulados >= args$min_casos) %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d$dia <- as.numeric(d$fecha - min(d$fecha) + 1)
    d
  }) %>%
  ggplot(aes(x = dia, y = muertes_acumuladas, group = pais)) +
  geom_line(aes(col = pais, size = pais)) +
  scale_color_brewer(palette = "Paired", name = "") +
  scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "") +
  scale_y_log10(labels = scales::comma) +
  ylab("Total de muertes") +
  xlab("Días desde el caso 60") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "muertes_acumuladas_por_dia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_acumuladas_por_dia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Mejor curva
incremento <- function(x){
  res <- x[-1] / x[-length(x)]
  res <- res[!is.na(res)]
  res <- res[!is.infinite(res)]
  res <- (res - 1)
  mean(res)
}
roll_incremento <- tibbletime::rollify(incremento, window = args$dias_ventana + 1)
p1 <- Dat %>%
  filter(pais != "China") %>%
  split(.$pais) %>%
  map_dfr(function(d, min_casos = 60, dias_ventana = 7){
    # Asegurarse de que los datos estén ordenaados por fecha
    d <- d %>%
      arrange(fecha)
    
    # Elegir los días necesarios
    ii <- which(d$casos_acumulados >= min_casos)
    ii <- c(max(1,min(ii) - dias_ventana):(min(ii) -1), ii)
    d <- d[ii,]
    d$dia <- -(dias_ventana - 1):(nrow(d) - dias_ventana)
    
    # Calcular incremento
    d %>%
      mutate(crecimiento = roll_incremento(casos_acumulados))
  }, min_casos = args$min_casos, dias_ventana = args$dias_ventana) %>%
  filter(!is.na(crecimiento)) %>%
  filter(casos_acumulados >= 1000) %>%
  
  select(pais, casos_acumulados, crecimiento,) %>%
  ggplot(aes(x = casos_acumulados, y = crecimiento, group = pais)) +
  geom_line(aes(col = pais, size = pais)) +
  scale_color_brewer(palette = "Paired", name = "") +
  scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "") +
  # scale_y_log10(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_log10(labels = scales::comma) +
  ylab("Incremento (%)") +
  xlab("Total de casos confirmados") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "incremento_por_casos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "incremento_por_casos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Casos vs muertes.para segunda versión
# p1 <- Dat %>%
#   filter(pais != "China") %>%
#   filter(casos_acumulados >= args$min_casos) %>%
#   ggplot(aes(x = casos_acumulados, y = muertes_acumuladas)) +
#   geom_line(aes(col = pais, size = pais)) +
#   scale_color_brewer(palette = "Paired", name = "País") +
#   scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "País") +
#   scale_y_log10(labels = scales::comma) +
#   scale_x_log10(labels = scales::comma) +
#   ylab("Total de muertes") +
#   xlab("Total de casos confirmados") +
#   AMOR::theme_blackbox()
# p1



