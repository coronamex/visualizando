library(tidyverse)

args <- list(mundo_dir = "../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/",
             min_casos = 60,
             tabla_mx = "../datos/ssa_dge/reportes_diarios.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

paises <- c("US", "Spain", "Italy", "Iran", "China",
            "France", "Brazil", "South Korea", "Japan")
nuevos_nombres <- c("EEUU", "España", "Italia", "Irán",
                    "China", "Francia", "Brasil", "Corea del Sur",
                    "Japón")

# Leer daots México
datos_mx <- read_csv(args$tabla_mx)
datos_mx$pais <- "México"

# Leer datos mundiales de John Hopkins
datos_mundo <- list.files(args$mundo_dir, full.names = TRUE) %>%
  str_subset("[.]md$", negate = TRUE) %>%
  map_dfr(function(file){
    # file <- list.files(args$mundo_dir, full.names = TRUE)[1]
    fecha <- basename(file) %>% str_remove("[.]csv$")
    fecha <- strptime(fecha, "%m-%d-%Y") %>% format("%Y-%m-%d") %>% as.Date()
    
    tab <- read_csv(file)
    names(tab)[names(tab) == "Country/Region"] <- "Country_Region"
    tab <- tab %>%
      mutate(Country_Region = replace(Country_Region, Country_Region == "Korea, South", "South Korea")) %>%
      mutate(Country_Region = replace(Country_Region, Country_Region == "Mainland China", "China"))
    
    tab %>%
      filter(Country_Region %in% paises) %>%
      split(.$Country_Region) %>%
      map_dfr(function(d){
        tibble(casos_acumulados = sum(d$Confirmed, na.rm = TRUE),
               muertes_acumuladas = sum(d$Deaths, na.rm = TRUE))
      }, .id = "pais") %>%
      mutate(fecha = fecha)
    
  })

# Calcular casos por día
datos_mundo <- datos_mundo %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d <- d %>%
      arrange(fecha)
    d <- d %>%
      bind_cols(casos_nuevos = c(NA, diff(d$casos_acumulados)),
                muertes_nuevas = c(NA, diff(d$muertes_acumuladas)))
  })

# Reemplazar nombres
for(i in 1:length(paises)){
  datos_mundo <- datos_mundo %>%
    mutate(pais = replace(pais, pais == paises[i], nuevos_nombres[i]))
}

# Combinar datos
Dat <- datos_mundo %>%
  bind_rows(datos_mx %>%
              select(pais, casos_acumulados, muertes_acumuladas,
                     fecha, casos_nuevos, muertes_nuevas))


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
  scale_color_brewer(palette = "Paired", name = "País") +
  scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "País") +
  scale_y_log10() +
  ylab("Total de casos confirmados") +
  xlab("Días desde el caso 60") +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "casos_acumulados_por_dia.jpeg")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_acumulados_por_dia@2x.jpeg")
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
  scale_color_brewer(palette = "Paired", name = "País") +
  scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "País") +
  scale_y_log10() +
  ylab("Total de muertes") +
  xlab("Días desde el caso 60") +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
p1
archivo <- file.path(args$dir_salida, "muertes_acumuladas_por_dia.jpeg")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_acumuladas_por_dia@2x.jpeg")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Casos vs muertes.para segunda versión
# p1 <- Dat %>%
#   filter(pais != "China") %>%
#   filter(casos_acumulados >= args$min_casos) %>%
#   ggplot(aes(x = casos_acumulados, y = muertes_acumuladas)) +
#   geom_line(aes(col = pais, size = pais)) +
#   scale_color_brewer(palette = "Paired", name = "País") +
#   scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "País") +
#   scale_y_log10() +
#   scale_x_log10() +
#   ylab("Total de muertes") +
#   xlab("Total de casos confirmados") +
#   AMOR::theme_blackbox()
# p1

