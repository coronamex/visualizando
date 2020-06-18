library(tidyverse)

#' Title
#'
#' @param Dat 
#' @param zonas_elegidas 
#' @param fecha_inicio 
#' @param fecha_final 
#'
#' @return
#' @export
#'
#' @examples
graficar_zms <- function(Dat, zonas_elegidas, fecha_inicio, fecha_final,
                         offset_totales = 20){
  # zonas_elegidas <- zonas_grupos[[2]]
  # zonas_elegidas
  # Zonas metropolitanas
  Dat <- Dat %>%
    filter(NOM_ZM %in% zonas_elegidas) %>%
    split(.$CVE_ZM) %>%
    map_dfr(function(d){
      d %>%
        mutate(casos = rolling_mean(sintomas_nuevos),
               muertes = rolling_mean(muertes_nuevas))
    }) 
  
  zm_tots <- Dat %>%
    group_by(NOM_ZM) %>%
    summarise(casos_totales = sum(sintomas_nuevos),
              muertes_totales = sum(muertes_nuevas),
              max_casos = max(sintomas_nuevos)) %>%
    ungroup()
  
  p1 <- Dat %>%
    filter(fecha >= fecha_inicio) %>%
    
    ggplot(aes(x = fecha)) +
    # facet_wrap(~ NOM_ZM, scales = "free_y") +
    geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                  ymin = -Inf, ymax = Inf,
                  fill = "pink")) +
    scale_fill_identity(guide = "legend", name = "", labels = "Casos en estas fechas pueden aumentar") +
    geom_bar(aes(y = sintomas_nuevos), width = 1, stat = "identity", color = "#4dac26", fill = "#4dac26") +
    geom_line(aes(y = casos), size = 2, col = "#b8e186") +
    # geom_text(data = zm_tots, aes(label = paste0("Casos: ", casos_totales, "\nMuertes: ", muertes_totales), 
    #                               y = 0.8 * max_casos), x = fecha_inicio + 25) +
    geom_text(data = zm_tots, aes(label = paste(casos_totales, "casos"),
                                  y = 0.9 * max_casos), x = fecha_inicio + offset_totales) +
    # geom_bar(aes(y = muertes_nuevas), width = 1, stat = "identity", color = "#4dac26", fill = "#4dac26") +
    # geom_line(aes(y = muertes), size = 2, col = "#b8e186") +
    # scale_fill_manual(name = "hola", values = "pink", labels = "Casos incompletos",
    #                   guide = guide_legend(override.aes=aes(fill=NA))) +
    # guides(fill = guide_legend(name = "", labels = "Casos en estas fechas pueden aumentar")) +
    ylab(label = "Número de nuevos casos") +
    # xlab(label = "Fecha de inicio de síntomas") +
    AMOR::theme_blackbox() +
    theme(axis.title = element_text(size = 20),
          axis.title.x = element_blank(),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 90),
          plot.margin = margin(l = 20, r = 20),
          panel.background = element_blank(),
          # panel.border = element_rect(fill=NA, colour = "black", size = 3),
          panel.border = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 12),
          legend.background = element_blank())
  
  if(length(zonas_elegidas) > 1){
    p1 <- p1 + facet_wrap(~ NOM_ZM, scales = "free_y")
  }
  
  p1
}

args <- list(serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv",
             municipios_lut = "../datos/util/municipios_lut_datos_abiertos.csv",
             # entidades_eliminar = c("09", "15"),
             dir_salida = "../sitio_hugo/static/imagenes/",
             zonas_metropolitanas = "../datos/util/zonas_metropolitanas_2015.csv")

lut_municipios <- read_csv(args$municipios_lut, col_names = FALSE,
                           col_types = cols(.default = col_character()))
stop_for_problems(lut_municipios)

lut_zm <- read_csv(args$zonas_metropolitanas,
                   col_types = cols(CVE_ZM = col_character(),
                                    NOM_ZM = col_character(),
                                    CVE_ENT = col_character(),
                                    NOM_ENT = col_character(),
                                    CVE_MUN = col_character()))
stop_for_problems(lut_zm)
lut_zm <- lut_zm %>%
  select(CVE_ZM, NOM_ZM, NOM_ENT, CVE_MUN, NOM_MUN, POB_2015)

Dat <- read_csv(args$serie_municipios,
                col_types = cols(municipio = col_character(),
                                 clave = col_character(),
                                 fecha = col_date(format = "%Y-%m-%d")))
stop_for_problems(Dat)

# Mapear municipios a zonas metropolitanas
lut <- lut_municipios %>%
  mutate(CVE_MUN = paste0(X5, X3)) %>%
  select(clave = X1, CVE_MUN, mun = X4, estado = X6) %>%
  left_join(lut_zm %>%
              select(CVE_ZM, NOM_ZM, CVE_MUN),
            by = c("CVE_MUN"))

# Calcular casos por zona metropolitana
Dat <- Dat %>%
  select(municipio, fecha, sintomas_nuevos, muertes_nuevas, clave) %>%
  left_join(lut, by = c("clave")) %>%
  filter(!is.na(CVE_ZM)) %>%
  group_by(fecha, CVE_ZM, NOM_ZM) %>%
  summarise(sintomas_nuevos = sum(sintomas_nuevos), muertes_nuevas = sum(muertes_nuevas)) %>%
  arrange(CVE_ZM, fecha) %>%
  ungroup()


# Dat %>%
#   split(.$CVE_ZM) %>%
#   map_dfr(function(d){
#     d %>%
#       arrange(fecha) %>%
#       mutate(sintomas_acumulados = cumsum(sintomas_nuevos)) 
#       # filter(sintomas_acumulados >= 500) %>%
#       # head(1)
#   }) %>%
#   # arrange(fecha) %>%
#   filter(fecha == "2020-05-15") %>%
#   arrange(desc(sintomas_acumulados)) %>%
#   print(n = 30)

# Elegir zonas por cantidad de casos en grupos
# tab_zm <- Dat %>%
#   group_by(CVE_ZM, NOM_ZM) %>%
#   summarise(casos_acumulados = sum(sintomas_nuevos),
#             muertes_acumuladas = sum(muertes_nuevas)) %>%
#   ungroup() %>%
#   arrange(desc(casos_acumulados)) %>%
#   print(n = 30)
zonas_grupos <- list(c("Valle de México"),
                     c("Tijuana",
                       "Villahermosa",
                       "Mexicali",
                       "Puebla-Tlaxcala",
                       "Culiacán",
                       "Veracruz",
                       "Cancún",
                       "Monterrey",
                       "Mérida"),
                     c("Toluca",
                       "Guadalajara",
                       "Juárez",
                       "Cuernavaca",
                       "Acapulco",
                       "Aguascalientes",
                       "Querétaro",
                       "Tampico",
                       "Oaxaca"),
                     c("León",
                       "Tuxtla Gutiérrez",
                       "La Laguna",
                       "Hermosillo",
                       "Pachuca",
                       "Mazatlán",
                       "Tlaxcala-Apizaco",
                       "San Luis Potosí",
                       "Coatzacoalcos"))


# zonas_elegidas <- Dat %>%
#   group_by(CVE_ZM, NOM_ZM) %>%
#   summarise(casos_acumulados = sum(sintomas_nuevos),
#             muertes_acumuladas = sum(muertes_nuevas)) %>%
#   arrange(desc(casos_acumulados)) %>%
#   print(n = 100) %>%
#   filter(NOM_ZM != "Valle de México") %>%
#   head(9) %>%
#   select(CVE_ZM) %>%
#   unlist

# Funcion de media suvizada
rolling_mean <- tibbletime::rollify(mean, window = 7, na_value = 0)
fecha_final <- max(Dat$fecha)
fecha_inicio <- parse_date("2020-03-01", format = "%Y-%m-%d")

# Valle de méxico
p1 <- Dat %>%
  filter(NOM_ZM %in% zonas_grupos[[1]]) %>%
  mutate(casos = rolling_mean(sintomas_nuevos),
         muertes = rolling_mean(muertes_nuevas)) %>%
  filter(fecha >= fecha_inicio) %>%
  
  ggplot(aes(x = fecha)) +
  geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  annotate("text",
           x = fecha_final - 7,
           y = 0.85 * max(Dat$sintomas_nuevos[Dat$NOM_ZM == "Valle de México"]),
           label = 'italic("Estos\nnúmeros\npueden\naumentar")',
           hjust = "middle",
           parse = TRUE) +
  annotate("text",
           x = fecha_inicio + 5,
           y = 0.90 * max(Dat$sintomas_nuevos[Dat$NOM_ZM == "Valle de México"]),
           label = paste(sum(Dat$sintomas_nuevos[Dat$NOM_ZM == "Valle de México"]), "casos"),
           hjust = "left",
           parse = FALSE,
           size = 6) +
  geom_bar(aes( y = sintomas_nuevos), width = 1, stat = "identity", color = "#4dac26", fill = "#4dac26") +
  geom_line(aes(y = casos), size = 2, col = "#b8e186") +
  ylab(label = "Número de nuevos casos") +
  xlab(label = "Fecha de inicio de síntomas") +
  ggtitle(label = "Valle de México") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.title = element_text(face = "bold", hjust = 0.5),
        # axis.text.x = element_text(angle = 90),
        plot.margin = margin(l = 20, r = 20, t = 10),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        # panel.border = element_blank(),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank())
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "vm_casos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "vm_casos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# graficar_zms(Dat = Dat, zonas_elegidas = zonas_grupos[[1]],
#              fecha_inicio = fecha_inicio,
#              fecha_final = fecha_final)

p1 <- graficar_zms(Dat = Dat, zonas_elegidas = zonas_grupos[[2]],
             fecha_inicio = fecha_inicio,
             fecha_final = fecha_final,
             offset_totales = 25)
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_zm_casos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_zm_casos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

p1 <- graficar_zms(Dat = Dat, zonas_elegidas = zonas_grupos[[3]],
             fecha_inicio = fecha_inicio,
             fecha_final = fecha_final,
             offset_totales = 25)
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_zm_casos2.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_zm_casos2@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


p1 <- graficar_zms(Dat = Dat, zonas_elegidas = zonas_grupos[[4]],
                   fecha_inicio = fecha_inicio,
                   fecha_final = fecha_final,
                   offset_totales = 25)
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_zm_casos3.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_zm_casos3@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


