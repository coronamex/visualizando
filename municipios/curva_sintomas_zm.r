library(tidyverse)

args <- list(serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv",
             municipios_lut = "../datos/util/municipios_lut_datos_abiertos.csv",
             # entidades_eliminar = c("09", "15"),
             dir_salida = "../sitio_hugo/static/imagenes/",
             zonas_metropolitanas = "../datos/util/zonas_metropolitanas_2015.csv")

lut_municipios <- read_csv(args$municipios_lut, col_names = FALSE)
stop_for_problems(lut_municipios)

lut_zm <- read_csv(args$zonas_metropolitanas)
stop_for_problems(lut_zm)
lut_zm <- lut_zm %>%
  select(CVE_ZM, NOM_ZM, NOM_ENT, CVE_MUN, NOM_MUN, POB_2015)

Dat <- read_csv(args$serie_municipios)
stop_for_problems(Dat)

# Mapear municipios a zonas metropolitanas
lut <- lut_municipios %>%
  mutate(CVE_MUN = paste0(X5, X3)) %>%
  select(clave = X1, CVE_MUN, mun = X4, estado = X6) %>%
  left_join(lut_zm %>%
              select(CVE_ZM, NOM_ZM, CVE_MUN),
            by = c("CVE_MUN"))
lut

# Calcular casos por zona metropolitana
Dat <- Dat %>%
  select(municipio, fecha, sintomas_nuevos, muertes_nuevas, clave) %>%
  left_join(lut, by = c("clave")) %>%
  filter(!is.na(CVE_ZM)) %>%
  group_by(fecha, CVE_ZM, NOM_ZM) %>%
  summarise(sintomas_nuevos = sum(sintomas_nuevos), muertes_nuevas = sum(muertes_nuevas)) %>%
  arrange(CVE_ZM, fecha) %>%
  ungroup()

# Elegir 9 zonas más afectadas
zonas_elegidas <- Dat %>%
  group_by(CVE_ZM, NOM_ZM) %>%
  summarise(casos_acumulados = sum(sintomas_nuevos),
            muertes_acumuladas = sum(muertes_nuevas)) %>%
  arrange(desc(casos_acumulados)) %>%
  print(n = 100) %>%
  filter(NOM_ZM != "Valle de México") %>%
  head(9) %>%
  select(CVE_ZM) %>%
  unlist

# Funcion de media suvizada
rolling_mean <- tibbletime::rollify(mean, window = 7, na_value = 0)
fecha_final <- max(Dat$fecha)

# Valle de méxico
p1 <- Dat %>%
  filter(NOM_ZM == "Valle de México") %>%
  mutate(casos = rolling_mean(sintomas_nuevos),
         muertes = rolling_mean(muertes_nuevas)) %>%
  
  filter(fecha >= "2020-03-01") %>%
  
  ggplot(aes(x = fecha)) +
  geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  annotate("text",
           x = fecha_final - 5,
           y = 0.85 * max(Dat$sintomas_nuevos),
           label = 'italic("Estos\nnúmeros\npueden\naumentar")',
           hjust = "middle",
           parse = TRUE) +
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
p1
archivo <- file.path(args$dir_salida, "vm_casos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "vm_casos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Zonas metropolitanas
p1 <- Dat %>%
  filter(CVE_ZM %in% zonas_elegidas) %>%
  split(.$CVE_ZM) %>%
  map_dfr(function(d){
    d %>%
      mutate(casos = rolling_mean(sintomas_nuevos),
             muertes = rolling_mean(muertes_nuevas))
  }) %>%
  
  filter(fecha >= "2020-03-01") %>%
  
  ggplot(aes(x = fecha)) +
  facet_wrap(~ NOM_ZM, scales = "free_y") +
  geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                ymin = -Inf, ymax = Inf,
                fill = "pink")) +
  scale_fill_identity(guide = "legend", name = "", labels = "Casos en estas fechas pueden aumentar") +
  geom_bar(aes(y= sintomas_nuevos), width = 1, stat = "identity", color = "#4dac26", fill = "#4dac26") +
  geom_line(aes(y = casos), size = 2, col = "#b8e186") +
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
p1
archivo <- file.path(args$dir_salida, "top_zm_casos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "top_zm_casos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# plots <- Dat %>%
#   split(.$municipio) %>%
#   map_dfr(function(d){
#     # d <- Dat %>%
#     #   filter(municipio == "Tijuana, Baja California")
#     
#     fecha_final <- max(d$fecha)
#     p1 <- d %>%
#       select(municipio, fecha, sintomas_nuevos, muertes_nuevas, estado, mun) %>%
#       mutate(casos = rolling_mean(sintomas_nuevos),
#              muertes = rolling_mean(muertes_nuevas)) %>%
#       
#       # print %>%
#       
#       ggplot(aes(x = fecha)) +
#       geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
#                     ymin = -Inf, ymax = Inf),
#                 fill = "pink") +
#       annotate("text",
#                x = fecha_final - 5,
#                y = 0.85 * max(d$sintomas_nuevos),
#                label = 'italic("Estos\nnúmeros\npueden\naumentar")',
#                hjust = "middle",
#                parse = TRUE) +
#       geom_bar(aes(y= sintomas_nuevos), width = 1, stat = "identity", color = "#4dac26", fill = "#4dac26") +
#       geom_line(aes(y = casos), size = 3, color = "#b8e186") +
#       ylab(label = "Número de nuevos casos") +
#       xlab(label = "Fecha de inicio de síntomas") +
#       ggtitle(label = unique(d$municipio)) +
#       AMOR::theme_blackbox() +
#       theme(axis.title = element_text(size = 20),
#             axis.text = element_text(size = 10),
#             plot.margin = margin(l = 20, r = 20),
#             panel.background = element_blank(),
#             panel.border = element_rect(fill=NA, colour = "black", size = 3),
#             legend.position = "top",
#             legend.text = element_text(size = 12),
#             legend.background = element_blank())
#     
#     # archivo <- file.path(args$dir_salida, paste0("municipio_", paste(unique(d$estado), unique(d$mun), sep = "."), ".png"))
#     # ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
#     # archivo <- file.path(args$dir_salida, paste0("municipio_", paste(unique(d$estado), unique(d$mun), sep = "."), "@2x.png"))
#     # ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
#     
#     p1$data
#   })

# fecha_final <- max(plots$fecha)
# p1 <- plots %>%
#   filter(fecha >= "2020-03-01") %>%
#   ggplot(aes(x = fecha)) +
#   facet_wrap(~ municipio) +
#   geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
#                 ymin = -Inf, ymax = Inf),
#             fill = "pink") +
#   geom_bar(aes(y= sintomas_nuevos), width = 1, stat = "identity", color = "#4dac26", fill = "#4dac26") +
#   geom_line(aes(y = casos), size = 2, col = "#b8e186") +
#   ylab(label = "Número de nuevos casos") +
#   xlab(label = "Fecha de inicio de síntomas") +
#   AMOR::theme_blackbox() +
#   theme(axis.title = element_text(size = 20),
#         axis.text = element_text(size = 10),
#         axis.text.x = element_text(angle = 90),
#         plot.margin = margin(l = 20, r = 20),
#         panel.background = element_blank(),
#         # panel.border = element_rect(fill=NA, colour = "black", size = 3),
#         panel.border = element_blank(),
#         legend.position = "top",
#         legend.text = element_text(size = 12),
#         legend.background = element_blank())
# p1
# archivo <- file.path(args$dir_salida, "top_municipios_casos.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "top_municipios_casos@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


