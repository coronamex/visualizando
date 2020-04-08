library(tidyverse)

args <- list(tabla_sintomas = "../datos/ssa_dge/tabla_casos_confirmados.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

Tab <- read_csv(args$tabla_sintomas,
                col_types = cols(estado = col_character(),
                                 sexo = col_character(),
                                 edad = col_number(),
                                 fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                 procedencia = col_character()))

# Todo el país
fecha_inicio <- min(Tab$fecha_sintomas) - 0.5
fecha_final <- max(Tab$fecha_sintomas) + 0.5
p1 <- ggplot(Tab, aes(x = fecha_sintomas)) +
  geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  geom_bar(aes(y=..count..)) +
  annotate("text",
           x = fecha_final - 4,
           y = 0.85 * max(table(Tab$fecha_sintomas)),
           label = 'italic("Estos\nnúmeros\npueden\naumentar")',
           hjust = "middle",
           parse = TRUE) +
  xlim(c(fecha_inicio, fecha_final)) +
  ylab(label = "Número de casos") +
  xlab(label = "Fecha de inicio de síntomas") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3))
p1
archivo <- file.path(args$dir_salida, "inicio_sintomas_por_fecha_nacional.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "inicio_sintomas_por_fecha_nacional@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


# # Tabla por estado (para segunda versión)
# fecha_inicio <- min(Tab$fecha_sintomas) - 0.5
# fecha_final <- max(Tab$fecha_sintomas) + 0.5
# labeller <- c(`FALSE` = "Probablemente estable", `TRUE` = "Probablemente cambiará")
# p1 <- Tab %>%
#   group_by(estado, fecha_sintomas) %>%
#   summarize(casos = n()) %>%
#   ungroup %>%
#   mutate(estado = factor(estado, levels = rev(unique(estado)))) %>%
#   mutate(incompleto = fecha_sintomas > (max(fecha_sintomas) - 15)) %>%
#   filter(fecha_sintomas > fecha_inicio & fecha_sintomas < fecha_final) %>%
#   ggplot(aes(x = fecha_sintomas, y = estado, fill = casos)) +
#   facet_grid(~ incompleto, scales = "free_x", space = "free",labeller = as_labeller(labeller)) +
#   geom_tile() +
#   ylab(label = "Entidad federativa") +
#   xlab(label = "Fecha de inicio de síntomas") +
#   scale_fill_gradient2(low = "#313695",
#                        mid = "#ffffbf",
#                        high = "#a50026",
#                        midpoint = 30) +
#   theme(panel.background = element_blank(),
#         axis.text.x = element_text(angle = 0))
# ggsave("inicio_sintomas_por_fecha_estado.png", p1, width = 10, height = 6, dpi = 150)

