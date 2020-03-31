library(tidyverse)


Tab <- read_csv("../datos/ssa_dge/tabla_casos_confirmados.csv",
                col_types = cols(estado = col_character(),
                                 sexo = col_character(),
                                 edad = col_number(),
                                 fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                 procedencia = col_character(),
                                 fecha_llegada = col_date(format = "%d/%m/%Y")))
# Tab
# Tab %>%
#   arrange(desc(fecha_sintomas))
fecha_inicio <- min(Tab$fecha_sintomas) - 0.5
fecha_final <- max(Tab$fecha_sintomas) + 0.5
# fecha_inicio
# fecha_final

p1 <- ggplot(Tab, aes(x = fecha_sintomas)) +
  geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  geom_bar(aes(y=..count..)) +
  xlim(c(fecha_inicio, fecha_final)) +
  ylab(label = "Número de casos") +
  xlab(label = "Fecha de inicio de síntomas") +
  theme(panel.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 2))
p1

w1 <- plotly::ggplotly(p1)
w1
htmlwidgets::saveWidget(plotly::as_widget(w1), "inicion_sintomas_por_fecha.html")


p1 <- ggplot(Tab, aes(x = fecha_sintomas, y = estado)) +
  stat_bin2d(aes(fill = ..count..)) +
  theme(panel.background = element_blank(),
        panel.grid = element_line(color = "black"))
p1


p1 <- ggplot(Tab, aes(x = fecha_sintomas, y = estado)) +
  stat_bin2d()
p1



aggregate(sexo ~ estado + fecha_sintomas, data = Tab, FUN = length)


Tab %>%
  pivot_wider(id_cols = c("estado", "fecha_sintomas"),
              values_from = "sexo",
              values_fill = list(sexo = 0))

labeller <- c(`FALSE` = "Probablemente estable", `TRUE` = "Probablemente cambiará")

p1 <- Tab %>%
  group_by(estado, fecha_sintomas) %>%
  summarize(casos = n()) %>%
  ungroup %>%
  mutate(estado = factor(estado, levels = rev(unique(estado)))) %>%
  mutate(incompleto = fecha_sintomas > (max(fecha_sintomas) - 15)) %>%
  ggplot(aes(x = fecha_sintomas, y = estado, fill = casos)) +
  facet_grid(~ incompleto, scales = "free_x", space = "free",labeller = as_labeller(labeller)) +
  geom_tile() +
  ylab(label = "Entidad federativa") +
  xlab(label = "Fecha de inicio de síntomas") +
  scale_fill_gradient2(low = "#313695",
                       mid = "#ffffbf",
                       high = "#a50026",
                       midpoint = 20) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90))
p1
ggsave("incio_sintomas_por_estado_y_fecha.jpeg", p1, width = 10, height = 6, dpi = 150)
