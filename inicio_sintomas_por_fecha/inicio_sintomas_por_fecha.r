library(tidyverse)

Tab <- read_csv("../datos/ssa_dge/tabla_casos_confirmados.csv",
                col_types = cols(estado = col_character(),
                                 sexo = col_character(),
                                 edad = col_number(),
                                 fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                 procedencia = col_character(),
                                 fecha_llegada = col_date(format = "%d/%m/%Y")))

# Todo el país
fecha_inicio <- min(Tab$fecha_sintomas) - 0.5
fecha_final <- max(Tab$fecha_sintomas) + 0.5
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
ggsave("inicio_sintomas_por_fecha_nacional.jpeg", p1, width = 10, height = 6, dpi = 150)

# Tabla por estado
fecha_inicio <- min(Tab$fecha_sintomas) - 0.5
fecha_final <- max(Tab$fecha_sintomas) + 0.5
labeller <- c(`FALSE` = "Probablemente estable", `TRUE` = "Probablemente cambiará")
p1 <- Tab %>%
  group_by(estado, fecha_sintomas) %>%
  summarize(casos = n()) %>%
  ungroup %>%
  mutate(estado = factor(estado, levels = rev(unique(estado)))) %>%
  mutate(incompleto = fecha_sintomas > (max(fecha_sintomas) - 15)) %>%
  filter(fecha_sintomas > fecha_inicio & fecha_sintomas < fecha_final) %>%
  ggplot(aes(x = fecha_sintomas, y = estado, fill = casos)) +
  facet_grid(~ incompleto, scales = "free_x", space = "free",labeller = as_labeller(labeller)) +
  geom_tile() +
  ylab(label = "Entidad federativa") +
  xlab(label = "Fecha de inicio de síntomas") +
  scale_fill_gradient2(low = "#313695",
                       mid = "#ffffbf",
                       high = "#a50026",
                       midpoint = 30) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 0))
ggsave("inicio_sintomas_por_fecha_estado.jpeg", p1, width = 10, height = 6, dpi = 150)
