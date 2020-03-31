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
