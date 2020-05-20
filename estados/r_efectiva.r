library(tidyverse)

args <- list(tabla_estimados = "estimados/rt_live_estimados.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")

Dat <- read_csv(args$tabla_estimados)

p1 <- Dat %>%
  ggplot(aes(x = date, y = median)) +

  facet_wrap(~ estado, ncol = 5) +
  geom_ribbon(aes(ymin = lower_90, ymax = upper_90), color = "blue", alpha = 0.2) +
  geom_line(size = 2, col = "blue") +
  geom_hline(yintercept = 1) +
  ylab("Promedio de contagios por enfermo de COVID-19 (R_t)") +
  # xlab("Fecha") +
  # AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top",
        axis.title = element_text(face = "bold", size = 12),
        plot.margin = margin(l = 20, r = 20, b = 20),
        strip.text = element_text(face = "bold"))
# p1
# archivo <- file.path(args$dir_salida, "r_efectiva.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "r_efectiva@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "r_efectiva.png")
ggsave(archivo, p1, width = 7, height = 9.5, dpi = 75)
archivo <- file.path(args$dir_salida, "r_efectiva@2x.png")
ggsave(archivo, p1, width = 7, height = 9.5, dpi = 150)



# p1 <- Dat %>%
#   ggplot(aes(x = date, y = median, group = estado)) +
#   
#   facet_wrap(~ estado, ncol = 4) +
#   geom_ribbon(aes(ymin = lower_90, ymax = upper_90), color = "blue", alpha = 0.2) +
#   geom_line(size = 2, col = "blue") +
#   geom_hline(yintercept = 1) +
#   ylab("Promedio de contagios por enfermo de COVID-19 (R_t)") +
#   # xlab("Fecha") +
#   # AMOR::theme_blackbox() +
#   theme(panel.background = element_blank(),
#         axis.text = element_text(size = 10),
#         axis.text.x = element_text(angle = 90),
#         axis.ticks.x = element_blank(),
#         axis.title.x = element_blank(),
#         legend.position = "top",
#         axis.title = element_text(face = "bold", size = 12),
#         plot.margin = margin(l = 20, r = 20, b = 20),
#         strip.text = element_text(face = "bold"))
# p1
# p1 <- plotly::ggplotly(p1)
# p1
# htmlwidgets::saveWidget(p1, "test.html")
