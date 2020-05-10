library(tidyverse)
library(ggmuller)

args <- list(serie = "../datos/datos_abiertos/serie_tiempo_estados_um_confirmados.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")


Dat <- read_csv(args$serie)

Dat <- Dat %>%
  select(fecha, muertes = muertes_nuevas, estado) %>%
  mutate(region = estado) %>%
  mutate(region = replace(region, region %in% c("Baja California",
                                                "Baja California Sur",
                                                "Chihuahua",
                                                "Sinaloa",
                                                "Durango",
                                                "Sonora"),
                          "Noroeste")) %>%
  mutate(region = replace(region, region %in% c("Campeche",
                                                "Quintana Roo",
                                                "Yucatán",
                                                "Tabasco"),
                          "Sureste")) %>%
  mutate(region = replace(region,
                          region %in% c("Colima",
                                        "Jalisco",
                                        "Michoacán",
                                        "Nayarit"),
                          "Occidente")) %>%
  mutate(region = replace(region,
                          region %in% c("Hidalgo",
                                        "Puebla",
                                        "Tlaxcala",
                                        "Veracruz"),
                          "Oriente")) %>%
  mutate(region = replace(region,
                          region %in% c("Coahuila",
                                        "Nuevo León",
                                        "Tamaulipas"),
                          "Noreste")) %>%
  mutate(region = replace(region,
                          region %in% c("Aguascalientes",
                                        "Guanajuato",
                                        "Querétaro",
                                        "San Luis Potosí",
                                        "Zacatecas"),
                          "Centronorte")) %>%
  mutate(region = replace(region,
                          region %in% c("Ciudad de México",
                                        "Morelos",
                                        "México"),
                          "Centrosur")) %>%
  mutate(region = replace(region,
                          region %in% c("Chiapas",
                                        "Oaxaca",
                                        "Guerrero"),
                          "Suroeste")) %>%
  group_by(fecha, region) %>%
  summarise(muertes = sum(muertes)) %>%
  ungroup() %>%

  # filter(muertes > 0) %>%
  filter(fecha >= "2020-03-18")
Dat

adj <- tibble(Parent = "0", Identity = unique(Dat$region))
dat <- Dat %>%
  transmute(Generation = as.numeric(fecha - min(fecha)),
            Identity = region,
            Population = muertes) %>%
  arrange(Generation) %>%
  bind_rows(tibble(Generation = 0, Identity = "0", Population = 0))
dat <- get_Muller_df(adj, dat)
dat <- dat %>%
  filter(Identity != "0") %>%
  add_empty_pop() %>%
  transmute(fecha = min(Dat$fecha) + Generation,
            muertes = Population,
            grupo = Group_id,
            region = Identity)

max_muertes_diarias <- dat %>%
  group_by(fecha) %>%
  summarize(muertes = sum(muertes)) %>%
  select(muertes) %>%
  max()

p1 <- dat %>%
  ggplot(aes(x = fecha, y = muertes, group = grupo)) +
  
  geom_rect(aes(xmin = max(fecha) - 15, xmax = max(fecha),
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  annotate("text",
           x = max(dat$fecha) - 7,
           y = 0.97 * max_muertes_diarias,
           label = 'italic("Estos números\npueden aumentar")',
           hjust = "middle",
           parse = TRUE) +
  
  geom_area(aes(fill = region)) +
  scale_fill_brewer(type = "qual", breaks = unique(Dat$region), name = "") +
  guides(fill = guide_legend(nrow = 2)) +
  
  geom_segment(x = parse_date("2020-03-20"), xend = parse_date("2020-03-20"), y = 90, yend = 140) +
  annotate("text",
           x = parse_date("2020-03-20") - 1.5,
           y = 115,
           size = 6,
           angle = 90,
           label = 'italic("50 fallecimientos")',
           hjust = "middle",
           parse = TRUE) +
  
  # ylab(label = paste(max_muertes_diarias, "muertes")) +
  ylab(label = "Muertes nuevas por día") +
  xlab(label = "Fecha de defunción") +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),

        plot.margin = margin(l = 20, r = 20))
# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_region.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_region@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)