# (C) Copyright 2020 Sur Herrera Paredes

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License

library(tidyverse)
library(ggmuller)

args <- list(serie = "../datos/datos_abiertos/serie_tiempo_estados_um_confirmados.csv.gz",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Muertes por fecha...\n")

Dat <- read_csv(args$serie,
                col_types = cols(fecha = col_date(format = "%Y-%m-%d"),
                                 estado = col_character(),
                                 .default = col_number()))

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
  summarise(muertes = sum(muertes),
            .groups = "drop") %>%
  # ungroup() %>%

  # filter(muertes > 0) %>%
  filter(fecha >= "2020-03-18")
# Dat

# max_muertes <- Dat %>% group_by(fecha) %>% summarise(muertes = sum(muertes)) %>% select(muertes) %>% max

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
  summarize(muertes = sum(muertes),
            .groups = "drop") %>%
  select(muertes) %>%
  max()

y_top <- (max_muertes_diarias / 2) + 50
y_bottom <- (max_muertes_diarias / 2) - 50

dat %>%
  # filter(region == "Centrosur") %>%
  filter(fecha == "2021-01-10") %>%
  print(n = 20)


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
  
  geom_vline(xintercept = max(dat$fecha) - 15, col = "black") +
  geom_segment(x = parse_date("2020-03-25"), xend = parse_date("2020-03-25"), y = y_top, yend = y_bottom, size =2) +
  annotate("text",
           x = parse_date("2020-03-20") - 3,
           y = max_muertes_diarias / 2,
           size = 6,
           angle = 90,
           label = 'italic("100 fallecimientos")',
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
p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_region.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_region@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
