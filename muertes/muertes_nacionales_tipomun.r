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

args <- list(serie_municipios = "../datos/datos_abiertos/serie_tiempo_municipio_res_confirmados.csv.gz",
             municipios_lut = "../datos/util/municipios_lut_datos_abiertos.csv",
             dir_salida = "../sitio_hugo/static/imagenes/",
             zonas_metropolitanas = "../datos/util/zonas_metropolitanas_2015.csv")
cat("Muertes por tipo municipio...\n")

# Leer zm definiciones
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

# Leer serie por municipios
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

fecha_final <- max(Dat$fecha)

Dat <- Dat %>%
  select(municipio, fecha, sintomas_nuevos, muertes_nuevas, clave) %>%
  left_join(lut, by = c("clave")) %>%
  mutate(grupo = NOM_ZM) %>%
  mutate(grupo = replace(grupo, grupo != "Valle de México", "Otras zonas metropolitanas")) %>%
  mutate(grupo = replace_na(grupo, "Zonas no metropolitanas")) %>%
  select(fecha, sintomas_nuevos, muertes_nuevas, grupo) %>%
  group_by(fecha, grupo) %>%
  summarise(sintomas_nuevos = sum(sintomas_nuevos),
            muertes_nuevas = sum(muertes_nuevas),
            .groups = "drop") %>%
  # ungroup() %>%
  mutate(grupo = factor(grupo, levels = c("Otras zonas metropolitanas", "Zonas no metropolitanas",
                                          "Valle de México"))) %>%
  filter(fecha >= "2020-03-01")
  
max_y <- Dat %>%
  group_by(fecha) %>%
  summarise(sintomas_nuevos = sum(sintomas_nuevos),
            muertes_nuevas = sum(muertes_nuevas),
            .groups = "drop")

p1 <- Dat %>%
  filter(fecha >= "2020-03-25") %>%
  ggplot(aes(x = fecha, y = muertes_nuevas)) +
  geom_rect(aes(xmin = fecha_final - 15, xmax = fecha_final,
                ymin = -Inf, ymax = Inf),
            fill = "pink") +
  annotate("text",
           x = fecha_final - 6,
           y = 0.88 * max(max_y$muertes_nuevas),
           label = 'italic("Estos\nnúmeros\npueden\naumentar")',
           hjust = "middle",
           parse = TRUE) +
  geom_bar(aes(fill = grupo), width=1, stat = "identity") +
  geom_vline(xintercept = fecha_final - 15) +
  # scale_fill_brewer(palette = "PuBu", name = "") +
  scale_fill_manual(values = c("#0570b0","#74a9cf","#d0d1e6"), name = "") +
  scale_y_continuous(labels = scales::comma) + 
  ylab(label = "Número de fallecimientos") +
  xlab(label = "Fecha de defunción") +
  AMOR::theme_blackbox() +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.margin = margin(l = 20, r = 20, t = 10),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 12),
        legend.background = element_blank())

# p1
# ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_nacionales_tipo_mun.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_nacionales_tipo_mun@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

