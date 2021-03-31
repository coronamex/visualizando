# (C) Copyright 2021 Sur Herrera Paredes

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


args <- list(min_muertes = 14,
             dias_recientes = 14,
             dias_ventana = 7,
             tabla_mx = "../datos/datos_abiertos/serie_tiempo_nacional_fecha_confirmacion.csv.gz",
             # serie_tiempo_casos_mundo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
             serie_tiempo_muertes_mundo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
             lut_csse = "../COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")
roll_media <- tibbletime::rollify(mean,
                                  window = args$dias_ventana,
                                  na_value = 0)
cat("Comparaciones mundiales...\n")

lut_paises <- set_names(c("EEUU", "España", "Italia",
                          "Irán", "China", "Francia",
                          "Brasil", "Corea del Sur",
                          "Japón", "México", "Baréin",
                          "Panamá", "Perú", "Omán",
                          "Luxemburgo", "Singapur", "Sudáfrica",
                          "Kirguistán", "Kazajistán", "República Dominicana",
                          "Suiza", "Arabia Saudita", "Bélgica",
                          "Reino Unido", "Suecia",
                          "Catar", "Israel", "Kuwait", "Chile",
                          "Argentina", "República Checa",
                          "Bolivia", "Ecuador", "Alemania",
                          "Rusia"),
                        c("US", "Spain", "Italy", 
                          "Iran", "China", "France",
                          "Brazil", "Korea, South",
                          "Japan", "Mexico", "Bahrain",
                          "Panama", "Peru", "Oman",
                          "Luxembourg", "Singapore", "South Africa",
                          "Kyrgyzstan", "Kazakhstan", "Dominican Republic",
                          "Switzerland", "Saudi Arabia", "Belgium",
                          "United Kingdom", "Sweden",
                          "Qatar", "Israel", "Kuwait", "Chile",
                          "Argentina", "Czechia",
                          "Bolivia", "Ecuador", "Germany",
                          "Russia"))

# Leer daots México
datos_mx <- read_csv(args$tabla_mx,
                     col_types = cols(fecha = col_date(format = "%Y-%m-%d")))
stop_for_problems(datos_mx)
datos_mx$pais <- "México"

# Leer muertes mundo
muertes_mundo <- read_csv(args$serie_tiempo_muertes_mundo,
                          col_types = cols("Province/State" = col_character(),
                                           "Country/Region" = col_character(),
                                           .default = col_number()))
stop_for_problems(muertes_mundo)
muertes_mundo <- muertes_mundo %>%
  filter(is.na(`Province/State`)) %>%
  mutate(pais = `Country/Region`) %>%
  select(-Lat, -Long, -"Province/State", -"Country/Region") %>%
  pivot_longer(-pais, names_to = "fecha", values_to = "muertes_acumuladas") %>%
  mutate(fecha = parse_date(fecha, format = "%m/%d/%y")) %>%
  filter(pais != "Mexico")


# Calcular casos por día
muertes_mundo <- muertes_mundo %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d <- d %>%
      arrange(fecha) %>%
      mutate(muertes_nuevas = muertes_acumuladas - lag(muertes_acumuladas, 1, default = 0))
    
    d
  })

# Combinar datos
Dat <- muertes_mundo %>%
  bind_rows(datos_mx %>%
              select(pais, fecha, muertes_acumuladas, muertes_nuevas))

mortalidad <- Dat %>%
  group_by(pais) %>%
  summarise(muertes = sum(muertes_nuevas),
            .groups = "drop") %>%
  arrange(desc(muertes))

# Paises con mayor cantidad de muertes por covi
paises_elegidos <- c(setdiff(mortalidad$pais, "México")[1:8], "México")
paises_sin_lut <- setdiff(paises_elegidos, names(lut_paises))
lut_paises <- c(lut_paises,
                set_names(paises_sin_lut, paises_sin_lut),
                set_names("Resto del Mundo", "Resto del Mundo"))

Dat <- Dat %>%
  mutate(pais = replace(pais,
                        !(pais %in% paises_elegidos),
                        "Resto del Mundo")) %>%
  mutate(pais = factor(lut_paises[pais], levels = c(lut_paises[c(paises_elegidos, "Resto del Mundo")]))) %>%
  # mutate(pais = factor(pais, levels = c(paises_elegidos, "Resto del Mundo"))) %>%
  group_by(pais, fecha) %>%
  summarise(muertes = sum(muertes_nuevas),
            .groups = "drop")
# Dat

p1 <- Dat %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d %>%
      arrange(fecha) %>%
      mutate(muertes = floor(roll_media(muertes))) %>%
      filter(muertes != 0)
  }) %>%
  split(.$fecha) %>%
  map_dfr(function(d){
    tot_def <- sum(d$muertes)
    
    d$ymax <- cumsum(d$muertes) - (tot_def / 2)
    d$xmin <- d$fecha - 1
    
    d %>%
      mutate(ymin = lag(ymax, n = 1, default = - (tot_def / 2))) %>%
      filter(muertes > 0)
  }) %>%
  ggplot(aes(fill = pais)) +
  # geom_rect(aes(xmin = max(fecha) - 15, xmax = max(fecha),
  #               ymin = -Inf, ymax = Inf),
  #           fill = "pink") +
  geom_rect(aes(xmin = xmin, xmax = fecha,
                ymin = ymin, ymax = ymax), col = NA) +
  scale_fill_brewer(palette = "Paired", type = "qual", name = "") +
  guides(fill = guide_legend(nrow = 2)) +
  
  # geom_vline(xintercept = max(Dat$fecha) - 15, col = "black") +
  scale_y_continuous(breaks = function(lims){
    seq(from = lims[1], to = lims[2], by = 1500)
  },
  labels = function(labs){
    labs <- as.numeric(labs)
    labs + abs(min(labs))
  }) +
  ylab(label = "Fallecimientos") +
  xlab(label = "Fecha de registro") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  AMOR::theme_blackbox() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        axis.title.y = element_text(size = 20),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90),
        plot.margin = margin(l = 20, r = 20, b = 10))
# p1
archivo <- file.path(args$dir_salida, "muertes_mundiales.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_mundiales@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)




# Combinar datos
Dat <- muertes_mundo %>%
  bind_rows(datos_mx %>%
              select(pais, fecha, muertes_acumuladas, muertes_nuevas))

lut_csse <- read_csv(args$lut_csse,
                     col_types = cols(.default = col_character(),
                                      Population = col_number(),
                                      Lat = col_number(),
                                      Long_ = col_number()))
lut_csse <- lut_csse %>%
  filter(is.na(Province_State)) %>%
  select(UID, pais = Country_Region, pob = Population) %>%
  mutate(pais = replace(pais, pais == "Mexico", "México"))
# lut_csse

mortalidad_reciente <- Dat %>%
  filter(fecha > max(fecha) - args$dias_recientes) %>%
  group_by(pais) %>%
  summarise(muertes = sum(muertes_nuevas)) %>%
  left_join(lut_csse %>%
              select(pais, pob),
            by = "pais") %>%
  mutate(mortalidad = 1e5 * muertes / pob) %>%
  arrange(desc(mortalidad)) %>%
  filter(muertes >= args$min_muertes)

Dat <- Dat %>%
  filter(pais %in% mortalidad_reciente$pais[1:10]) %>%
  filter(fecha > max(fecha) - 60 - args$dias_ventana ) %>%
  rename(muertes = muertes_nuevas)


p1 <- Dat %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d %>%
      arrange(fecha) %>%
      mutate(muertes = floor(roll_media(muertes))) %>%
      filter(muertes != 0)
  }) %>%
  filter(fecha > max(fecha) - 60) %>%
  left_join(lut_csse %>%
              select(pais, pob),
            by = "pais") %>%
  mutate(mortalidad = 1e5 * muertes / pob) %>%
  ggplot(aes(x = fecha, y = mortalidad, group = pais)) +
  geom_line(aes(col = pais), size = 2) +
  scale_color_brewer(palette = "Set3", name = "") +
  guides(color = guide_legend(nrow = 3)) +
  ylab(expression(frac("Muertes", "100 mil habitantes"))) +
  xlab("Fecha de registro") +
  AMOR::theme_blackbox() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA, colour = "black", size = 3),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 20),
        axis.text.x = element_text(size = 10, angle = 90),
        plot.margin = margin(l = 20, r = 20, b = 10))
# p1
archivo <- file.path(args$dir_salida, "paises_mortalidad_reciente.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "paises_mortalidad_reciente@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
