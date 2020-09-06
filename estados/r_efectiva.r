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

args <- list(dir_estimados_ent = "../covid-model/R_efectiva/entidades/",
             dir_estimados_zms = "../covid-model/R_efectiva/zms/",
             lut_zm_ent = "../covid-model/selected_zms.csv",
             dir_salida = "../sitio_hugo/static/imagenes/",
             n_dias = 60,
             dias_retraso = 5)
cat("Graficando R efectiva...\n")

dat_ent <- list.files(args$dir_estimados_ent, full.names = TRUE) %>%
  map_dfr(~read_csv(.x,
                    col_types = cols(date = col_date(),
                                     fecha_estimado = col_date(),
                                     estado = col_character(),
                                     .default = col_number()))) %>%
  filter(date >= "2020-03-01") %>%
  filter(date < min(fecha_estimado) - args$dias_retraso) %>%
  filter(date >= max(date) - args$n_dias)

dat_zms <- list.files(args$dir_estimados_zms, full.names = TRUE) %>%
  map_dfr(~read_csv(.x,
                    col_types = cols(date = col_date(),
                                     fecha_estimado = col_date(),
                                     zona_metropolitana = col_character(),
                                     .default = col_number()))) %>%
  # rename(zona_metropolitana = zona_metropolitada) %>%
  filter(date >= "2020-03-01") %>%
  filter(date < min(fecha_estimado) - args$dias_retraso) %>%
  filter(date >= max(date) - args$n_dias)

zm_ents <- read_csv(args$lut_zm_ent,
                    col_types = cols(.default = col_character()))
zm_ents <- zm_ents %>%
  group_by(CVE_ZM) %>% 
  summarise(NOM_ZM = unique(NOM_ZM),
            estado = unique(NOM_ENT),
            .groups = "drop") %>%
  # ungroup() %>%
  rename(zona_metropolitana = CVE_ZM) %>%
  mutate(estado = replace(estado,
                          estado == "Coahuila de Zaragoza",
                          "Coahuila")) %>%
  mutate(estado = replace(estado,
                          estado == "Veracruz de Ignacio de la Llave",
                          "Veracruz")) %>%
  mutate(estado = replace(estado,
                          estado == "Michoacán de Ocampo",
                          "Michoacán"))

dat <- bind_rows(dat_zms %>%
            select(date, median, lower_80, upper_80, zona_metropolitana, fecha_estimado) %>%
            left_join(zm_ents, by = "zona_metropolitana") %>%
            select(-zona_metropolitana) %>%
            rename(nombre = NOM_ZM) %>%
            mutate(tipo = "zm"),
          dat_ent %>%
            select(date, median, lower_80, upper_80, estado, fecha_estimado) %>%
            mutate(nombre = estado,
                   tipo = "estado"))

p1 <- dat %>%
  
  ggplot(aes(x = date, y = median, group = tipo)) +
  
  facet_wrap(~ estado, ncol = 5) +
  geom_ribbon(aes(ymin = lower_80, ymax = upper_80, col = tipo, fill = tipo), alpha = 0.2) +
  geom_line(aes(col=tipo), size = 2, alpha = 0.7) +
  geom_text(data = dat %>%
              filter(tipo == "zm") %>%
              filter(date == min(date) + 30),
            aes(label = nombre, y = 1.3), col = "#5e3c99", size = 3) +
  
  scale_color_manual(values = c("#e66101", "#5e3c99"),
                     name = "",
                     labels = c("Entidad", "Mayor zona metropolitana")) +
  scale_fill_manual(values = c("#e66101", "#5e3c99"),
                     name = "",
                     labels = c("Entidad", "Mayor zona metropolitana")) +
  labs(tag = paste0("Última actualización: ", min(dat$fecha_estimado))) + 
  
  geom_hline(yintercept = 1) +
  ylab("Promedio de contagios por enfermo de COVID-19 (R_t)") +
  # scale_color_identity(guide = "legend", name = "",
  #                      labels = paste0("Última actualización: ", min(dat$fecha_estimado))) +
  # guides(color=guide_legend(override.aes = list(color = NA))) +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        panel.spacing = unit(1, "points"),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 90),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        # legend.position = "bottom",
        plot.tag.position = c(0.7, 0.03),
        legend.position = c(0.9, 0),
        legend.justification = c(1, 0),
        legend.text = element_text(size = 12),
        legend.background = element_blank(),
        legend.key = element_blank(),
        
        axis.title = element_text(face = "bold", size = 12),
        plot.margin = margin(l = 20, r = 20, b = 20),
        strip.text = element_text(face = "bold", color = "#e66101"))
# p1
# ggsave("test.png", p1, width = 7, height = 9.5, dpi = 150)
archivo <- file.path(args$dir_salida, "r_efectiva.png")
ggsave(archivo, p1, width = 7, height = 9.5, dpi = 75)
archivo <- file.path(args$dir_salida, "r_efectiva@2x.png")
ggsave(archivo, p1, width = 7, height = 9.5, dpi = 150)




