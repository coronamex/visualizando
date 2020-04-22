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
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(tidyverse)
library(deSolve)
source("casos/sir_funciones.r")
# https://rstudio-pubs-static.s3.amazonaws.com/6852_c59c5a2e8ea3456abbeb017185de603e.html
# https://gabgoh.github.io/COVID/index.html
# https://en.wikipedia.org/wiki/Compartmental_models_in_epidemiology

args <- list(reportes_diarios = "../datos/datos_abiertos/serie_tiempo_nacional_fecha_confirmacion.csv",
             # reportes_diarios = "../datos/ssa_dge/reportes_diarios.csv",
             dias_retraso = 15,
             dir_salida = "../sitio_hugo/static/imagenes/",
             periodo_ajuste = 100,
             fecha1 = "2020-03-01",
             fecha2 = "2020-03-15",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv")
args$fecha1 <- args$fecha1 %>% as.Date(format = "%Y-%m-%d")
args$fecha2 <- args$fecha2 %>% as.Date(format = "%Y-%m-%d")

# Lee base de datos
Tab <- read_csv(args$base_de_datos,
                col_types = cols(FECHA_ACTUALIZACION = col_date(format = "%Y-%m-%d"),
                                 FECHA_INGRESO = col_date(format = "%Y-%m-%d"),
                                 FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"),
                                 FECHA_DEF = col_character(),
                                 EDAD = col_number(),
                                 .default = col_character())) 
stop_for_problems(Tab)
Tab <- Tab %>%
  mutate(FECHA_DEF = parse_date(x = FECHA_DEF, format = "%Y-%m-%d", na = c("9999-99-99", "", "NA")),
         PAIS_NACIONALIDAD = parse_character(PAIS_NACIONALIDAD, na = c("99", "", "NA")),
         PAIS_ORIGEN = parse_character(PAIS_ORIGEN, na = c("97", "", "NA")))
Tab <- Tab %>%
  filter(RESULTADO == "1") %>%
  select(fecha_sintomas = FECHA_SINTOMAS)
Tab <- table(Tab$fecha_sintomas)
Tab <- tibble(fecha = names(Tab) %>% as.Date("%Y-%m-%d"),
              casos_nuevos = as.vector(Tab)) %>%
  mutate(casos_acumulados = cumsum(casos_nuevos)) %>%
  mutate(dia = as.numeric(fecha - min(fecha)))

# Precalcular dias
fecha_inicio <- min(Tab$fecha)
fecha_final <- Sys.Date()
n_dias <- as.numeric(fecha_final - fecha_inicio)
n_dias_ajuste <- n_dias - args$dias_retraso + 1
fechas <- c("2020-03-02", "2020-03-09", "2020-03-16") %>% parse_date(format = "%Y-%m-%d")
fechas <- c("2020-03-09", "2020-03-16", "2020-03-23") %>% parse_date(format = "%Y-%m-%d")
fechas <- c("2020-03-02", "2020-03-16", "2020-03-23", "2020-03-30") %>% parse_date(format = "%Y-%m-%d")
fechas_dias <- as.numeric(fechas - fecha_inicio)
fechas_dias

# Parameters to make optimization
# pob <- 135552447
T_inc <- c(3, 4, 5, 6)
T_inf <- c(1, 2, 3)
T_inc <- c(4, 5)
T_inf <- c(2)
pob <- 127792286
# T_inc <- c(5,5.2,5.4)
# T_inf <- c(2.5,3,3.5)
# T_inc <- 5
# T_inf <- 2

R_hat <- encontrar_R_0(real = Tab, n_dias_ajuste = n_dias_ajuste,
                       dias_int = fechas_dias,
                       T_inc = T_inc, T_inf = T_inf, pob = pob)
R_hat 

# Simular con parámetros estimados
sims <- simular_multiples_modelos(modelos = R_hat,
                                  FUN = sir, real = Tab, pob = pob,
                                  n_dias = n_dias)
ctds <- read_csv(args$reportes_diarios,
                 col_types = cols(fecha = col_date(format = "%Y-%m-%d"))) %>%
  select(fecha, casos_acumulados) %>%
  mutate(dia = as.numeric(fecha - fecha_inicio),
         modelo = "Confirmado")
# ctds
sims <- sims %>%
  split(.$modelo) %>%
  map_dfr(function(d){
    # d <- sims %>% filter(modelo == "m1")
    
    max_diff <- Tab %>%
      select(dia, casos_acumulados) %>%
      mutate(modelo = "real") %>%
      filter(dia < n_dias_ajuste) %>%
      left_join(d, by = "dia") %>%
      mutate(diff = abs(casos_acumulados.x - casos_acumulados.y)) %>%
      # arrange(desc(diff))
      select(diff) %>%
      max
    
    d
    # if(max_diff < 500)
    #   return(d)
  })
sims %>%
  filter(dia >= 50) %>%
  print(n = 300)

p1 <- Tab %>%
  select(fecha, dia, casos_acumulados) %>%
  mutate(modelo = "real") %>%
  bind_rows(sims %>% mutate(fecha = fecha_inicio + dia)) %>%
  bind_rows(ctds) %>%
  mutate(grupo = "Estimado (SEIR)") %>%
  mutate(grupo = replace(grupo, modelo == "real", "Inicio de síntomas")) %>%
  mutate(grupo = replace(grupo, modelo == "Confirmado", "Confirmado")) %>%
  mutate(grupo = factor(grupo, levels = c("Confirmado", "Inicio de síntomas", "Estimado (SEIR)"))) %>%
  mutate(modelo = factor(modelo, levels = c("Confirmado", "real", unique(sims$modelo)))) %>%
  
  filter(fecha >= "2020-02-15") %>%
  # filter(!(modelo %in% para_quitar)) %>%

  ggplot(aes(x = fecha, y = casos_acumulados, group = modelo)) +
  geom_line(aes(col = grupo, size = grupo)) +
  geom_vline(xintercept = Sys.Date() - args$dias_retraso) +
  annotate("text", label = "Fin ajuste de curva",
           x = Sys.Date() - args$dias_retraso - 1.5,
           y = 10000, angle = 90,
           size = 6) +
  # geom_vline(xintercept = args$fecha1, col = "red") +
  # annotate("text", label = "Transmisión comunitaria",
  #          x = args$fecha1 - 1.5,
  #          y = 10000, angle = 90,
  #          size = 6) +
  geom_vline(xintercept = fechas, col = "red") +
  scale_color_manual(values = c("#1b9e77", "#7570b3", "#d95f02"),
                     name = "") +
  # geom_vline(xintercept = args$fecha2, col = "red") +
  # annotate("text", label = "Medidas de mitigación",
  #          x = args$fecha2 - 1.5,
  #          y = 10000, angle = 90,
  #          size = 6) +
  scale_size_manual(values = c(2, 2, 0.2)) +
  guides(size = FALSE) +
  ylab("Casos acumulados") +
  xlab("Fecha") +
  scale_y_continuous(labels = scales::comma) +
  # scale_y_log10() +
  AMOR::theme_blackbox() +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", size = 3),
        legend.position = "top",
        legend.text = element_text(size = 14),
        legend.key = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10, color = "black"),
        plot.margin = margin(l = 20, r = 20))
p1
ggsave("test.png", p1, width = 7, height = 6.7, dpi = 150)
# archivo <- file.path(args$dir_salida, "sir_nacional.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
# archivo <- file.path(args$dir_salida, "sir_nacional@2x.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

summary(R_hat$R_hat)
summary(R_hat$f1_hat)
summary(R_hat$f2_hat)
