library(tidyverse)

args <- list(base_de_datos = "../datos/datos_abiertos/base_de_datos.csv")

Dat <- read_csv(args$base_de_datos,
                col_types = cols(FECHA_ACTUALIZACION = col_date(format = "%Y-%m-%d"),
                                 FECHA_INGRESO = col_date(format = "%Y-%m-%d"),
                                 FECHA_SINTOMAS = col_date(format = "%Y-%m-%d"),
                                 FECHA_DEF = col_character(),
                                 EDAD = col_number(),
                                 .default = col_character())) 
stop_for_problems(Dat)
Dat <- Dat %>%
  mutate(FECHA_DEF = parse_date(x = FECHA_DEF, format = "%Y-%m-%d", na = c("9999-99-99", "", "NA")),
         PAIS_NACIONALIDAD = parse_character(PAIS_NACIONALIDAD, na = c("99", "", "NA")),
         PAIS_ORIGEN = parse_character(PAIS_ORIGEN, na = c("97", "", "NA"))) %>%
  filter(RESULTADO == "1")
Dat

# Seleccionar muertes
Dat <- Dat %>%
  filter(!is.na(FECHA_DEF))

Dat <- Dat %>%
  group_by(FECHA_SINTOMAS) %>%
  summarise(numero_sintomas = length(ID_REGISTRO)) %>%
  ungroup() %>%
  rename(fecha_sintomas = FECHA_SINTOMAS) %>%
  arrange(fecha_sintomas) %>%
  pmap_dfr(function(fecha_sintomas, numero_sintomas){
    t_incubacion <- rgamma(n = numero_sintomas, shape = 3.45, rate = 0.66) %>%
      ceiling()
    tab <- table(fecha_sintomas - t_incubacion)
    tibble(fecha = names(tab) %>% parse_date(format = "%Y-%m-%d"),
           numero_contagios = as.vector(tab))
  }) %>%
  group_by(fecha) %>%
  summarise(contagios_estimados = sum(numero_contagios)) %>%
  ungroup() %>%
  arrange(fecha)
Dat


fines_semana <- tibble(xmin = seq.Date(from = min(Dat$fecha),
                       to = max(Dat$fecha),
                       by = 1)) %>%
  mutate(xmax = xmin + 1) %>%
  filter(chron::is.weekend(xmin) & chron::is.weekend(xmax))
fines_semana

Dat %>%
  ggplot() +
  geom_rect(data = fines_semana,
            aes(xmin = xmin,
                xmax = xmax,
                ymin = -Inf, ymax = Inf), fill = "yellow") +
  geom_line(aes(x = fecha, y = contagios_estimados)) +
  geom_vline(xintercept = "2020-04-05" %>% parse_date(format = "%Y-%m-%d")) +
  geom_vline(xintercept = "2020-04-12" %>% parse_date(format = "%Y-%m-%d"))


Dat %>%
  ggplot(aes(x = chron::is.weekend(fecha), y = contagios_estimados)) +
  # geom_violin() +
  geom_boxplot() +
  geom_point(position = position_jitter(width = 0.3))
