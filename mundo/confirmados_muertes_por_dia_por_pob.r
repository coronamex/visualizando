library(tidyverse)

incremento <- function(x){
  res <- x[-1] / x[-length(x)]
  res <- res[!is.na(res)]
  res <- res[!is.infinite(res)]
  res <- (res - 1)
  mean(res)
}
roll_incremento <- tibbletime::rollify(incremento, window = args$dias_ventana + 1)


graficar_incremento <- function(casos_mundo, paises_elegidos, paises_lut,
                                min_incidencia = 50, dias_ventana = 7){
  casos_mundo <- casos_mundo %>%
    filter(pais %in% paises_elegidos) %>%
    select(pais, fecha, casos_acumulados, incidencia_acumulada) %>%
    mutate(pais = as.character(lut_paises[pais])) %>%
    mutate(pais = factor(pais, levels = as.character(lut_paises[paises_elegidos])))
  
  p1 <- casos_mundo %>%
    split(.$pais) %>%
    map_dfr(function(d, min_incidencia = 20, dias_ventana = 7){
      # Asegurarse de que los datos estén ordenaados por fecha
      d <- d %>%
        arrange(fecha)
      
      # Elegir los días necesarios
      ii <- which(d$incidencia_acumulada >= min_incidencia)
      ii <- c(max(1,min(ii) - dias_ventana):(min(ii) -1), ii)
      d <- d[ii,]
      d$dia <- -(dias_ventana - 1):(nrow(d) - dias_ventana)
      
      # Calcular incremento
      d %>%
        mutate(crecimiento = roll_incremento(incidencia_acumulada))
    }, min_incidencia = min_incidencia, dias_ventana = dias_ventana) %>%
    filter(!is.na(crecimiento)) %>%
    # filter(casos_acumulados >= 1000) %>%
    
    select(pais, incidencia_acumulada, crecimiento)  %>%
    ggplot(aes(x = incidencia_acumulada, y = crecimiento, group = pais)) +
    geom_line(aes(col = pais, size = pais)) +
    scale_color_brewer(palette = "Paired", name = "") +
    scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "") +
    # scale_y_log10(labels = scales::percent) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_log10(labels = scales::comma) +
    ylab("Incremento (%)") +
    xlab("Casos acumulados por 100 mil habitantes") +
    guides(color = guide_legend(override.aes = list(size = 3))) +
    AMOR::theme_blackbox() +
    theme(legend.position = "top",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold"),
          legend.background = element_blank(),
          legend.key = element_blank(),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 10),
          plot.margin = margin(l = 20, r = 20))
  # p1
  # ggsave("test.png", p1, width = 7, height = 6.7, dpi = 75)
  
  p1
}


suma_ventana <- tibbletime::rollify(sum, window = args$dias_ventana, na_value = 0)


args <- list(min_casos = 1e3,
             min_incidencia = 50,
             dias_ventana = 7,
             serie_tiempo_casos_mundo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
             serie_tiempo_muertes_mundo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
             lut_csse = "../COVID-19/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv",
             dir_salida = "../sitio_hugo/static/imagenes/")
cat("Comparaciones mundiales...\n")

lut_paises <- set_names(c("EEUU", "España", "Italia",
                          "Irán", "China", "Francia",
                          "Brasil", "Corea del Sur",
                          "Japón", "México", "Baréin",
                          "Panamá", "Perú", "Omán",
                          "Luxemburgo", "Singapur", "Sudáfrica",
                          "Kirguistán", "Kazajistán", "República Dominicana",
                          "Suiza", "Arabia Saudita", "Bélgica",
                          "Reino Unido", "Suecia"),
                        c("US", "Spain", "Italy", 
                          "Iran", "China", "France",
                          "Brazil", "Korea, South",
                          "Japan", "Mexico", "Bahrain",
                          "Panama", "Peru", "Oman",
                          "Luxembourg", "Singapore", "South Africa",
                          "Kyrgyzstan", "Kazakhstan", "Dominican Republic",
                          "Switzerland", "Saudi Arabia", "Belgium",
                          "United Kingdom", "Sweden"))


lut_csse <- read_csv(args$lut_csse,
                     col_types = cols(.default = col_character(),
                                      Population = col_number(),
                                      Lat = col_number(),
                                      Long_ = col_number()))
lut_csse <- lut_csse %>%
  filter(is.na(Province_State)) %>%
  select(UID, pais = Country_Region, pob = Population)

# Leer casos mundo
casos_mundo <- read_csv(args$serie_tiempo_casos_mundo,
                        col_types = cols("Province/State" = col_character(),
                                         "Country/Region" = col_character(),
                                         .default = col_number()))
stop_for_problems(casos_mundo)
casos_mundo <- casos_mundo %>%
  filter(is.na(`Province/State`))  %>%
  select(-Lat, -Long, -"Province/State") %>%
  rename(pais = `Country/Region`)  %>%
  pivot_longer(-pais, names_to = "fecha", values_to = "casos_acumulados") %>%
  mutate(fecha = parse_date(fecha, format = "%m/%d/%y")) %>%
  left_join(lut_csse, by = "pais") %>%
  mutate(incidencia_acumulada = 1e5 * casos_acumulados / pob) %>%
  select(pais, fecha, casos_acumulados, incidencia_acumulada)

incidencia_mundo <- casos_mundo %>%
  filter(casos_acumulados >= args$min_casos) %>%
  group_by(pais) %>%
  summarise(incidencia_acumulada = max(incidencia_acumulada),
            .groups = "drop") %>%
  arrange(desc(incidencia_acumulada))

# Paises con mayor incidencia poblacional y minimo de casos
paises_elegidos <- c(setdiff(incidencia_mundo$pais, "Mexico")[1:8], "Mexico")
paises_sin_lut <- setdiff(paises_elegidos, names(lut_paises))
lut_paises <- c(lut_paises, set_names(paises_sin_lut, paises_sin_lut))

p1 <- graficar_incremento(casos_mundo = casos_mundo,
                          paises_elegidos = paises_elegidos,
                          paises_lut = paises_lut,
                          min_incidencia = args$min_incidencia,
                          dias_ventana = args$dias_ventana)
archivo <- file.path(args$dir_salida, "incremento_por_casos.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "incremento_por_casos@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)

# Incidencia por semana
casos_mundo <- casos_mundo %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d %>%
      arrange(fecha) %>%
      mutate(casos_nuevos = c(casos_acumulados[1], diff(casos_acumulados)),
             incidencia_nueva = c(incidencia_acumulada[1], diff(incidencia_acumulada))) %>%
      mutate(incidencia_semana = suma_ventana(incidencia_nueva),
             casos_semana = suma_ventana(casos_nuevos)) 
  }) 

incidencia_mundo <- casos_mundo %>%
  group_by(pais) %>%
  summarise(incidencia_semana = max(incidencia_semana),
            casos_semana = max(casos_semana),
            .groups = "drop") %>%
  arrange(desc(incidencia_semana)) %>%
  filter(casos_semana >= args$min_casos*7 )

# Paises con mayor pico por poblacion
paises_elegidos <- c(setdiff(incidencia_mundo$pais, "Mexico")[1:8], "Mexico")
paises_sin_lut <- setdiff(paises_elegidos, names(lut_paises))
lut_paises <- c(lut_paises, set_names(paises_sin_lut, paises_sin_lut))

p1 <- casos_mundo %>%
  filter(pais %in% paises_elegidos)  %>%
  select(pais, fecha, incidencia_acumulada, incidencia_semana) %>%
  mutate(pais = as.character(lut_paises[pais])) %>%
  mutate(pais = factor(pais, levels = as.character(lut_paises[paises_elegidos]))) %>%
  
  split(.$pais) %>%
  map_dfr(function(d, min_casos){
    ii <- which(d$incidencia_semana > 5)[1]:nrow(d)
    d <- d[ii,]
    d <- d %>%
      mutate(dia = as.numeric(fecha - min(fecha)))
  }, min_casos = args$min_casos) %>%
  select(pais, fecha, dia, incidencia_semana, incidencia_acumulada) %>%

  ggplot(aes(x = dia, y = incidencia_semana + 1, group = pais)) +
  geom_line(aes(col = pais, size = pais)) +
  scale_color_brewer(palette = "Paired", name = "") +
  scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "") +

  scale_y_continuous(labels = scales::comma) +
  # scale_y_log10(labels = scales::comma) +

  ylab("Casos semanales por 100 mil habitantes") +
  xlab("Días desde 5 casos por 100 mil habitantes") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        # panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
archivo <- file.path(args$dir_salida, "casos_acumulados_por_dia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_acumulados_por_dia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)


# Leer muertes mundo
muertes_mundo <- read_csv(args$serie_tiempo_muertes_mundo,
                        col_types = cols("Province/State" = col_character(),
                                         "Country/Region" = col_character(),
                                         .default = col_number()))
stop_for_problems(muertes_mundo)
muertes_mundo <- muertes_mundo %>%
  filter(is.na(`Province/State`))  %>%
  select(-Lat, -Long, -"Province/State") %>%
  rename(pais = `Country/Region`)  %>%
  pivot_longer(-pais, names_to = "fecha", values_to = "muertes_acumuladas") %>%
  mutate(fecha = parse_date(fecha, format = "%m/%d/%y")) %>%
  left_join(lut_csse, by = "pais") %>%
  mutate(mortalidad_acumulada = 1e5 * muertes_acumuladas / pob) %>%
  select(pais, fecha, muertes_acumuladas, mortalidad_acumulada)

mortalidad_mundo <- muertes_mundo %>%
  group_by(pais) %>%
  summarise(mortalidad_acumulada = max(mortalidad_acumulada),
            muertes_acumuladas = max(muertes_acumuladas),
            .groups = "drop") %>%
  arrange(desc(mortalidad_acumulada)) %>%
  filter(muertes_acumuladas >= 1e3)

# Paises con mayor mortalidad poblacion
paises_elegidos <- c(setdiff(mortalidad_mundo$pais, "Mexico")[1:8], "Mexico")
paises_sin_lut <- setdiff(paises_elegidos, names(lut_paises))
lut_paises <- c(lut_paises, set_names(paises_sin_lut, paises_sin_lut))

# muertes_mundo %>%
#   filter(pais == "Spain") %>%
#   mutate(a = mortalidad_acumulada - lag(mortalidad_acumulada)) %>%
#   print(n = 200)

p1 <- muertes_mundo %>%
  filter(pais %in% paises_elegidos)  %>%
  select(pais, fecha, mortalidad_acumulada, muertes_acumuladas) %>%
  mutate(pais = as.character(lut_paises[pais])) %>%
  mutate(pais = factor(pais, levels = as.character(lut_paises[paises_elegidos]))) %>%
  
  split(.$pais) %>%
  map_dfr(function(d){
    d %>%
      filter(mortalidad_acumulada >= 1) %>%
      mutate(dia = as.numeric(fecha - min(fecha)))
  }) %>%
  
  ggplot(aes(x = dia, y = mortalidad_acumulada + 1, group = pais)) +
  geom_line(aes(col = pais, size = pais)) +
  scale_color_brewer(palette = "Paired", name = "") +
  scale_size_manual(values = c(1,1,1,1,1,1,1,1,3), name = "") +
  
  scale_y_continuous(labels = scales::comma) +
  # scale_y_log10(labels = scales::comma) +
  
  ylab("Muertes totales por 100 mil habitantes") +
  xlab("Días desde 1 muerte por 100 mil habitantes") +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        # panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
archivo <- file.path(args$dir_salida, "muertes_acumuladas_por_dia.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "muertes_acumuladas_por_dia@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
  
