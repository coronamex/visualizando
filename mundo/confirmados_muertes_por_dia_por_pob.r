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
    xlab("Casos totales por 100 mil habitantes") +
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
                          "Luxemburgo", "Singapur", "Sudáfrica"),
                        c("US", "Spain", "Italy", 
                          "Iran", "China", "France",
                          "Brazil", "Korea, South",
                          "Japan", "Mexico", "Bahrain",
                          "Panama", "Peru", "Oman",
                          "Luxembourg", "Singapore", "South Africa"))


lut_csse <- read_csv(args$lut_csse,
                     col_types = cols(.default = col_character(),
                                      Population = col_number(),
                                      Lat = col_number(),
                                      Long_ = col_number()))
lut_csse <- lut_csse %>%
  filter(is.na(Province_State)) %>%
  select(UID, pais = Country_Region, pob = Population)
lut_csse

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
casos_mundo

incidencia_mundo <- casos_mundo %>%
  filter(casos_acumulados >= args$min_casos) %>%
  group_by(pais) %>%
  summarise(incidencia_acumulada = max(incidencia_acumulada),
            .groups = "drop") %>%
  arrange(desc(incidencia_acumulada))
incidencia_mundo

# Paises con mayor incidencia poblacional y minimo de casos
paises_elegidos <- c(setdiff(incidencia_mundo$pais, "Mexico")[1:8], "Mexico")
paises_sin_lut <- setdiff(paises_elegidos, names(lut_paises))
lut_paises <- c(lut_paises, set_names(paises_sin_lut, paises_sin_lut))

p1 <- graficar_incremento(casos_mundo = casos_mundo,
                          paises_elegidos = paises_elegidos,
                          paises_lut = paises_lut,
                          min_incidencia = args$min_incidencia,
                          dias_ventana = args$dias_ventana)
p1





