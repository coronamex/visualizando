library(tidyverse)

args <- list(mundo_dir = "../COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/",
             min_casos = 60,
             dias_ventana = 7,
             dge_dir = "../datos/ssa_dge",
             dir_salida = "../sitio_hugo/static/imagenes/",
             base_de_datos = "../datos/datos_abiertos/base_de_datos.csv")


paises <- c("US", "Spain", "Italy", "Iran", "China",
            "France", "Brazil", "South Korea", "Japan")
nuevos_nombres <- c("EEUU", "España", "Italia", "Irán",
                    "China", "Francia", "Brasil", "Corea del Sur",
                    "Japón")
estados_nombres_dge <- c(AGUASCALIENTES = "Aguascalientes",
                         `BAJA CALIFORNIA` = "Baja California",
                         `BAJA CALIFORNIA SUR` = "Baja California Sur",
                         "BAJA CALIFORNIA \nSUR" = "Baja California Sur",
                         `CAMPECHE` = "Campeche",
                         `CIUDAD DE MÉXICO` = "Ciudad de México",
                         `DISTRITO FEDERAL` = "Ciudad de México",
                         COAHUILA = "Coahuila",
                         COLIMA = "Colima",
                         CHIAPAS = "Chiapas",
                         CHIHUAHUA = "Chihuahua",
                         DURANGO = "Durango",
                         GUANAJUATO = "Guanajuato",
                         GUERRERO = "Guerrero",
                         HIDALGO = "Hidalgo",
                         JALISCO = "Jalisco",
                         `MÉXICO` = "México",
                         `MEXICO` = "México",
                         `MICHOACÁN` = "Michoacán",
                         `MICHOACAN` = "Michoacán",
                         MORELOS = "Morelos",
                         NAYARIT = "Nayarit",
                         `NUEVO LEÓN` = "Nuevo León",
                         `NUEVO LEON` = "Nuevo León",
                         OAXACA = "Oaxaca",
                         PUEBLA = "Puebla",
                         QUERETARO = "Querétaro",
                         `QUINTANA ROO` = "Quintana Roo",
                         `SAN LUIS POTOSÍ` = "San Luis Potosí",
                         `SAN LUIS POTOSI` = "San Luis Potosí",
                         SINALOA = "Sinaloa",
                         SONORA = "Sonora",
                         TABASCO = "Tabasco",
                         TAMAULIPAS = "Tamaulipas",
                         TLAXCALA = "Tlaxcala",
                         VERACRUZ = "Veracruz",
                         `YUCATÁN` = "Yucatán",
                         `YUCATAN` = "Yucatán",
                         ZACATECAS = "Zacatecas")

# Leer datos mundiales de John Hopkins
datos_mundo <- list.files(args$mundo_dir, full.names = TRUE) %>%
  str_subset("[.]md$", negate = TRUE) %>%
  map_dfr(function(file){
    # file <- list.files(args$mundo_dir, full.names = TRUE)[1]
    fecha <- basename(file) %>% str_remove("[.]csv$")
    fecha <- strptime(fecha, "%m-%d-%Y") %>% format("%Y-%m-%d") %>% as.Date()

    tab <- read_csv(file)
    names(tab)[names(tab) == "Country/Region"] <- "Country_Region"
    tab <- tab %>%
      mutate(Country_Region = replace(Country_Region, Country_Region == "Korea, South", "South Korea")) %>%
      mutate(Country_Region = replace(Country_Region, Country_Region == "Mainland China", "China"))

    tab %>%
      filter(Country_Region %in% paises) %>%
      split(.$Country_Region) %>%
      map_dfr(function(d){
        tibble(casos_acumulados = sum(d$Confirmed, na.rm = TRUE),
               muertes_acumuladas = sum(d$Deaths, na.rm = TRUE))
      }, .id = "pais") %>%
      mutate(fecha = fecha)

  })

# Calcular casos por día
datos_mundo <- datos_mundo %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d <- d %>%
      arrange(fecha)
    d <- d %>%
      bind_cols(casos_nuevos = c(NA, diff(d$casos_acumulados)),
                muertes_nuevas = c(NA, diff(d$muertes_acumuladas)))
  })

# Reemplazar nombres
for(i in 1:length(paises)){
  datos_mundo <- datos_mundo %>%
    mutate(pais = replace(pais, pais == paises[i], nuevos_nombres[i]))
}

datos_mundo

fechas_dirs <- list.dirs(args$dge_dir, full.names = TRUE, recursive = FALSE)
datos_mx <- fechas_dirs %>%
  map_dfr(function(fecha_dir, lu_tab){
    # fecha_dir <- fechas_dirs[36]
    # lu_tab <- estados_nombres_dge
    archivo_tabla <- file.path(fecha_dir, "tabla_casos_confirmados.csv")
    if(file.exists(archivo_tabla)){
      # Tab <- read_csv(archivo_tabla,
      #                 col_types = cols(estado = col_character(),
      #                                  sexo = col_character(),
      #                                  edad = col_number(),
      #                                  fecha_sintomas = col_date(format = "%d/%m/%Y"),
      #                                  procedencia = col_character(),
      #                                  fecha_llegada = col_date(format = "%d/%m/%Y")))
      Tab <- read_csv(archivo_tabla,
                      col_types = cols(estado = col_character(),
                                       sexo = col_character(),
                                       edad = col_number(),
                                       fecha_sintomas = col_date(format = "%d/%m/%Y"),
                                       procedencia = col_character()))

      Tab$estado <- as.vector(lu_tab[Tab$estado])
      fecha <- basename(fecha_dir) %>% as.Date()

      res <- Tab %>%
        # split(.$procedencia == "Contacto" & is.na(.$fecha_llegada)) %>%
        split(.$procedencia == "Contacto") %>%
        map_dfr(function(d, fecha){
          acum_estado <- table(d$estado)
          res <- tibble(estado = names(acum_estado),
                        casos_acumulados = as.numeric(acum_estado),
                        fecha = fecha)
          return(res)
        }, fecha = fecha, .id = "transmicion_comunitaria")

      return(res)
    }
    },lu_tab = estados_nombres_dge)
datos_mx
datos_mundo

datos_mx <- aggregate(casos_acumulados ~ transmicion_comunitaria + fecha, data = datos_mx, FUN = sum) %>%
  mutate(pais = transmicion_comunitaria) %>%
  mutate(pais = replace(pais, pais == "TRUE", "México (comunitario)")) %>%
  mutate(pais = replace(pais, pais == "FALSE", "México (importado)")) %>%
  select(-transmicion_comunitaria) %>%
  as_tibble()
datos_mx %>%
  arrange(fecha)

# Combinar
Dat <- datos_mundo %>%
  filter(pais != "China") %>%
  select(pais, casos_acumulados, fecha) %>%
  bind_rows(datos_mx)
Dat

# Casos acumukados
p1 <- Dat %>%
  mutate(group = replace(pais, !(pais %in% c("México (comunitario)", "México (importado)")), "Otros países")) %>%
  filter(casos_acumulados >= args$min_casos) %>%
  split(.$pais) %>%
  map_dfr(function(d){
    d$dia <- as.numeric(d$fecha - min(d$fecha) + 1)
    d
  }) %>%
  ggplot(aes(x = dia, y = casos_acumulados, group = pais)) +
  geom_line(aes(col = group, size = group)) +
  scale_color_manual(values = c("red", "pink", "grey"), name = "País",
                     guide = guide_legend(nrow = 2)) +
  scale_size_manual(values = c(2,2,1), guide = NULL) +
  scale_y_log10(limits = c(60, 1e5)) +
  xlim(c(0,30)) +
  # ylim(c(60,1e5)) +
  ylab("Total de casos confirmados") +
  xlab("Días desde el caso 60") +
  AMOR::theme_blackbox() +
  theme(legend.position = "top",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),
        legend.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 10),
        plot.margin = margin(l = 20, r = 20))
p1
# archivo <- file.path("test.png")
# ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
archivo <- file.path(args$dir_salida, "casos_acumulados_comunitarios_importados.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 75)
archivo <- file.path(args$dir_salida, "casos_acumulados_comunitarios_importados@2x.png")
ggsave(archivo, p1, width = 7, height = 6.7, dpi = 150)
