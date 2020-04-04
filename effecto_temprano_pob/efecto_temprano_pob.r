library(tidyverse)

convertir_formato <- function(datos, values_to = "valor"){
  # datos <- muertes
  # datos <- casos
  datos <- datos %>%
    select(-UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Lat, -Long_, -Combined_Key, -Country_Region) %>%
    pivot_longer(!all_of(intersect(c("Province_State", "Population"), colnames(datos))),
                 names_to = "fecha", values_to = values_to)
  
  # if(!("Population" %in% colnames(datos))){
  #   if(is.null(pob)){
  #     stop("ERROR")
  #   }else{
  #     datos %>%
  #       left_join(pob, by = c("Province_State"="estado"))
  #   }
  # }
  
  datos <- datos %>%
    split(.$Province_State) %>%
    map_dfr(function(d){
      d %>%
        split(.$fecha) %>%
        map_dfr(function(d, col){
          res <- tibble(!!col := sum(d[,col]))
          if("Population" %in% colnames(d)){
            res$pob <- sum(d$Population)
          }
          res
        }, col = values_to, .id = "fecha") %>%
        mutate(fecha = as.Date(fecha, "%m/%d/%y") %>% strftime(format = "%Y-%m-%d") %>% as.Date()) %>%
        arrange(fecha)
    }, .id = "Province_State") 
  
  return(datos)
}


args <- list(pob = "effecto_temprano_pob/eeuu_pob.tsv",
             casos_tiempo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv",
             muertes_tiempo = "../COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

pob <- read_tsv("effecto_temprano_pob/eeuu_pob.tsv")
pob
casos <- read_csv(args$casos_tiempo)
casos
muertes <-read_csv(args$muertes_tiempo)
muertes


casos <- convertir_formato(datos = casos, values_to = "casos_acumulados") %>%
  left_join(pob %>% select(Province_State = estado, pob = pob_2019))
casos
muertes <- convertir_formato(datos = muertes, values_to = "muertes_acumuladas")
muertes


grafica_pob_futuro <- function(dat, var, valor_min = 10, n = 7, grupo = "Province_State"){
  # var <- "muertes_acumuladas"
  # valor_min = 10
  # n = 7
  # grupo = "Province_State"
  # dat <- muertes
  dat <- dat %>%
    split(.[,grupo]) %>%
    map_dfr(function(d, var, valor_min = 10, n = 7){
      # print(var)
      d %>%
        mutate(acum_prox = lead(.[,var] %>% unlist, n = n)) %>%
        mutate(nuevos_prox = acum_prox - .[,var] %>% unlist) %>%
        filter(.[,var] >= valor_min) %>%
        head(1)
    }, var = var, valor_min = valor_min, n = n)
  # dat
  
  p1 <- ggplot(dat, aes(x=pob, y = nuevos_prox)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    scale_x_log10() +
    AMOR::theme_blackbox()
  p1
}

p11 <- grafica_pob_futuro(dat = muertes, var = "muertes_acumuladas",
                         valor_min = 1, grupo = "Province_State", n = 7)
p11
p21 <- grafica_pob_futuro(dat = muertes, var = "muertes_acumuladas",
                          valor_min = 5, grupo = "Province_State", n = 7)
p21
p31 <- grafica_pob_futuro(dat = muertes, var = "muertes_acumuladas",
                          valor_min = 10, grupo = "Province_State", n = 7)
p31

p12 <- grafica_pob_futuro(dat = casos, var = "casos_acumulados",
                          valor_min = 1, grupo = "Province_State", n = 7)
p12
p22 <- grafica_pob_futuro(dat = casos, var = "casos_acumulados",
                          valor_min = 5, grupo = "Province_State", n = 7)
p22
p32 <- grafica_pob_futuro(dat = casos, var = "casos_acumulados",
                          valor_min = 10, grupo = "Province_State", n = 7)
p32






