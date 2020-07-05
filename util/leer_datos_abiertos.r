library(tidyverse)

#' Title
#'
#' @param archivo 
#' @param solo_confirmados 
#' @param solo_fallecidos 
#'
#' @return
#' @export
#'
#' @examples
leer_datos_abiertos <- function(archivo, solo_confirmados = TRUE, solo_fallecidos = FALSE){
  Dat <- read_csv(archivo,
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
           PAIS_ORIGEN = parse_character(PAIS_ORIGEN, na = c("97", "", "NA"))) 
  
  if(solo_confirmados){
    Dat <- Dat %>%
      filter(RESULTADO == "1")
  }
  
  if(solo_fallecidos){
    Dat <- Dat %>%
      filter(!is.na(FECHA_DEF))
  }
  
  return(Dat)
}