library(tidyverse)

#' Title
#'
#' @param archivo 
#' @param solo_confirmados 
#' @param solo_fallecidos 
#' @param solo_laboratorio sí se deben considerar como confirmados sólo los casos
#' con prueba positiva PCR, o se deben considerar también las dicatminaciones y
#' las asociaciones. Sólo aplica para versión octubre2020.
#' @param version "adivinar", "abril2020", o "octubre2020".
#'
#' @return
#' @export
#'
#' @examples
leer_datos_abiertos <- function(archivo,
                                solo_confirmados = TRUE,
                                solo_fallecidos = FALSE,
                                solo_laboratorio = FALSE,
                                version = "adivinar"){
  
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
  
  if(version == "adivinar"){
    variables_variables <- c("RESULTADO", "RESULTADO_LAB", "CLASIFICACION_FINAL", "INDIGENA")
    variables_variables <- variables_variables %in% colnames(Dat)
    
    if( all(variables_variables[c(2,3,4)]) & !any(variables_variables[c(1)]) ){
      version <- "octubre2020"
    }else if( all(variables_variables[c(1)]) & !any(variables_variables[c(2,3,4)]) ){
      version <- "abril2020"
    }else{
      stop("ERROR: no puedo adivinar versión de la base de datos", call. = TRUE)
    }
  }
  
  # Seleccionar confirmados dependiendo de versión
  if(solo_confirmados){
    if(version == "octubre2020"){
      if(solo_laboratorio){
        res_vals <- c("3")
      }else{
        res_vals <- c("1", "2", "3")
      }
      Dat <- Dat %>%
        filter(CLASIFICACION_FINAL %in% res_vals)
   
    }else if(version == "abril2020"){
      Dat <- Dat %>%
        filter(RESULTADO == "1")
    }else{
      stop("ERROR: versión inválida", call. = TRUE)
    }
  }
  
  if(solo_fallecidos){
    Dat <- Dat %>%
      filter(!is.na(FECHA_DEF))
  }
  
  return(Dat)
}