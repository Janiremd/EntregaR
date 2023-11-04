
# NORMALIZACION -----------------------------------------------------------

#' Funci?n para normalizar un vector num?rico en una escala de 0 a 1
#'
#' @description Esta funci?n normaliza un vector num?rico en una escala de 0 a 1. La normalizaci?n se realiza dividiendo cada valor por la diferencia entre el valor m?ximo y m?nimo del vector.
#'
#' @param vector_numerico Un vector num?rico que se desea normalizar.
#'
#' @return Un vector num?rico normalizado en una escala de 0 a 1.
#'
#' @examples
#' Ejemplo de uso
#' datos <- c(10, 20, 30, 40, 50)
#' datos_normalizados <- normalizar_vector(datos)  # Normaliza el vector en una escala de 0 a 1.
#' datos_normalizados
#'
#' Ejemplo con falta de variaci?n
#' datos_faltavariacion <- c(10, 10, 10, 10, 10)
#' normalizar_vector(datos_faltavariacion)  # Muestra un mensaje de advertencia y devuelve el vector original.
#'
#' @export
normalizar_vector <- function(vector_numerico) {

  #Calculo de valores m?ximos y m?nimos
  maximo <- max(vector_numerico)
  minimo <- min(vector_numerico)

  #Asegurar que es posible la normalizaci?n, sino, hacerlo saber
  #Evitar divisiones por 0
  if (maximo == minimo) {
    cat("La columna no se normaliz? debido a la falta de variaci?n.\n")
    return(vector_numerico)
  }
  #Normalizar un vector num?rico en una escala de 0 a 1.
  Normalizaci?n <- (vector_numerico - minimo) / (maximo - minimo)
  return(Normalizaci?n)
}


# ESTANDARIZACION ---------------------------------------------------------

#' Funci?n para estandarizar un vector num?rico (media 0, desviaci?n est?ndar 1)
#'
#' @description Esta funci?n estandariza un vector num?rico para que tenga una media de 0 y una desviaci?n est?ndar de 1. La estandarizaci?n se realiza restando la media y dividiendo por la desviaci?n est?ndar del vector.
#'
#' @param vector_numerico Un vector num?rico que se desea estandarizar.
#'
#' @return Un vector num?rico estandarizado con media 0 y desviaci?n est?ndar 1. Si no es posible estandarizar el vector (por falta de variaci?n), se muestra un mensaje
#' de advertencia y se devuelve el vector original sin cambios.
#'
#' @examples
#' Ejemplo de uso
#' datos <- c(10, 20, 30, 40, 50)
#' datos_estandarizados <- estandarizar_vector(datos) # Estandariza el vector con media 0 y desviaci?n est?ndar 1.
#' datos_estandarizados
#'
#' Ejemplo con falta de variaci?n
#' datos_faltavariacion <- c(10, 10, 10, 10, 10)
#' estandarizar_vector(datos_faltavariacion)  # Muestra un mensaje de advertencia y devuelve el vector original.
#'
#' @export
estandarizar_vector <- function(vector_numerico) {
  #C?lculo de media y desviaci?n est?ndar
  media <- mean(vector_numerico)
  sd <- sd(vector_numerico)

  #Asegurar que es posible la estandarizaci?n, sino, hacerlo saber
  #Evitar divisiones por 0
  if (sd == 0) {
    cat("La columna no se estandariz? debido a la falta de variaci?n.\n")
    return(vector)
  }
  #Estandariza un vector num?rico para que tenga media 0 y desviaci?n est?ndar 1.
  Estandarizaci?n<- (vector_numerico - mean(vector_numerico)) / sd(vector_numerico)
  return(Estandarizaci?n)
}

# NORMALIZACION Y ESTANDARIZACION DE DATASET ------------------------------

#' Funci?n para normalizar y estandarizar un dataset num?rico
#'
#' @description Esta funci?n toma un conjunto de datos y aplica normalizaci?n y estandarizaci?n a todas las columnas num?ricas del conjunto de datos. La normalizaci?n escala los valores de cada columna a un rango de 0 a 1, mientras que la estandarizaci?n ajusta la media a 0 y la desviaci?n est?ndar a 1 en cada columna num?rica.
#'
#' @param dataset El dataset que se desea normalizar y estandarizar
#'
#' @return Una lista que contiene dos versiones del dataset: uno normalizado y otro estandarizado. Cada versi?n tiene las mismas columnas que el dataset original, pero con los valores normalizados  o estandarizados, respectivamente.
#'
#' @examples
#' # Crear un conjunto de datos de ejemplo
#' datos <- data.frame(
#'   Atributo1 = c(10, 20, 30, 40, 50),
#'   Atributo2 = c(1, 2, 3, 4, 5)
#' )
#'
#' # Aplicar normalizaci?n y estandarizaci?n al conjunto de datos de ejemplo
#' resultados <- normalizar_estandarizar_dataset(datos)
#' normalizado <- resultados$normalizado # Dataset normalizado
#' estandarizado <- resultados$estandarizado # Dataset estandarizado
#' normalizado
#' estandarizado
#'
#'
#' @export
normalizar_estandarizar_dataset <- function(dataset) {
  columnas_numericas <- sapply(dataset, is.numeric) # Obtener las columnas num?ricas
  dataset_normalizado <- dataset_estandarizado <- dataset

  #Aplicar normalizaci?n y estandarizaci?n a todas las columnas num?ricas de un dataset
  for (col_name in colnames(dataset)[columnas_numericas]) {
    dataset_estandarizado[[col_name]] <- estandarizar_vector(dataset_estandarizado[[col_name]]) # Estandariza un vector num?rico para que tenga media 0 y desviaci?n est?ndar 1
    dataset_normalizado[[col_name]] <- normalizar_vector(dataset_normalizado[[col_name]]) # Normaliza un vector num?rico en una escala de 0 a 1
  }
  return(list(normalizado = dataset_normalizado, estandarizado = dataset_estandarizado))
}
