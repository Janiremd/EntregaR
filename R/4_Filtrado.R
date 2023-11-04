
# FILTRADO DE VARIABLES BASADO EN ENTROPIA --------------------------------

#' Filtrar variables de un dataset basado en la entrop?a
#'
#' @description Esta funci?n filtra las variables de un dataset basado en un umbral de entrop?a. Las variables cuyas entrop?as superen el umbral se mantienen en el dataset resultante.
#'
#' @param dataset El dataset del cual se van a filtrar las variables.
#' @param umbral_entropia El umbral de entrop?a. Las variables con una entrop?a mayor que este umbral se mantienen en el dataset resultante.
#'
#' @return Un nuevo dataset que contiene las variables que cumplen con el criterio de entrop?a especificado.
#'
#' @examples
#' Ejemplo de uso
#'dataset <- list(
#'  Variable1 = c("A", "A", "A", "C", "B"),
#'  Variable2 = c("X", "Y", "X", "Z", "Z"),
#'  Variable3 = c("A", "B", "A", "C", "B")
#')

#' Definir un umbral de entrop?a
#' umbral_entropia <- 1.4
#' Filtrar las variables basadas en la entrop?a
#' variables_filtradas <- filtrar_variables_por_entropia(dataset, umbral_entropia)
#' print(variables_filtradas)
#'
#' @export
filtrar_variables_por_entropia <- function(dataset, umbral_entropia) {
  # Crear una lista para almacenar las variables que cumplen con el criterio
  variables_filtradas <- list()

  # Calcular la entrop?a para cada variable y filtrar
  for (col_name in names(dataset)) {
    variable <- dataset[[col_name]]
    #verificar si todos los elementos de la variable son de tipo caracter.
    if (all(sapply(variable, is.character))) {
      entropia <- calcular_entropia(variable)
      if (entropia > umbral_entropia) {
        variables_filtradas[[col_name]] <- variable
      }
    }
  }
  # Crear un nuevo dataset con las variables filtradas
  nuevo_dataset <- variables_filtradas

  return(nuevo_dataset)
}
