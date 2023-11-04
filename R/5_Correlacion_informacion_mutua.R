
# CORRELACION DE PEARSON --------------------------------------------------

#' Funci?n para calcular la correlaci?n de Pearson entre dos variables
#'
#' @description Esta funci?n calcula la correlaci?n de Pearson entre dos variables,
#' que mide el grado de relaci?n lineal entre ellas. La correlaci?n de Pearson es un valor
#' en el rango de -1 a 1, donde 1 indica una correlaci?n positiva perfecta, -1 indica una
#' correlaci?n negativa perfecta y 0 indica ausencia de correlaci?n.
#'
#' @param variable1 Un vector num?rico que representa la primera variable.
#' @param variable2 Un vector num?rico que representa la segunda variable.
#'
#' @return El valor de la correlaci?n de Pearson entre las dos variables. Si los vectores  no tienen la misma longitud, la funci?n muestra un mensaje de error y se detiene.
#'
#' @examples
#' Ejemplo de uso
#' var1 <- c(1, 2, 3, 4, 5)
#' var2 <- c(2, 3, 4, 5, 6)
#' calcular_correlacion(var1, var2)  # Calcula la correlaci?n de Pearson entre las dos variables.
#'
#' Ejemplo con vectores de diferente longitud
#' var1 <- c(1, 2, 3)
#' var2 <- c(2, 3, 4, 5)
#' calcular_correlacion(var1, var2)  # Muestra un mensaje de error.
#'
#' @export
calcular_correlacion <- function(variable1, variable2) {

  #Asegurar que es posible la correlaci?n, sino, hacerlo saber
  if (length(variable1) != length(variable2)) {
    stop("Los vectores deben tener la misma longitud")
  }

  # Calcular la media de cada variable
  n <- length(variable1)
  media_v1 <- sum(variable1) / n
  media_v2 <- sum(variable2) / n

  # Formula de correlaci?n de Pearson
  correlacion_pearson <- (sum((variable1 - media_v1) * (variable2 - media_v2)) / sqrt(sum((variable1 - media_v1)^2) * sum((variable2 - media_v2)^2)))

  return(correlacion_pearson)
}


# INFORMACION MUTUA -------------------------------------------------------

#' Funci?n para calcular la informaci?n mutua entre dos variables categ?ricas
#'
#' @description Esta funci?n calcula la informaci?n mutua entre dos variables categ?ricas, que mide la dependencia entre ellas. La informaci?n mutua cuantifica cu?nta informaci?n compartida existe entre las dos variables. Es un valor no negativo, donde 0 indica independencia y valores mayores indican mayor dependencia.
#'
#' @param variable1 Un vector categ?rico que representa la primera variable.
#' @param variable2 Un vector categ?rico que representa la segunda variable.
#'
#' @return El valor de la informaci?n mutua entre las dos variables. Si los vectores no tienen la misma longitud, la funci?n muestra un mensaje de error y se detiene.
#'
#' @examples
#' Ejemplo de uso
#' var1 <- c("A", "B", "A", "C", "B")
#' var2 <- c("X", "Y", "X", "Z", "X")
#' calcular_informacion_mutua(var1, var2)  # Calcula la informaci?n mutua entre las dos variables.
#'
#' Ejemplo con vectores de diferente longitud
#' var1 <- c("A", "B", "A")
#' var2 <- c("X", "Y", "X", "Z")
#' calcular_informacion_mutua(var1, var2)  # Muestra un mensaje de error.
#'
#' @export
#'
calcular_informacion_mutua <- function(variable1, variable2) {

  #Asegurar que es posible la correlaci?n (informaci?n mutua), sino, hacerlo saber
  if (length(variable1) != length(variable2)) {
    stop("Los vectores deben tener la misma longitud")
  }

  # Crear tablas de contingencia (nij)
  nij <- table(variable1, variable2)

  # Probabilidades conjuntas y marginales
  n <- length(variable1)

  P_v1_v2 <- nij / n
  P_v1 <- rowSums(nij) / n
  P_v1 <- as.vector(P_v1)
  P_V2 <- colSums(nij) / n
  P_V2 <- as.vector(P_V2)

    # Informaci?n mutua (I)
  informacion_mutua <- 0
  for (i in 1:nrow(P_v1_v2)) {
    for (j in 1:ncol(P_v1_v2)) {
      valor <- P_v1_v2[i, j]
      # Verificar si el valor es numeric(0) y asignarle 0
      valor_convertido <- ifelse(length(valor) == 0, 0, valor)
      if (valor_convertido > 0) {
        #Formula para el c?lculo
        informacion_mutua <- informacion_mutua +
          P_v1_v2[i, j] * log2(P_v1_v2[i, j] / (P_v1[i] * P_V2[j]))
      }
    }
  }
  return(informacion_mutua)
}


# CORRELACION Y INFORMACION MUTUA DE DATASET ------------------------------

#' Funci?n para calcular correlaci?n o informaci?n mutua entre variables en un dataset
#'
#' @description Esta funci?n toma un dataset y calcula las correlaciones (en el caso de variables num?ricas) o la informaci?n mutua (en el caso de variables categ?ricas) entre todas las pares de variables del dataset. La funci?n devuelve una matriz de correlaciones donde cada celda representa la correlaci?n entre dos variables. Para variables incompatibles (una num?rica y una categ?rica), se asigna NA.
#'
#' @param dataset El dataset que contiene las variables a analizar.
#'
#' @return Una matriz de correlaciones que muestra las correlaciones (o informaci?n mutua) entre las variables del dataset. Las filas y columnas de la matriz est?n etiquetadas con los nombres de las variables originales.
#'
#' @examples
#' Ejemplo de uso
#' dataset <- data.frame(
#' Variable1 = c(1.8, 2.6, 4.5, 4.0, 1.2),
#' Variable2 = c(0.7, 2.0, 3.5, 6.0, 0.5),
#' Variable3 = factor(c("B", "B", "A", "C", "C")),
#' Variable4 = factor(c("B", "Y", "A", "Z", "Z"))
#')
#'matriz_correlacion <- calcular_correlacion_informacion_mutua(dataset)
#'matriz_correlacion
#'
#'@export
calcular_correlacion_informacion_mutua <- function(dataset) {
  #Crear la matriz de correlaciones inicializada a ceros
  n_variables <- ncol(dataset)
  matriz_correlacion <- matrix(0, nrow = n_variables, ncol = n_variables)

  #Crear bucle para el dataset. Evitar duplicados
  for (i in 1:(n_variables - 1)) {
    for (j in (i + 1):n_variables) {
      # Obtener los valores de las dos variables a comparar
      variable1 <- dataset[, i]
      variable2 <- dataset[, j]

      # ?De qu? tipo de variable se trata?
      if (is.numeric(variable1) && is.numeric(variable2)) {
        # Variables num?ricas.Correlaci?n (Relaci?n lineal)
        correlacion <- calcular_correlacion(variable1, variable2)

      } else if (is.factor(variable1) && is.factor(variable2)) {
        # Variables categ?ricas. Informaci?n mutua (sin suponer relaci?n lineal)
        correlacion <- calcular_informacion_mutua(variable1, variable2)

      } else {
        # Variables no compatibles (una num?rica y una categ?rica), asignar NA
        correlacion <- NA
      }
      # Asignar el valor de correlaci?n (informaci?n mutua en categ?ricas)
      matriz_correlacion[i, j] <- correlacion
      matriz_correlacion[j, i] <- correlacion #asegurar simetr?a
    }
  }
  # Evitar confusi?n. Nombrar filas/columnas con los nombres de las variables originales
  rownames(matriz_correlacion) <- colnames(dataset)
  colnames(matriz_correlacion) <- colnames(dataset)

  return(matriz_correlacion)
}
