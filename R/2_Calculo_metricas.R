
# VARIANZA DE UNA VARIABLE CONTINUA ---------------------------------------

#' Funci?n para el c?lculo de la varianza en una variable continua (vector num?rico)
#'
#' @description Esta funci?n calcula la varianza de un vector num?rico. La varianza es una medida de la dispersi?n o variabilidad de los datos.
#'
#' @param vector_numerico Un vector num?rico del cual se calcular? la varianza.
#'
#' @return Un valor num?rico que representa la varianza del vector proporcionado.
#'
#' @examples
#' Ejemplo de uso
#' datos <- c(1, 2, 3, 4, 5)
#' var <- calcular_varianza(datos) # Calcula la varianza del vector
#' var
#'
#' #Ejemplo con un solo valor
#' valor <- 7
#' calcular_varianza(valor)  # Devuelve 0, ya que la varianza de un solo valor es 0.
#'
#' Ejemplo con un vector de longitud 1
#' vector_pequeno <- c(5)
#' calcular_varianza(vector_pequeno)  # Devuelve 0, ya que la varianza de un vector de #'un solo valor es 0.
#'
#' @seealso Puede usar esta funci?n para calcular la varianza de atributos num?ricos en un conjunto de datos.  Se la llama desde la funci?n 'calcular_metricas'.
#'
#' @export

calcular_varianza <- function(vector_numerico) {
  n <- length(vector_numerico)
  if (n <= 1) {
    return(0)  # La varianza de un solo valor es 0
  }
  media <- mean(vector_numerico)
  suma_cuadrados <- sum((vector_numerico - media)^2)
  varianza <- suma_cuadrados / (n - 1)  # Varianza muestral
  return(varianza)
}


# AREA BAJO LA CURVA (AUC) EN VARIABLES CONTINUAS -------------------------

#' Funci?n para calcular el ?rea Bajo la Curva ROC (AUC), en variables continuas (vector num?rico)
#'
#' @description Esta funci?n calcula el AUC (?rea Bajo la Curva ROC) para evaluar
#' el rendimiento de un modelo de clasificaci?n binaria. El AUC mide la capacidad
#' del modelo para distinguir entre clases positivas y negativas. Por tanto, para distinguir #'entre clase 0 y 1.
#'
#' @param predicciones Un vector num?rico que contiene las predicciones del modelo.
#' @param clase_verdadera Un vector num?rico que contiene las clases verdaderas #'correspondientes a las predicciones.  Debe ser un vector binario y representa las #'clases verdaderas  (0 para negativo, 1 para positivo).
#'
#' @return Un valor num?rico que representa el AUC calculado a partir de las predicciones y las clases verdaderas.  Un n?mero en el rango de 0 a 1, donde un valor m?s alto  indica un mejor rendimiento del modelo y 0.5 indica un modelo que no es mejor que el azar. Si la variable de clase no es binaria (no tiene exactamente dos valores ?nicos), la funci?n devuelve NA y muestra una advertencia.
#'
#' @examples
#' # Ejemplo de uso
#' predicciones <- c(0.9, 0.7, 0.6, 0.4, 0.2)
#' clase_verdadera <- c(1, 0, 1, 0, 1)
#' auc <- calcular_auc(predicciones, clase_verdadera)# Calcula el AUC del modelo.
#' auc
#'
#' # Ejemplo con advertencia
#' clase_no_binaria <- c(0, 1, 2, 1, 0)
#' auc <-  calcular_auc(predicciones, clase_no_binaria)  # Devuelve NA con advertencia.
#' auc
#'
#' @export
calcular_auc <- function(predicciones, clase_verdadera) {
  # Asegurar que es binario contando los valores ?nicos. Condici?n que verifique que la longitud es diferente de 2 asegurando que no hay exactamente dos valores ?nicos.

  if (length(unique(clase_verdadera)) != 2) {
    warning("La variable de clase debe ser binaria (0/1) para calcular el AUC.")
    return(NA)
  }

  # Predicciones en orden descendente, las predicciones m?s altas al principio
  orden <- order(predicciones, decreasing = TRUE)

  # Reordenar las predicciones y la clase verdadera seg?n los ?ndices
  predicciones_ordenadas <- predicciones[orden]
  clase_ordenada <- clase_verdadera[orden]

  # Inicializar variables para contar los pares (positivo, negativo)
  # y los pares (negativo, positivo)

  pares_positivos <- 0
  pares_negativos <- 0
  auc <- 0

  # C?lculo de n?mero total de pares (positivo, negativo)
  n_positivos <- sum(clase_ordenada == 1)
  n_negativos <- sum(clase_ordenada == 0)

  if (n_positivos == 0 || n_negativos == 0) {
    warning("No hay suficientes ejemplos positivos o negativos para calcular el AUC.")
    return(NA)
  }

  # Calcular el AUC sumando las ?reas debajo de la curva ROC
  for (i in 1:length(predicciones_ordenadas)) {
    if (clase_ordenada[i] == 1) {
      pares_positivos <- pares_positivos + 1
    } else {
      pares_negativos <- pares_negativos + 1
      auc <- auc + pares_positivos
    }
  }

  # AUC, dividiendo por el producto de pares positivos y negativos que asegura que est? en el rango 0 a 1.
  auc <- auc / (n_positivos * n_negativos)
  return(auc)
}

# ENTROPIA DE UN VECTOR DISCRETO/FACTOR -----------------------------------

#' Funci?n para calcular la entrop?a de un vector discreto/factor
#'
#' @description Esta funci?n calcula la entrop?a de un vector discreto o factor, que es una medida de la incertidumbre o desorden en las categor?as del vector. La entrop?a es una medida de la informaci?n contenida en el vector.
#'
#' @param vector_discreto Un vector discreto o factor del cual se calcular? la entrop?a. Contiene categor?as o etiquetas.
#'
#' @return Un valor num?rico que representa la entrop?a del vector discreto o factor. La entrop?a es un n?mero positivo o cero, puede estar en el rango de 0 (sin incertidumbre) a 1 (m?xima incertidumbre).
#'
#' @examples
#' categorias <- factor(c("A", "B", "A", "C", "B", "A"))
#' entropia <- calcular_entropia(categorias)
#' entropia
#'
#' @export
calcular_entropia <- function(vector_discreto) {

  # Frecuencia de cada categor?a
  frecuencias <- table(vector_discreto)

  #Probabilidad de cada categor?a
  probabilidades <- frecuencias / sum(frecuencias)

  # Calcular la entrop?a utilizando la f?rmula
  entropia <- -sum(probabilidades * log2(probabilidades))
  return(entropia)
}

# CALCULAR METRICAS EN FUNCION DEL TIPO DE ATRIBUTO  ----------------------


#' Funci?n principal para calcular m?tricas en funci?n del tipo de atributo
#'
#' @description Esta funci?n calcula m?tricas espec?ficas en funci?n del tipo de atributo en un conjunto de datos. Puede calcular varianza, entrop?a y AUC (?rea Bajo la Curva ROC) dependiendo de si los atributos son num?ricos o discretos.
#'
#' @param dataset El conjunto de datos en forma de data frame.
#'
#' @param variable_clase (Opcional) La variable de clase utilizada para calcular el AUC.
#' Un vector que contiene las clases verdaderas para calcular el AUC en caso de que los atributos sean num?ricos. Debe ser un vector binario (0/1). Por defecto, es NULL y no se calcula el AUC.  Si decides proporcionar este vector, debe ser un "factor".
#'
#' @return Un data frame que contiene m?tricas calculadas para cada atributo en el conjunto de datos, incluyendo varianza, entrop?a y AUC (si es aplicable).
#'
#' @examples
#' # Crear un conjunto de datos de ejemplo
#' datos <- data.frame(
#'   Atributo1 = c(1.2, 2.3, 3.1, 4.0, 5.5),
#'   Atributo2 = factor(c("A", "B", "A", "C", "B")),
#'   Clase = c(1, 0, 1, 0, 1)
#' )
#' resultados <- calcular_metricas(datos, datos$Clase)
#' resultados
#'
#' @export
calcular_metricas <- function(dataset, variable_clase = NULL) {
  resultados <- data.frame()  # Crear un dataframe para almacenar los resultados

  for (col_name in colnames(dataset)) {  # Recorrer cada atributo del dataset
    col_data <- dataset[[col_name]]

    if (is.numeric(col_data)) {
      # Si el atributo es num?rico, calcular la varianza
      varianza <- calcular_varianza(col_data)
      if (!is.null(variable_clase)) {
        # Si se proporciona una variable de clase, calcular el AUC
        if (is.factor(variable_clase)) {
          auc <- calcular_auc(col_data, variable_clase)
        } else {
          warning("La variable de clase debe ser de tipo factor para calcular el AUC.")
          auc <- NA
        }
      } else {
        auc <- NA
      }
      resultados <- rbind(resultados, c(Atributo = col_name, Tipo = "Continuo", Varianza = varianza, AUC = auc))

    } else if (is.factor(col_data)) {
      # Si el atributo es discreto (factor), calcular la entrop?a
      entropia <- calcular_entropia(col_data)
      resultados <- rbind(resultados, c(Atributo = col_name, Tipo = "Discreto", Entropia = entropia, AUC = NA))

    } else {
      warning(paste("El atributo", col_name, "no es ni num?rico ni discreto. Se omitir?."))
    }
  }

  return(resultados)
}
