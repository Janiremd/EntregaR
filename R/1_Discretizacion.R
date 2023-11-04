
# DISCRETIZACI?N POR IGUAL ANCHURA ----------------------------------------

#' Discretizaci?n por igual anchura
#'
#' @description Esta funci?n discretiza un atributo en un conjunto de datos en intervalos de igual anchura.
#'
#' @param datos Un dataframe que contiene los datos a procesar.
#' @param atributo_a_discretizar El nombre del atributo que se va a discretizar.
#' @param n_intervalos El n?mero de intervalos en los que se va a discretizar el atributo.
#'
#' @return Un vector de enteros que representa la discretizaci?n del atributo.
#'
#' @examples
#' Ejemplo de uso
#'datos <- data.frame(
#''Edad' = c(30, 40, 35, 28, 45, 37, 32, 41, 29, 33),
#''Ingresos' = c(55000, 48000, 60000, 52000, 58000, 49000, 57000, 51000, 54000, 63000),
#'Puntuacion' = c(75, 68, 72, 80, 67, 73, 78, 69, 74, 76)
#')
#'n_intervalos <- 5
#' discretizacion <- Discretizacion_igual_anchura(datos, "Edad", n_intervalos)
#'
#' @seealso Puede usar esta funci?n para calcular la discretizaci?n por igual anchura en un conjunto de datos.  Se la llama desde la funci?n 'Discretizacion_dataset'.
#' @export
Discretizacion_igual_anchura <- function(datos, atributo_a_discretizar, n_intervalos) {
  # Extraer de los datos el atributo a discretizar
  atributo <- datos[[atributo_a_discretizar]]
  # Eliminar valores NA
  atributo <- atributo[!is.na(atributo)]

  if (length(atributo) == 0) {
    # No hay valores validos para discretizar
    return(integer(0))
  }

  # Calcular los limites de los intervalos
  intervalo_inferior <- min(atributo)
  intervalo_superior <- max(atributo)
  anchura <- (intervalo_superior - intervalo_inferior) / n_intervalos

  # Aplicar la discretizacion
  discretizacion <- vector()
  for (valor in atributo) {
    intervalo <- as.integer((valor - intervalo_inferior) / anchura)
    discretizacion <- c(discretizacion, intervalo) #Agregar los indices de intervalo en el vector discretizacion
  }
  return(discretizacion)
}

# DISCRETIZACI?N POR IGUAL FRECUENCIA -------------------------------------

#' Discretizaci?n por igual frecuencia
#'
#' @description Esta funci?n discretiza un atributo en un conjunto de datos en intervalos de igual frecuencia.
#'
#' @param datos Un dataframe que contiene los datos a procesar.
#' @param atributo_a_discretizar El nombre del atributo que se va a discretizar.
#' @param n_intervalos El n?mero de intervalos en los que se va a discretizar el atributo.
#'
#' @return Un vector de enteros que representa la discretizaci?n del atributo.
#'
#' @examples
#' Ejemplo de uso
#'datos <- data.frame(
#''Edad' = c(30, 40, 35, 28, 45, 37, 32, 41, 29, 33),
#''Ingresos' = c(55000, 48000, 60000, 52000, 58000, 49000, 57000, 51000, 54000, 63000),
#''Puntuacion' = c(75, 68, 72, 80, 67, 73, 78, 69, 74, 76)
#')
#'n_intervalos <- 5
#' discretizacion <- Discretizacion_igual_frecuencia(datos, "Edad", n_intervalos)
#'
#' @export
Discretizacion_igual_frecuencia <- function(datos, atributo_a_discretizar, n_intervalos) {

  # Extraer el atributo de interes y eliminar valores NaN
  atributo <- datos[[atributo_a_discretizar]]
  atributo <- atributo[!is.na(atributo)]

  # Calcular los cuantiles de manera lineal (type=1)
  cuantiles <- quantile(atributo, probs = seq(0, 1, length.out = n_intervalos + 1), type = 1, na.rm = TRUE)
  # Los valores Na se excluyen utilizando na.rm

  # Aplicar la discretizacion
  discretizacion <- cut(atributo, breaks = cuantiles, labels = FALSE, include.lowest = TRUE) #include.lowest asegura que el valor mas bajo del atributo este incluido en el primer intervalo

  return(discretizacion)
}


# DISCRETIZACION DE UN DATASET COMPLETO  ----------------------------------


#' Discretizaci?n de un dataset completo
#'
#' @description Esta funci?n discretiza todas las variables num?ricas en un dataset en intervalos de igual anchura o igual frecuencia, seg?n el m?todo especificado, y opcionalmente muestra gr?ficos de los resultados.
#'
#' @param dataset El dataset a discretizar.
#' @param n_intervalos El n?mero de intervalos en los que se va a discretizar cada variable.
#' @param metodo El m?todo de discretizaci?n a utilizar. Puede ser "anchura" o "frecuencia".
#' @param plot Un valor l?gico que indica si se deben mostrar gr?ficos de los resultados (TRUE o FALSE).
#'
#' @return Una lista que contiene las variables del dataset discretizadas de acuerdo al m?todo especificado.
#'
#' @examples
#'
#' Ejemplo de uso
#'datos <- data.frame(
#'  'Edad' = c(30, 40, 35, 28, 45, 37, 32, 41, 29, 33),
#'  'Ingresos' = c(55000, 48000, 60000, 52000, 58000, 49000, 57000, 51000, 54000, 63000),
#'  'Puntuacion' = c(75, 68, 72, 80, 67, 73, 78, 69, 74, 76)
#')
#'n_intervalos <- 5
#'
#'dataset_discretizado_anchura <- Discretizacion_dataset(datos, n_intervalos, metodo = "anchura", plot= TRUE)
#'dataset_discretizado_anchura
#'dataset_discretizado_frecuencia <- Discretizacion_dataset(datos, n_intervalos, metodo = "frecuencia", plot= TRUE)
#'dataset_discretizado_frecuencia
#'
#' Ejemplo de uso con valores NA
#'datos <- data.frame(
#'  'Edad' = c(30, 40, NA, 28, 45, 37, 32, 41, 29, 33),
#'  'Ingresos' = c(55000, NA, 60000, NA, 58000, 49000, 57000, 51000, 54000, 63000),
#'  'Puntuacion' = c(75, 68, 72, 80, 67, 73, 78, 69, 74, 76)
#')
#'n_intervalos <- 5

#'dataset_discretizado_anchura <- Discretizacion_dataset(datos, n_intervalos, metodo = "anchura", plot= TRUE)
#'dataset_discretizado_anchura
#'dataset_discretizado_frecuencia <- Discretizacion_dataset(datos, n_intervalos, metodo = "frecuencia", plot= TRUE)
#'dataset_discretizado_frecuencia

#' @export

# Funci?n para discretizar un dataset completo
Discretizacion_dataset <- function(dataset, n_intervalos, metodo, plot) {

  dataset_discretizado <- list()

  for (col_name in names(dataset)) {
    variable <-dataset[[col_name]]

    if (all(sapply(variable, is.numeric))) {
      if (length(variable) > 0){
        if (metodo == "anchura") {
          columna_discretizada <- Discretizacion_igual_anchura(dataset, col_name, n_intervalos)
        } else if (metodo == "frecuencia") {
          columna_discretizada <- Discretizacion_igual_frecuencia(dataset, col_name, n_intervalos)
        } else {
          stop("M?todo de discretizaci?n no v?lido. Debe ser 'anchura' o 'frecuencia.")
        }
        dataset_discretizado[[col_name]] <- columna_discretizada
      }
    }
  }

  # Representaci?n segun metodo
  if (plot == TRUE) {

    # Mostar las representaciones en una ?nica ventana gr?fica
    num_plots <- length(names(dataset_discretizado))
    rows <- ceiling(sqrt(num_plots))
    cols <- ceiling(num_plots / rows)
    par(mfrow = c(rows, cols))

    #Representaciones. Boxplot y histograma
    for (col_name in names(dataset_discretizado)) {
      if (metodo == "anchura") {
        boxplot(dataset_discretizado[[col_name]], main = paste("Discretizaci?n Igual Anchura - ", col_name),
                xlab = "Atributo", ylab = "Valor", names = col_name)
      } else if (metodo == "frecuencia") {
        hist(dataset_discretizado[[col_name]], breaks = n_intervalos, col = 'blue', main = paste("Discretizaci?n Igual Frecuencia - ", col_name),
             xlab = "Intervalos", ylab = "Frecuencia")
      }
    }
    par(mfrow = c(1, 1))  # Restaurar a una ?nica ventana gr?fica
  }

  return(dataset_discretizado)
}


# DISCRETIZACI?N K-MEANS --------------------------------------------------

#' Discretizaci?n basada en Cl?steres, m?todo K-Means
#' @description Esta funci?n discretiza un atributo num?rico utilizando el algoritmo K-Means con una implementaci?n simple y opcionalmente muestra gr?ficos de los resultados.
#'
#' @param atributo Un vector num?rico que se va a discretizar.
#' @param n_clusters El n?mero de cl?steres en los que se va a discretizar el atributo.
#' @param plot Un valor l?gico que indica si se deben mostrar gr?ficos de los resultados (TRUE o FALSE).
#'
#' @return Un vector de etiquetas que indica a qu? cl?ster pertenece cada valor del atributo.Y el plot si asi se ha indicado.
#'
#' @examples
#'
#' Ejemplo de uso
#'datos <- data.frame(
#'  Valores = c(10, 15, 600, 20, 35, 80, 85, 55, 60, 300, 400, 500, 80, 90, 90)
#')
#'n_clusters <- 5
#'
#'Discretizado_Clusteres_Simple <- Discretizacion_Basada_En_Clusteres_Simple(datos$Valores, n_clusters, plot = TRUE)
#'print(Discretizado_Clusteres_Simple)
#'
#' @export

# Funci?n para la discretizaci?n basada en k-means (implementaci?n simple)
Discretizacion_Basada_En_Clusteres_Simple <- function(atributo, n_clusters, plot=TRUE) {

  # Verificar que el n?mero de cl?steres sea menor que la poblaci?n de datos
  if (n_clusters >= length(atributo)) {
    stop("El n?mero de cl?steres debe ser menor que el tama?o de la poblaci?n de datos.")
  }

  # Inicializaci?n manual de centroides
  centroides <- seq(min(atributo), max(atributo), length.out = n_clusters)

  # Inicializaci?n de etiquetas de cl?ster
  etiquetas <- rep(0, length(atributo))

  # N?mero m?ximo de iteraciones
  max_iter <- 100

  for (iter in 1:max_iter) {
    # Asignar cada valor al cl?ster m?s cercano
    for (i in 1:length(atributo)) {
      distancias <- abs(atributo[i] - centroides)
      etiquetas[i] <- which.min(distancias)
    }

    # Actualizar los centroides a la media de los valores de cada cl?ster
    for (c in 1:n_clusters) {
      centroides[c] <- mean(atributo[etiquetas == c])
    }
  }

  if (plot == TRUE) {

    for (i in 1:length(atributo)) {
      valor <- atributo[i]
      cluster <- etiquetas[i]
      cat("Valor:", valor, "Cl?ster:", cluster, "\n")
    }

    # Crear un diagrama de dispersi?n para representar los cl?steres
    plot(atributo, col = etiquetas, pch = 19, main = "Discretizaci?n basada en Cl?steres", xlab = "Valores", ylab = "Cl?steres")

    # Agregar una leyenda
    legend("topright", legend = unique(etiquetas), col = unique(etiquetas), pch = 19, title = "Cl?steres")

    # Etiquetas de ejes
    title(main = "Discretizaci?n basada en Cl?steres", xlab = "Valores", ylab = "Cl?steres")
  }
  return(etiquetas)
}

