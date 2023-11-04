
# PLOT AUC ----------------------------------------------------------------

#' Funci?n para trazar la Curva ROC y calcular el AUC (?rea Bajo la Curva ROC)
#'
#' @description Esta funci?n toma las predicciones de un modelo y las etiquetas verdaderas (clase)  y traza la Curva ROC (Receiver Operating Characteristic) para evaluar el rendimiento del modelo en clasificaci?n binaria. Tambi?n calcula el ?rea Bajo la Curva ROC (AUC) como una m?trica de evaluaci?n. La Curva ROC muestra la relaci?n entre la Tasa de Verdaderos Positivos (TPR) y la Tasa de Falsos Positivos (FPR) en varios umbrales de decisi?n.
#'
#' @param predicciones Un vector de predicciones num?ricas del modelo. Es decir, un vector de predicciones de clasificaci?n, generalmente probabilidades de pertenencia a la clase positiva.
#' @param clase_verdadera Un vector de etiquetas verdaderas de clase (0 o 1) que representa las observaciones reales. Es decir, 1 para verdadero positivo y 0 para falso positivo.
#'
#' @return Un gr?fico de la Curva ROC y el valor del ?rea Bajo la Curva ROC (AUC).
#'
#' @examples
#' Ejemplo de uso
#' predicciones <- c(0.8, 0.6, 0.7, 0.9, 0.3, 0.5, 0.2, 0.4, 0.6, 0.8)
#' clase_verdadera <- c(1, 1, 0, 1, 0, 0, 0, 1, 1, 0)
#' plot_AUC(predicciones, clase_verdadera) # Trazar la Curva ROC y calcular el AUC.
#'
#' @export
plot_AUC <- function(predicciones, clase_verdadera) {

  # Calcular la TPR y FPR en varios umbrales de decisi?n
  umbrales <- sort(unique(predicciones), decreasing = TRUE)

  #Tasa de verdaderos positivos o sensibilidad (eje vertical)
  TRP <- numeric(length(umbrales))
  #Tasa de falsos positivos (eje horizontal)
  FPR <- numeric(length(umbrales))

  # Bucle para calcular TPR y FPR para cada umbral de decisi?n
  for (i in 1:length(umbrales)) {
    umbral <- umbrales[i]
    predicciones_umbral <- ifelse(predicciones >= umbral, 1, 0)
    TRP[i] <- sum(predicciones_umbral == 1 & clase_verdadera == 1) / sum(clase_verdadera == 1)
    FPR[i] <- sum(predicciones_umbral == 1 & clase_verdadera == 0) / sum(clase_verdadera == 0)
  }

  # Calcular el ?rea Bajo la Curva ROC (AUC)
  auc <- calcular_auc(predicciones, clase_verdadera)

  # Plot de la Curva ROC
  # TPR en el eje vertical y FPR en eje horizontal, rangos de (0,1)
  plot(FPR, TRP, type = "l", col = "black", lwd = 2, xlim = c(0, 1), ylim = c(0, 1),
       xlab = "False Positive Rate (FPR)", ylab = "True Positive Rate (TPR)",
       main = paste("Curva ROC (AUC =", round(auc, 2), ")"))

  # L?nea de referencia diagonal (clasificador aleatorio)
  lines(c(0, 1), c(0, 1), col = "gray")

  # Leyenda
  legend("bottomright", legend = "ROC Curve", col = "black", lwd=2)
}


# PLOT CORRELACION E INFORMACION MUTUA ------------------------------------

#' Funci?n para calcular la matriz de correlacion y trazar la matriz utilizando el metodo especificado (correlacion o informacion mutua)
#'
#' @description Esta funci?n calcula la matriz de correlacion y la representa segun el metodo seleccionado.
#'
#' @param dataset El conjunto de datos utilizado para calcular la matriz de correlaci?n.
#' @param method En m?todo utilizado para calcular la matriz de correlaci?n. Puede ser 'correlacion' o 'mutua
#'
#' @return Un gr?fico de la Curva ROC y el valor del ?rea Bajo la Curva ROC (AUC).
#'
#' @examples
#' Ejemplo de uso
#' dataset <- data.frame(
#'  Variable1 = c(1.8, 2.6, 4.5, 4.0, 1.2),
#'  Variable2 = c(0.7, 2.0, 3.5, 6.0, 0.5),
#'  Variable3 = factor(c("B", "B", "A", "C", "C")),
#'  Variable4 = factor(c("B", "Y", "A", "Z", "Z"))
#')
# Representaci?n matriz de correlaci?n
#'plot_correlation_matrix (dataset, 'correlacion')
#'plot_correlation_matrix (dataset, 'mutua')
#'
#' @export
plot_correlation_matrix <- function(dataset, method) {
  correlation_matrix <- calcular_correlacion_informacion_mutua(dataset)
  if(method == 'correlacion'){
    plot_matrix(correlation_matrix, "correlacion")

  }else if(method == 'mutua'){
    plot_matrix(correlation_matrix, "mutua")

  }
}


#' Funci?n para calcular la matriz de correlacion y trazar la matriz utilizando el metodo especificado (correlacion o informacion mutua)
#'
#' @description Representacion de la matriz
#'
#' @param correlation_matrix El conjunto de datos
#' @param title En m?todo utilizado para calcular la matriz de correlaci?n. Puede ser 'correlacion' o 'mutua
#'
#' @return Un gr?fico de la Curva ROC y el valor del ?rea Bajo la Curva ROC (AUC).
#'
#' @export
plot_matrix <- function(correlation_matrix, title) {

  color_matrix <- colorRampPalette(c("blue", "white", "red"))(100)
  col_limits <- c(-1, 1)
  image(1:ncol(correlation_matrix), 1:nrow(correlation_matrix), t(correlation_matrix), col = color_matrix, ylab = "", xlab = "", main = paste("Matriz de", title))
  axis(1, at = 1:ncol(correlation_matrix), cex.axis = 0.8)
  axis(2, at = 1:nrow(correlation_matrix), cex.axis = 0.8)

  for (i in 1:ncol(correlation_matrix)) {
    for (j in 1:nrow(correlation_matrix)) {
      if (i != j) {
        text(i, j, round(correlation_matrix[i, j], 2), cex = 1.5, col = "black")
      } else {
        text(i, j, colnames(correlation_matrix)[i], cex = 1.2, col = "black")
      }
    }
  }
}





