% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/6_PLOT.R
\name{plot_AUC}
\alias{plot_AUC}
\title{Funci?n para trazar la Curva ROC y calcular el AUC (?rea Bajo la Curva ROC)}
\usage{
plot_AUC(predicciones, clase_verdadera)
}
\arguments{
\item{predicciones}{Un vector de predicciones num?ricas del modelo. Es decir, un vector de predicciones de clasificaci?n, generalmente probabilidades de pertenencia a la clase positiva.}

\item{clase_verdadera}{Un vector de etiquetas verdaderas de clase (0 o 1) que representa las observaciones reales. Es decir, 1 para verdadero positivo y 0 para falso positivo.}
}
\value{
Un gr?fico de la Curva ROC y el valor del ?rea Bajo la Curva ROC (AUC).
}
\description{
Esta funci?n toma las predicciones de un modelo y las etiquetas verdaderas (clase)  y traza la Curva ROC (Receiver Operating Characteristic) para evaluar el rendimiento del modelo en clasificaci?n binaria. Tambi?n calcula el ?rea Bajo la Curva ROC (AUC) como una m?trica de evaluaci?n. La Curva ROC muestra la relaci?n entre la Tasa de Verdaderos Positivos (TPR) y la Tasa de Falsos Positivos (FPR) en varios umbrales de decisi?n.
}
\examples{
Ejemplo de uso
predicciones <- c(0.8, 0.6, 0.7, 0.9, 0.3, 0.5, 0.2, 0.4, 0.6, 0.8)
clase_verdadera <- c(1, 1, 0, 1, 0, 0, 0, 1, 1, 0)
plot_AUC(predicciones, clase_verdadera) # Trazar la Curva ROC y calcular el AUC.

}
