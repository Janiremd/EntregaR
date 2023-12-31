% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/5_Correlacion_informacion_mutua.R
\name{calcular_correlacion}
\alias{calcular_correlacion}
\title{Funci?n para calcular la correlaci?n de Pearson entre dos variables}
\usage{
calcular_correlacion(variable1, variable2)
}
\arguments{
\item{variable1}{Un vector num?rico que representa la primera variable.}

\item{variable2}{Un vector num?rico que representa la segunda variable.}
}
\value{
El valor de la correlaci?n de Pearson entre las dos variables. Si los vectores  no tienen la misma longitud, la funci?n muestra un mensaje de error y se detiene.
}
\description{
Esta funci?n calcula la correlaci?n de Pearson entre dos variables,
que mide el grado de relaci?n lineal entre ellas. La correlaci?n de Pearson es un valor
en el rango de -1 a 1, donde 1 indica una correlaci?n positiva perfecta, -1 indica una
correlaci?n negativa perfecta y 0 indica ausencia de correlaci?n.
}
\examples{
Ejemplo de uso
var1 <- c(1, 2, 3, 4, 5)
var2 <- c(2, 3, 4, 5, 6)
calcular_correlacion(var1, var2)  # Calcula la correlaci?n de Pearson entre las dos variables.

Ejemplo con vectores de diferente longitud
var1 <- c(1, 2, 3)
var2 <- c(2, 3, 4, 5)
calcular_correlacion(var1, var2)  # Muestra un mensaje de error.

}
