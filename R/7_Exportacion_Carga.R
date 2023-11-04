
# EXPORTACION  ------------------------------------------------------------

#' Exportar un DataFrame a un archivo CSV y mostrar la ruta del archivo creado
#'
#' @description Esta funci?n toma un DataFrame, lo convierte en un archivo CSV y muestra la ruta absoluta del archivo creado. Es ?til para guardar y compartir conjuntos de datos en formato CSV.
#'
#' @param df El DataFrame que se exportar? a un archivo CSV.
#' @param nombre_archivo_csv El nombre del archivo CSV en el que se guardar?n los datos.
#'
#' @return El contenido del archivo CSV creado.
#'
#' @examples
#' Ejemplo de uso
#' datos<- data.frame(
#'  Edad = c(25, 30, 35, 40, 45),
#'  Altura = c(160, 175, 180, 170, 185),
#'  Peso = c(60, 70, 75, 80, 90)
#' )
#' Nombra el archivo CSV en el que deseas guardar los datos y llama a la funci?n para exportar
#' exportar_a_CSV(datos, nombre_archivo_csv="resultados.csv")
#'
#' @export
exportar_a_CSV <- function(df, nombre_archivo_csv) {
  df <- as.data.frame(df)

  # Exportar el DataFrame a un archivo CSV
  write.csv(df, file = nombre_archivo_csv, row.names = FALSE)

  # Verificar si el archivo se cre? correctamente
  if (file.exists(nombre_archivo_csv)) {
    ruta_absoluta <- normalizePath(nombre_archivo_csv)
    cat(paste("Los datos se han exportado correctamente. La ruta del archivo exportado es", ruta_absoluta, "\n"))
  } else {
    cat("No se pudo crear el archivo CSV.\n")
  }

  # Mostrar parte del contenido exportado
  contenido_exportado <- read.csv(nombre_archivo_csv)
  cat("Contenido del archivo CSV:\n")

  return(contenido_exportado)
}

# CARGA -------------------------------------------------------------------

#' Cargar datos desde un archivo CSV utilizando la biblioteca readr
#'
#' @description Esta funci?n carga datos desde un archivo CSV utilizando la biblioteca readr. Se espera que el archivo CSV tenga un formato v?lido y contenga datos tabulares. Los datos cargados se almacenan en un DataFrame y se muestran en la consola.
#'
#' @param nombre_archivo_csv El nombre del archivo CSV desde el cual se cargar?n los datos.
#'
#' @return Un DataFrame que contiene los datos cargados desde el archivo CSV, o NULL si no se pudieron cargar los datos.
#'
#' @examples
#' Ejemplo de uso
#' Llamar a la funci?n para cargar datos desde un archivo CSV
#' datos_cargados <- cargar_datos_desde_csv(nombre_archivo_csv = "resultados.csv")
#'
#' @export
cargar_datos_desde_csv <- function(nombre_archivo_csv) {
  # Intentar leer datos desde el archivo CSV
  datos_cargados <- read.csv(nombre_archivo_csv)

  # Verificar si se cargaron datos
  if (!is.null(datos_cargados)) {
    cat("Datos cargados exitosamente desde el archivo CSV:\n")
    print(datos_cargados)
    return(datos_cargados)
  } else {
    cat("No se pudieron cargar los datos desde el archivo CSV.\n")
    return(NULL)
  }
}

