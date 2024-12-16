
leer_numeros <- function(archivo) {
  if (!file.exists(archivo)) {
    stop("El archivo no existe. Asegúrate de que numeros.txt está en el directorio correcto.")
  }
  numeros <- as.integer(readLines(archivo))
  return(numeros)
}

archivo_entrada <- "numeros.txt"
numeros <- leer_numeros(archivo_entrada)

media <- mean(numeros)
mediana <- median(numeros)
desviacion_estandar <- sd(numeros)

if (desviacion_estandar > 10) {
  mensaje_variabilidad <- "Alta variabilidad detectada: la desviación estándar es mayor a 10."
  print(mensaje_variabilidad)
}

numeros_cuadrados <- sapply(numeros, function(x) x^2)

archivo_salida <- "resultados.txt"
writeLines(c(
  "Resultados del análisis de numeros.txt:",
  "",
  paste("Media:", round(media, 2)),
  paste("Mediana:", mediana),
  paste("Desviación estándar:", round(desviacion_estandar, 2)),
  ifelse(desviacion_estandar > 10, mensaje_variabilidad, ""),
  "",
  "Cuadrados de los números:",
  paste(numeros_cuadrados, collapse = ", ")
), archivo_salida)

cat("El análisis se ha completado. Revisa el archivo resultados.txt para los detalles.\n")
