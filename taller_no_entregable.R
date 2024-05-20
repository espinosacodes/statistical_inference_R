tecnica_1 <- c(72,73,73,77,82,82,83,84,85,89)
tecnica_2 <- c(81,82,83,83,83,84,87,90,92,93)
tecnica_3 <- c(77,78,79,88,89,90,91,95,95,98)

tabla_de_puntajes <- data.frame(tecnica_1, tecnica_2, tecnica_3)
View(tabla_de_puntajes)
#a. Identifique: el factor de estudio y la variable de respuesta. Explique porque los considera así.

factor_de_estudio <- "Técnica"
variable_de_respuesta <- "Nota promedio"
variable_de_respuesta <- mean(tabla_de_puntajes$tecnica_1)


#b. A un nivel de significancia del 5%, se puede asegurar que hay diferencia en las notas promedio de las diferentes técnicas? Plantee, desarrolle e interprete adecuadamente la hipótesis.


#c. Cual o cuales rutas de bus considera que son las mejores técnicas? Plantee, desarrolle e interprete adecuadamente las hipótesis. 

#d. Desarrolle las pruebas de supuestos del modelo. Plantee, desarrolle e interprete adecuadamente las hipótesis.

