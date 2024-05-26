library(readxl)
respuestas <- read_excel("C:/Users/super/Documents/statistical_inference_R/IA's en los estudiantes (Respuestas).xlsx")


View(respuestas)

library(ggplot2)

# Extraer la segunda columna
sexo <- respuestas[[2]]

# Crear un dataframe con el conteo de cada categoría
sexo_df <- as.data.frame(table(sexo))
colnames(sexo_df) <- c("Sexo", "Frecuencia")

# Calcular la posición de las etiquetas
sexo_df$Porcentaje <- (sexo_df$Frecuencia / sum(sexo_df$Frecuencia)) * 100
sexo_df$Posicion <- cumsum(sexo_df$Frecuencia) - sexo_df$Frecuencia / 2

# Crear el gráfico de pastel
ggplot(sexo_df, aes(x = "", y = Frecuencia, fill = Sexo)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Frecuencia, " (", round(Porcentaje, 1), "%)"), y = Posicion), color = "white") +
  labs(title = "Distribución del Sexo de los Entrevistados", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank())



# Extraer la tercera columna
semestre <- respuestas[[3]]

# Crear un dataframe con el conteo de cada categoría
semestre_df <- as.data.frame(table(semestre))
colnames(semestre_df) <- c("Semestre", "Frecuencia")

# Crear el diagrama de barras
ggplot(semestre_df, aes(x = Semestre, y = Frecuencia, fill = Semestre)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, size = 3.5) +
  labs(title = "Distribución de los Semestres de los Entrevistados", x = "Semestre", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")



# Extraer la cuarta columna
uso_chatgpt <- respuestas[[4]]

# Crear un dataframe con el conteo de cada categoría
uso_chatgpt_df <- as.data.frame(table(uso_chatgpt))
colnames(uso_chatgpt_df) <- c("Frecuencia", "Cantidad")

# Calcular el porcentaje de cada categoría
uso_chatgpt_df$Porcentaje <- (uso_chatgpt_df$Cantidad / sum(uso_chatgpt_df$Cantidad)) * 100
uso_chatgpt_df$Etiqueta <- paste0(uso_chatgpt_df$Frecuencia, ": ", uso_chatgpt_df$Cantidad, " (", round(uso_chatgpt_df$Porcentaje, 1), "%)")

# Crear el gráfico de pastel
ggplot(uso_chatgpt_df, aes(x = "", y = Cantidad, fill = Frecuencia)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Frecuencia de Uso de ChatGPT en Actividades Académicas", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(labels = uso_chatgpt_df$Etiqueta)


# Extraer las respuestas de la quinta columna
actividades <- respuestas[[5]]

# Dividir las respuestas separadas por comas en opciones individuales
opciones <- unlist(strsplit(as.character(actividades), ", "))

# Crear un dataframe con la frecuencia de cada opción
opciones_df <- as.data.frame(table(opciones))
colnames(opciones_df) <- c("Actividad", "Frecuencia")

# Ordenar el dataframe por frecuencia descendente
opciones_df <- opciones_df[order(opciones_df$Frecuencia, decreasing = TRUE), ]

# Crear el gráfico de barras
ggplot(opciones_df, aes(x = reorder(Actividad, Frecuencia), y = Frecuencia, fill = Actividad)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, size = 3.5, color = "black") +
  labs(title = "Actividades Académicas con ChatGPT", x = "Actividad", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Extraer la sexta columna (rendimiento académico sin el uso de ChatGPT)
rendimiento_sin_chatgpt <- respuestas[[6]]

# Crear un dataframe con el conteo de cada categoría
rendimiento_sin_chatgpt_df <- as.data.frame(table(rendimiento_sin_chatgpt))
colnames(rendimiento_sin_chatgpt_df) <- c("Rendimiento", "Frecuencia")

# Calcular el porcentaje de cada categoría
rendimiento_sin_chatgpt_df$Porcentaje <- (rendimiento_sin_chatgpt_df$Frecuencia / sum(rendimiento_sin_chatgpt_df$Frecuencia)) * 100

# Crear el gráfico de pastel
ggplot(rendimiento_sin_chatgpt_df, aes(x = "", y = Frecuencia, fill = Rendimiento)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Rendimiento Académico sin Uso de ChatGPT", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank())




# Extraer las respuestas de la séptima columna
necesidad_chatgpt <- respuestas[[7]]

# Crear un dataframe con la frecuencia de cada opción
necesidad_chatgpt_df <- as.data.frame(table(necesidad_chatgpt))
colnames(necesidad_chatgpt_df) <- c("Necesidad", "Frecuencia")

# Calcular el porcentaje de cada categoría
necesidad_chatgpt_df$Porcentaje <- (necesidad_chatgpt_df$Frecuencia / sum(necesidad_chatgpt_df$Frecuencia)) * 100

# Crear el gráfico de pastel
ggplot(necesidad_chatgpt_df, aes(x = "", y = Frecuencia, fill = Necesidad)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Frecuencia, " (", round(Porcentaje, 1), "%)")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Necesidad de Utilizar ChatGPT en Trabajos Académicos", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank())



library(ggplot2)


# Convertir la columna de notas finales a tipo numérico
respuestas$Notas <- as.numeric(as.character(respuestas[[8]]))

# Crear el histograma
ggplot(respuestas, aes(x = Notas)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Notas Finales en la Materia con Más Uso de ChatGPT", x = "Nota Final", y = "Frecuencia") +
  theme_minimal()



# Obtener la columna de notas finales en la materia sin uso de ChatGPT
notas_sin_chatgpt <- respuestas[[9]]

# Convertir la columna de notas finales a tipo numérico
notas_sin_chatgpt <- as.numeric(as.character(notas_sin_chatgpt))

# Crear el histograma
ggplot(data.frame(Notas = notas_sin_chatgpt), aes(x = Notas)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Notas Finales en la Materia sin Uso de ChatGPT", x = "Nota Final", y = "Frecuencia") +
  theme_minimal()



# Obtener la columna de notas finales en el estudio con ChatGPT
notas_con_chatgpt <- respuestas[[10]]

# Convertir la columna de notas finales a tipo numérico
notas_con_chatgpt <- as.numeric(as.character(notas_con_chatgpt))

# Crear el histograma
ggplot(data.frame(Notas = notas_con_chatgpt), aes(x = Notas)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Horas de estudio con ChatGPT", x = "Horas", y = "Frecuencia") +
  theme_minimal()



# Obtener la columna de notas finales en el estudio con ChatGPT
notas_con_chatgpt <- respuestas[[11]]

# Convertir la columna de notas finales a tipo numérico
notas_con_chatgpt <- as.numeric(as.character(notas_con_chatgpt))

# Crear el histograma
ggplot(data.frame(Notas = notas_con_chatgpt), aes(x = Notas)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Horas de estudio sin ChatGPT", x = "Horas", y = "Frecuencia") +
  theme_minimal()



# Obtener la columna de cantidad de materias uso de ChatGPT
materias_con_chatgpt <- respuestas[[12]]

# Crear un dataframe con el conteo de cada cantidad de materias
conteo_materias_con_chatgpt <- table(materias_con_chatgpt)

# Convertir a dataframe
conteo_materias_con_chatgpt_df <- as.data.frame(conteo_materias_con_chatgpt)
colnames(conteo_materias_con_chatgpt_df) <- c("Cantidad de Materias", "Frecuencia")

# Ordenar el dataframe por la cantidad de materias
conteo_materias_con_chatgpt_df <- conteo_materias_con_chatgpt_df[order(conteo_materias_con_chatgpt_df$`Cantidad de Materias`), ]

# Crear el diagrama de barras
ggplot(conteo_materias_con_chatgpt_df, aes(x = factor(`Cantidad de Materias`), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Cantidad de Materias en las que se Utiliza ChatGPT", x = "Cantidad de Materias", y = "Frecuencia") +
  theme_minimal()



# Obtener la columna de notas finales en la materia con más uso de ChatGPT
notas_finalCHAT <- respuestas[[8]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio materia en que mas se usa chatGPT:", promedio))
print(paste("Desviación Estándar materia en que mas se usa chatGPT:", desviacion_estandar))


# Obtener la columna de notas finales en la materia con más uso de ChatGPT
notas_finalCHAT <- respuestas[[9]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio de la materia Sin ChatGPT:", promedio))
print(paste("Desviación Estándar de la materia Sin ChatGPT:", desviacion_estandar))





# Obtener la columna de notas finales en la materia con más uso de ChatGPT
notas_finalCHAT <- respuestas[[10]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio de horas de estudio con ChatGPT:", promedio))
print(paste("Desviación Estándar de horas de estudio con ChatGPT:", desviacion_estandar))



# Obtener la columna de notas finales en la materia con más uso de ChatGPT
notas_finalCHAT <- respuestas[[11]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio de horas de estudio sin ChatGPT:", promedio))
print(paste("Desviación Estándar de horas de estudio sin ChatGPT:", desviacion_estandar))

