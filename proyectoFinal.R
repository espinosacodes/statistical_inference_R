library(readxl)
respuestas <- read_excel("C:/Users/super/Documents/statistical_inference_R/IA's en los estudiantes (Respuestas).xlsx")


View(respuestas)

library(ggplot2)
#----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extraer la segunda columna
motivos <- respuestas[[2]]

# Crear un dataframe con el conteo de cada categoría
motivos_df <- as.data.frame(table(motivos))
colnames(motivos_df) <- c("motivos", "Frecuencia")

# Calcular la posición de las etiquetas

# Crear el gráfico de pastel
ggplot(motivos_df, aes(x = "", y = Frecuencia, fill = motivos)) +
  geom_col() +
  coord_polar(theta = "y")


library(dplyr)

# Calcular el porcentaje por grupo
motivos_df <- motivos_df %>%
  group_by(motivos) %>%
  mutate(porcentaje = Frecuencia / sum(Frecuencia) * 100)

# Encontrar los tres mayores motivos
top_3_motivos <- motivos_df %>%
  arrange(desc(Frecuencia)) %>%
  top_n(3)

# Mostrar los resultados
top_3_motivos



#----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extraer la tercera columna
IAs <- respuestas[[3]]

# Crear un dataframe con el conteo de cada categoría
IAs_df <- as.data.frame(table(IAs))
colnames(IAs_df) <- c("IAs", "Frecuencia")

# Crear el diagrama de barras
ggplot(IAs_df, aes(x = IAs, y = Frecuencia, fill = IAs)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frecuencia), vjust = -0.5, size = 3.5) +
  labs(title = "Distribución de los IAss de los Entrevistados", x = "IAs", y = "Frecuencia") +
  theme_minimal() +
  theme(legend.position = "none")






#----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extraer la cuarta columna
caracteristicasPreferidas <- respuestas[[4]]

# Crear un dataframe con el conteo de cada categoría
caracteristicasPreferidas_df <- as.data.frame(table(caracteristicasPreferidas))
colnames(caracteristicasPreferidas_df) <- c("Frecuencia", "Cantidad")

# Calcular el porcentaje de cada categoría
caracteristicasPreferidas_df$Porcentaje <- (caracteristicasPreferidas_df$Cantidad / sum(caracteristicasPreferidas_df$Cantidad)) * 100
caracteristicasPreferidas_df$Etiqueta <- paste0(caracteristicasPreferidas_df$Frecuencia, ": ", caracteristicasPreferidas_df$Cantidad, " (", round(caracteristicasPreferidas_df$Porcentaje, 1), "%)")

# Crear el gráfico de pastel
ggplot(caracteristicasPreferidas_df, aes(x = "", y = Cantidad, fill = Frecuencia)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Frecuencia de las carasteristicas preferidas para la IA's", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(labels = caracteristicasPreferidas_df$Etiqueta)







#----------------------------------------------------------------------------------------------------------------------------------------------------------------

library(ggplot2)

# Extraer las respuestas de la quinta columna
tiempoDedicado <- respuestas[[5]]

# Dividir las respuestas separadas por comas en opciones individuales
opciones <- unlist(strsplit(as.character(tiempoDedicado), ", "))

# Crear un dataframe con la frecuencia de cada opción
opciones_df <- as.data.frame(table(opciones))
colnames(opciones_df) <- c("Tiempo", "Frecuencia")

# Ordenar el dataframe por frecuencia descendente
opciones_df <- opciones_df[order(opciones_df$Frecuencia, decreasing = TRUE), ]

# Crear el gráfico de barras con un solo color
ggplot(opciones_df, aes(x = reorder(Tiempo, Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "#FF6666") +
  labs(title = "Tiempo dedicado a actividades académicas usando IA's",
       x = "Tiempo",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




#----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extraer la sexta columna (rango de calificaciones)
calificaciones <- respuestas[[6]]

# Crear un dataframe con el conteo de cada categoría
calificaciones_df <- as.data.frame(table(calificaciones))
colnames(calificaciones_df) <- c("calificaciones", "Frecuencia")

# Calcular el porcentaje de cada categoría
calificaciones_df$Porcentaje <- (calificaciones_df$Frecuencia / sum(calificaciones_df$Frecuencia)) * 100

# Crear el gráfico de pastel
ggplot(calificaciones_df, aes(x = "", y = Frecuencia, fill = calificaciones)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(Porcentaje, 1), "%")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Rango de calificaciones haciendo usao de IA's ", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank())

#----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Convertir la columna de notas a tipo numérico
respuestas$Notas <- as.numeric(as.character(respuestas[[6]]))

# Crear el histograma
ggplot(respuestas, aes(x = Notas)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Notas en materias con más uso de IA's", x = "notas", y = "frecuencia") +
  theme_minimal()





#----------------------------------------------------------------------------------------------------------------------------------------------------------------


# Extraer las respuestas de la séptima columna (satisfaccion)

satisfaccion <- respuestas[[7]]

# Crear un dataframe con la frecuencia de cada opción
satisfaccion_df <- as.data.frame(table(satisfaccion))
colnames(satisfaccion_df) <- c("satisfaccion", "Frecuencia")

# Calcular el porcentaje de cada categoría
satisfaccion_df$Porcentaje <- (satisfaccion_df$Frecuencia / sum(satisfaccion_df$Frecuencia)) * 100

# Crear el gráfico de pastel
ggplot(satisfaccion_df, aes(x = "", y = Frecuencia, fill = satisfaccion)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Frecuencia, " (", round(Porcentaje, 1), "%)")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Satisfaccion de Utilizar IA's en Trabajos Académicos", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank())



library(ggplot2)






#----------------------------------------------------------------------------------------------------------------------------------------------------------------

# Extraer las respuestas de la séptima columna (satisfaccion)

lineasFormacion <- respuestas[[8]]

# Crear un dataframe con la frecuencia de cada opción
lineasFormacion_df <- as.data.frame(table(lineasFormacion))
colnames(lineasFormacion_df) <- c("lineasFormacion", "Frecuencia")

# Calcular el porcentaje de cada categoría
lineasFormacion_df$Porcentaje <- (lineasFormacion_df$Frecuencia / sum(lineasFormacion_df$Frecuencia)) * 100

# Crear el gráfico de pastel
ggplot(lineasFormacion_df, aes(x = "", y = Frecuencia, fill = lineasFormacion)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Frecuencia, " (", round(Porcentaje, 1), "%)")), position = position_stack(vjust = 0.5), color = "white") +
  labs(title = "Lineas de formacion de mas uso de IA's por los estudiantes", x = "", y = "") +
  theme_void() +
  theme(legend.title = element_blank())



library(ggplot2)









#----------------------------------------------------------------------------------------------------------------------------------------------------------------


# Obtener la columna de cantidad de materias uso de IA's
a <- respuestas[[12]]

# Crear un dataframe con el conteo de cada cantidad de materias
conteo_a <- table(a)

# Convertir a dataframe
conteo_a_df <- as.data.frame(conteo_a)
colnames(conteo_a_df) <- c("Cantidad de Materias", "Frecuencia")

# Ordenar el dataframe por la cantidad de materias
conteo_a_df <- conteo_a_df[order(conteo_a_df$`Cantidad de Materias`), ]

# Crear el diagrama de barras
ggplot(conteo_a_df, aes(x = factor(`Cantidad de Materias`), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Cantidad de Materias en las que se Utiliza IA's", x = "Cantidad de Materias", y = "Frecuencia") +
  theme_minimal()



# Obtener la columna de notas finales en la materia con más uso de IA's
notas_finalCHAT <- respuestas[[8]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio materia en que mas se usa IA's:", promedio))
print(paste("Desviación Estándar materia en que mas se usa IA's:", desviacion_estandar))


# Obtener la columna de notas finales en la materia con más uso de IA's
notas_finalCHAT <- respuestas[[9]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio de la materia Sin IA's:", promedio))
print(paste("Desviación Estándar de la materia Sin IA's:", desviacion_estandar))





# Obtener la columna de notas finales en la materia con más uso de IA's
notas_finalCHAT <- respuestas[[10]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio de horas de estudio con IA's:", promedio))
print(paste("Desviación Estándar de horas de estudio con IA's:", desviacion_estandar))



# Obtener la columna de notas finales en la materia con más uso de IA's
notas_finalCHAT <- respuestas[[11]]

# Convertir la columna de notas finales a tipo numérico
notas_finalCHAT <- as.numeric(as.character(notas_finalCHAT))

# Calcular el promedio y la desviación estándar
promedio <- mean(notas_finalCHAT, na.rm = TRUE)
desviacion_estandar <- sd(notas_finalCHAT, na.rm = TRUE)

# Imprimir los resultados
print(paste("Promedio de horas de estudio sin IA's:", promedio))
print(paste("Desviación Estándar de horas de estudio sin IA's:", desviacion_estandar))

