
#______________________________________________________________________________________________________________________________________________________________

#1.	 Jaime un nuevo empleado de una compañía desea seleccionar la ruta de autobús que lo lleve más rápido a su trabajo.
#Desde su casa al trabajo hay cuatro rutas que le sirven a Jaime. Para tomar con mayor seguridad su decisión, evaluó cinco
#veces el recorrido en cada ruta, y registró el tiempo en minutos que demora cada una desde su casa hasta su trabajo. 
#Los resultados de las observaciones fueron:

Problema1 <- data.frame(Rutas = c(rep("Ruta 1",5),rep("Ruta 2",5),rep("Ruta 3",5),rep("Ruta 4",5)),
                        Tiempo = c(18,21,20,22,19,28,25,23,26,24,20,24,23,25,22,26,34,30,35,30))
Problema1

#______________________________________________________________________________________________________________________________________________________________

#a. Identifique: el factor de estudio y la variable de respuesta. Explique porque los considera así.


print(paste("Factor de estudio: Las rutas de autobús (Ruta 1, Ruta 2, Ruta 3 y Ruta 4).

Se considera factor de estudio a las rutas de autobús porque es la variable que
            se manipula o modifica en el experimento. Jaime evalúa cada ruta cinco veces para
            determinar cuál es la más rápida. Al cambiar la ruta, se espera observar un cambio 
            en el tiempo de viaje (variable de respuesta)."))


print(paste("Se considera variable de respuesta al tiempo de viaje porque es la variable que
            se mide y se observa para cada nivel del factor de estudio.
            El tiempo de viaje depende de la ruta que se tome, por lo que es la variable 
            que responde al cambio en el factor."))

#______________________________________________________________________________________________________________________________________________________________

#b. A un nivel de significancia del 5%, se puede asegurar que hay diferencia en el tiempo promedio de recorrido entre las rutas? Plantee, desarrolle e interprete adecuadamente la hipótesis.

#h0: No hay diferencia en el tiempo promedio de recorrido entre las rutas
#h1: hay diferencia en el tiempo promedio de recorrido entre las rutas

modelo1 <- aov(Tiempo ~ Rutas, data = Problema1)
summary(modelo1)

print(paste("Como el valor de P es menor que el nivel de significancia de 0.05, 
            se rechaza h0 por lo que si hay diferencia en el tiempo promedios de recorrido entre las rutas,
            ANOVA indica que existe una variabilidad significativa en el tiempo de recorrido entre las rutas"))

#______________________________________________________________________________________________________________________________________________________________

#c. Cual o cuales rutas de bus considera que debe tomar Jaime para desplazarse a su lugar de trabajo? 
#Plantee, desarrolle e interprete adecuadamente las hipótesis. 


#h0: No hay diferencia en el tiempo promedio de recorrido entre las rutas
#h1: hay diferencia en el tiempo promedio de recorrido entre las rutas
TukeyHSD(modelo1, conf.level = 0.95)

# Ruta 1:
#
#Tiempo promedio de recorrido: 19.6 minutos
#Comparaciones significativas:
#  Es significativamente más rápida que la Ruta 2 (diferencia de 5.2 minutos).
#Es significativamente más rápida que la Ruta 4 (diferencia de 11 minutos).
#Comparaciones no significativas:
#   No hay diferencia significativa en el tiempo promedio de recorrido con la Ruta 3 (diferencia de 2.8 minutos).
# Ruta 2:
#   
#   Tiempo promedio de recorrido: 24.8 minutos
# Comparaciones significativas:
#   Es significativamente más lenta que la Ruta 1 (diferencia de 5.2 minutos).
# Comparaciones no significativas:
#   No hay diferencia significativa en el tiempo promedio de recorrido con la Ruta 3 (diferencia de -2.4 minutos).
# No hay diferencia significativa en el tiempo promedio de recorrido con la Ruta 4 (diferencia de 5.8 minutos).
# Ruta 3:
#   
#   Tiempo promedio de recorrido: 22.4 minutos
# Comparaciones significativas:
#   No hay diferencia significativa en el tiempo promedio de recorrido con la Ruta 1 (diferencia de 2.8 minutos).
# No hay diferencia significativa en el tiempo promedio de recorrido con la Ruta 2 (diferencia de -2.4 minutos).
# Comparaciones no significativas:
#   No hay diferencia significativa en el tiempo promedio de recorrido con la Ruta 4 (diferencia de 8.2 minutos).
# Ruta 4:
#   
#   Tiempo promedio de recorrido: 30.8 minutos
# Comparaciones significativas:
#   Es significativamente más lenta que la Ruta 1 (diferencia de 11 minutos).
# Es significativamente más lenta que la Ruta 2 (diferencia de 5.8 minutos).
# Comparaciones no significativas:
#   No hay diferencia significativa en el tiempo promedio de recorrido con la Ruta 3 (diferencia de 8.2 minutos).

print(paste("Jaime debería considerar elegir la Ruta 1 como la opción más rápida para llegar a su trabajo.
            La Ruta 1 presenta un menor tiempo promedio de recorrido y
            no tiene diferencias significativas en tiempo con la Ruta 3, que también presenta un menor tiempo
            promedio de recorrido"))

#______________________________________________________________________________________________________________________________________________________________

#d. Desarrolle las pruebas de supuestos del modelo. Plantee, desarrolle e interprete adecuadamente las hipótesis.

shapiro.test(Problema1$Tiempo)

#Kolmogorov-Smornov
ks.test(residuals(modelo1), "pnorm", mean=mean(residuals(modelo1)), sd=sd(residuals(modelo1)))

print("Dado que el valor de P es mayor que el nivel de significancia de 0.05 en ambas pruebas, no se rechaza la
hipótesis nula")


#modelo de regresion
library(lmtest)
dwtest(modelo1)

bptest(modelo1)


print("Dado que el valor de P es mayor que el nivel de significancia de 0.05, no se rechaza la hipótesis nula de
que los errores son aleatorios e independientes.")

#______________________________________________________________________________________________________________________________________________________________


#2.	Una compañía desea comprar una máquina para realizar un trabajo específico, decide realizar un experimento y 
#evaluar cuatro tipos de máquinas sometiéndolos a un trabajo que simula el trabajo a realizar, de tal manera que 
#cada hora de trabajo representa seis meses de tiempo real. En la tabla siguiente registra el tiempo en que la maquina 
#presenta alguna falla.  


#Maquina

#1: 6.4, 7.8, 5.3, 7.4, 8.4
#2: 8.7, 7.4, 9.4, 10.1, 9.2, 9.8
#3: 11.1, 10.3, 9.7, 10.3, 9.2, 8.8
#4: 9.9, 12.8, 12.1, 10.8, 11.3


# Maquina
maquina_1 <- c(6.4, 7.8, 5.3, 7.4, 8.4, NA)
maquina_2 <- c(8.7, 7.4, 9.4, 10.1, 9.2, 9.8)
maquina_3 <- c(11.1, 10.3, 9.7, 10.3, 9.2, 8.8)
maquina_4 <- c(9.9, 12.8, 12.1, 10.8, 11.3, NA)

datos <- data.frame(maquina_1 , maquina_2 , maquina_3 , maquina_4) 
View(datos)


#______________________________________________________________________________________________________________________________________________________________


#A un nivel de significancia del 5%, se puede asegurar que hay diferencia en el tiempo promedio de falla de las maquinas?
#Plantee las hipótesis a probar.

#h0:NO hay diferencia en el tiempo promedio de falla de las maquinas
#h1:hay diferencia en el tiempo promedio de falla de las maquinas


modelo2 <- aov(tiempo_falla ~ maquina, data = datos)
summary(modelo2)

#p-value: 8.498e-12
print("Como el valor de p es menor que el nivel de significancia (0.05), se rechaza la hipótesis nula.")

print("sí existe una diferencia significativa en el tiempo promedio de falla entre las máquinas al nivel
      de significancia del 5%,  hay evidencia suficiente para afirmar que al menos una de las máquinas
      tiene un tiempo promedio de falla diferente al de las demás")
      



#______________________________________________________________________________________________________________________________________________________________


#Que maquina le recomienda a la compañía para que realice el trabajo, justifique y esplique claramente su respuesta.



# Calculate the mean time until failure for each machine
tiempo_promedio_falla <- colMeans(datos, na.rm = TRUE)

# Order the machines by mean time until failure
ordenado <- sort(tiempo_promedio_falla)

# Identify the machine with the highest mean time until failure
mejor_maquina <- names(ordenado)[length(ordenado)]

# Print the recommended machine
print(paste("La máquina recomendada es:", mejor_maquina))


print("máquina tiene el tiempo promedio más alto hasta fallar, lo que la convierte en la opción más confiable
      según los datos.")

