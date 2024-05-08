#Ejercicio 1


#______________________________________________________________________________________________________________________________________________________________
# a.	Calcule el coeficiente de correlación 
# lineal. Plantee y desarrolle la hipótesis sobre 
# la correlación   


# Datos de Tiempo (segundos) (Variable X)
tiempo_segundos <- c(918, 805, 892, 962, 968, 907, 770, 743, 1045, 810, 927, 813, 858, 860, 760, 747, 743, 803, 683, 844, 755, 700, 748, 775)

# Datos de Volumen O2 (Variable Y)
volumen <- c(42.33, 53.10, 42.08, 50.06, 42.45, 42.46, 47.82, 49.92, 36.23, 49.66, 41.49, 46.17, 46.18, 43.21, 51.81, 53.28, 53.29, 47.18, 56.91, 47.80, 48.65, 53.67, 60.62, 56.73)

#data frame de las dos variables
datos <- data.frame(volumen, tiempo_segundos) #x and then y
View(datos)

coeficiente_correlacion <- cor(datos$tiempo_segundos, datos$volumen)

print(paste("Coeficiente de correlacion: ", coeficiente_correlacion))

print(paste("Conclusion: Segun con el valor obtenido mediante el coeficiente de relacion",coeficiente_correlacion,
            "determinamos como la variacion del volumen del oxigeno disminuye en funcion del tiempo"))

#realizamos la prueba de hipotesis
#H_0: r = 0 vs H_1: r != 0
#r = 0 significa que no hay correlacion entre las variables
#r != 0 significa que hay correlacion entre las variables

cor.test(datos$tiempo_segundos, datos$volumen)
#como el valor p es menor a 0.05 se rechaza la hipotesis nula y se concluye que hay una correlacion entre las variables
#H_0 no se acepta y
#H_1 se acepta 

#si
#r>0,7 hay buena relación lineal y que si
#r>0,4 hay cierta relación. Se debe tener
#en cuenta el tamaño de la muestra  

print(paste("Se concluye que hay una correlacion entre las variables"))

#valor absoluto del coeficiente de correlacion
coeficiente_correlacion <- abs(coeficiente_correlacion)

if (coeficiente_correlacion > 0.7) {
  print("Hay una buena relacion lineal")
} else if (coeficiente_correlacion > 0.4) {
  print("Hay cierta relacion")

} else {
  print("No hay relacion")
}


#______________________________________________________________________________________________________________________________________________________________

#b. Realice una grafica de dispersión con las variables propuestas


#Diagrama de dispersion
plot(datos$tiempo_segundos, datos$volumen, main="Diagrama de dispersion", xlab="Tiempo", 
     ylab="Variacion del volumen del oxigeno")

abline(lm(datos$volumen ~ datos$tiempo_segundos), col="red")

#______________________________________________________________________________________________________________________________________________________________

#c. Construya el modelo de regresión ajustado. Interprete adecuadamente los parámetros del modelo (intercepto y la pendiente).


#Intercepto y pendiente
modelo <- lm(datos$volumen ~ datos$tiempo_segundos)
modelo$coefficients

print(paste("Para Beta_1 se concluye que por cada segundo que pasa, una persona pierde
            aproximadamente 0.05 de oxigeno", "Para Beta_0 se concluye que cuando el tiempo esta en 0
            segundos el oxigeno seria de aproximadamente 91, lo queindica que esa es el volumen de oxigeno cuando la persona empieza a correr"))          


#Verificar si es necesario usar esto en alguna parte del ejercicio
#abline(modelo, col="red")
#summary(modelo)

#______________________________________________________________________________________________________________________________________________________________

#d. Cree intervalos de confianza para los betas del modelo


#Intervalos de confianza para betas

#para los betas del modelo, alpha=0.05
print("Se concluye que los intervalos de confianza para los betas del modelo son:")

confint(modelo, level = 0.95)

#______________________________________________________________________________________________________________________________________________________________

#e.  Plantee y desarrolle la prueba de hipótesis para comprobar si el modelo pasa por el origen. Explique claramente la conclusión. 



#Prueba de hipotesis para saber si el modelo pasa por el origen
pruebas_hipotesis <- summary(modelo)

p_valor_intercepto <- pruebas_hipotesis$coefficients[1,4]

print(paste("P valor del intercepto: ", p_valor_intercepto))
print(paste("El valor del intercepto es menor al nivel de significancia tomado como 0.05, por lo tanto se 
            rechaza la hipotesis nula y se concluye que el modelo pasa por el origen"))

#______________________________________________________________________________________________________________________________________________________________

#f. Plantee y desarrolle la prueba de hipótesis de linealidad del modelo.


# Instalar y cargar el paquete gvlma si aún no está instalado
if (!require(gvlma)) {
  install.packages("gvlma")
  library(gvlma)
}

# Realizar la prueba de linealidad
resultado_gvlma <- gvlma(modelo)

# Imprimir los resultados de la prueba de linealidad
print(summary(resultado_gvlma)$tests[2,])
print("Como el valor P es < 0.05 se concluye que el modelo no es lineal en los parametros")

#______________________________________________________________________________________________________________________________________________________________

#g. Determine e interprete adecuadamente el coeficiente de determinación del modelo.


print(paste("Coeficiente de determinación (R²): ", pruebas_hipotesis$r.squared))
print(paste("Ya que se tiene un coeficiente de determinacion mas cercano a 1, se concluye que 
            una gran proporción de la variabilidad en la variable dependiente es predecible a 
            partir de la variable independiente"))

#______________________________________________________________________________________________________________________________________________________________

#h. Plantee la hipótesis y compruebe cada uno de los supuestos del modelo de regresión.


# Hipótesis:
# H0: El modelo no es significativo, es decir, todos los coeficientes son cero. H_0 == 0
# H1: Al menos uno de los coeficientes no es cero, es decir, el modelo es significativo. H_1 != 0

print(paste("P valor del modelo: ", summary(modelo)$fstatistic[1]))

#FALTA HACER PARA ESTEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE
#EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE!!!!!!!!!!!11
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1

#______________________________________________________________________________________________________________________________________________________________

#i. Suponiendo que el modelo es adecuado, determine el consumo de oxígeno pronosticado por el modelo y compárelo con el consumo real, para el tiempo utilizado por los deportistas registrados en la siguiente tabla: 
#Tiempo (750, 950,1200)
#Consumo O2 (43.5,51.8,27.5)
#(Pista: Calcule intervalos de predicción para los valores estimados y observe si los valores observados están dentro de los intervalos de predicción)


# Tus nuevos datos
nuevos_datos <- data.frame(tiempo_segundos = c(750, 950, 1200))

# Predicciones del modelo
predicciones <- predict(modelo, newdata = nuevos_datos, interval = "prediction")


#______________________________________________________________________________________________________________________________________________________________
#j. Considera que el modelo sirve para estimar el consumo de oxígeno en los deportistas? Justifique su respuesta basada en los resultados obtenidos.