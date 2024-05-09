
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
datos <- data.frame(volumen, tiempo_segundos) 
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
summary(modelo)
anova(modelo)
#TAREA: Interpretar correctamente la prueba de hipotesis
# sobre la linealidad del modelo

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
# de determinación del modelo.
#R^2
#cor(volumen, tiempo_segundos)^2 #Coeficiente de correlación

analisis_var=anova(modelo)
analisis_var$`Sum Sq`[1]/(sum(analisis_var$`Sum Sq`))

print(paste("Coeficiente de determinación (R²): ", pruebas_hipotesis$r.squared))#coeficiente de correlacion
print(paste("Ya que se tiene un coeficiente de determinacion mas cercano a 1, se concluye que 
            una gran proporción de la variabilidad en la variable dependiente es predecible a 
            partir de la variable independiente"))

#______________________________________________________________________________________________________________________________________________________________

#h. Plantee la hipótesis y compruebe cada uno de los supuestos del modelo de regresión.



# 1. NORMALIDAD de los errores
# H_0: Los e_i tiene distribución normal
# H_1: Los e_i NO tienen distribución normal

#Kolgomorov-Smornov
residuales=rstandard(modelo)
ks.test(residuales,"pnorm")
# p-value = 0.3555 > alpha=0.05
#Shapiro-Wilk
shapiro.test(residuales)
# p-value = 0.001783 < alpha=0.05

#Encontramos que con la prueba kolgomorov smirnov no 
#rechazamos la hipotesis de normalidad, sin embargo,
#con la prueba shapiro wilk rechazamos la hipotesis de
#normalidad, por tanto, concluiremos que el supuesto
#de normalidad de los errores no se está cumpliendo
#en este modelo

# 2.  Independenia de los errores
#     H_0: Los errores son independientes
#     H_1: Los errores NO son independientes
if (!require(car)) {
  install.packages("car")
  library(car)
}

durbinWatsonTest(modelo)
# Valor p = 0.352> alpha = 0.05
# No rechazamos la hipotesis que plantea que los
# errores son independientes.

# 3. Homocedasticidad
# H_0: Var(e_i)==0
# H_1: Var(e_i)!=0
if (!require(lmtest)) {
  install.packages("lmtest")
  library(lmtest)
}
bptest(modelo)
#p-value = 0.7987 alpha = 0.05
#No hay evidencia suficiente para rechazar H_0
# se cumple el supuesto de homocedasticidad



#______________________________________________________________________________________________________________________________________________________________

#i. Suponiendo que el modelo es adecuado, determine el consumo de oxígeno pronosticado por el modelo y compárelo con el consumo real, para el tiempo utilizado por los deportistas registrados en la siguiente tabla: 
#Tiempo (750, 950,1200)
#Consumo O2 (43.5,51.8,27.5)
#(Pista: Calcule intervalos de predicción para los valores estimados y observe si los valores observados están dentro de los intervalos de predicción)


# Your new data
nuevos_datos <- data.frame(tiempo_segundos = c(750, 950, 1200))

# Predictions from the model
predicciones <- predict(modelo, newdata = nuevos_datos, interval = "prediction", level = 0.95)

# Actual oxygen consumption values
consumo_real <- c(43.5, 51.8, 27.5)

print(predicciones)




#______________________________________________________________________________________________________________________________________________________________
#j. Considera que el modelo sirve para estimar el consumo de oxígeno en los deportistas? Justifique su respuesta basada en los resultados obtenidos.

print(paste("el modelo proporciona una herramienta útil para estimar el consumo de oxígeno en función del tiempo"))









###############################################################################################################################################################

#ejercicio 2




# Datos de numero de cajas (Variable x)
numero_cajas <- c(52,	64,	73,	85,	103,	121,	157,	184,	218,	254,	275)

# Datos de Tiempo (min) (Variable y)
tiempo_entrega<- c(32.1,34.8,	36.2,	37.8,	39.7,	41.9,	47.1,	49.4,	56.8,	61.2,	63.1)
                   
#data frame de las dos variables
datos2 <- data.frame(tiempo_entrega, numero_cajas) #x and then y
View(datos2)

#______________________________________________________________________________________________________________________________________________________________
#a. Obtenga un diagrama de dispersión donde X: Numero de cajas  e Y: Tiempo de entrega (min).

plot(numero_cajas, tiempo_entrega, main="Diagrama de dispersión", xlab="Número de cajas", ylab="Tiempo de entrega (min)")


abline(lm(datos2$tiempo_entrega ~ datos2$numero_cajas), col="red")


#______________________________________________________________________________________________________________________________________________________________
#b. Obtenga el modelo de regresión lineal estimado:  y ̂_i=b_0+b_1 x_i 

# Modelo de regresión lineal
modelo <- lm(tiempo_entrega ~ numero_cajas)

# Interceptar y pendiente
b0 <- coef(modelo)[1]
b1 <- coef(modelo)[2]


#Verificar si es necesario usar esto en alguna parte del ejercicio
summary(modelo)


#______________________________________________________________________________________________________________________________________________________________
#c. Interprete b_0 y b_1.


# Interpretación de b0 y b1
interpretacion <- paste("b0 (intercepto) representa el tiempo de entrega cuando el número de cajas es 0. En este contexto, no tiene un significado real.",
                        "b1 (pendiente) representa el cambio esperado en el tiempo de entrega por cada unidad adicional en el número de cajas.",
                        "Es decir, por cada caja adicional, se espera un cambio de", round(b1, 3), "minutos en el tiempo de entrega.")

# Imprimir e interpretar los coeficientes
cat("Intercepto (b0):", b0, "\n")
cat("Pendiente (b1):", b1, "\n")

print(interpretacion)


#______________________________________________________________________________________________________________________________________________________________
#d. Pruebe que el modelo obtenido es lineal α=0.03.

# Instalar y cargar el paquete gvlma si aún no está instalado
if (!require(gvlma)) {
  install.packages("gvlma")
  library(gvlma)
}

# Realizar la prueba de linealidad
resultado_gvlma <- gvlma(modelo)

# Imprimir los resultados de la prueba de linealidad
print(summary(resultado_gvlma)$tests[2,])

# Calcular el valor p asociado a la pendiente
p_valor_pendiente <- summary(modelo)$coefficients[2, 4]

# Nivel de significancia
alpha <- 0.03

# Prueba de hipótesis
if (p_valor_pendiente < alpha) {
  print("No se acepta la hipótesis nula. El modelo es lineal.")
} else {
  print("se acepta la hipótesis nula. No hay suficiente evidencia para afirmar que el modelo es lineal.")
}

#______________________________________________________________________________________________________________________________________________________________
#e. Prediga el tiempo de entrega para 150 cajas de bebida refrescante. Dé los intervalos de predicción y confianza respectivos para un α=0.03.

# Nuevos datos
nuevos_datos <- data.frame(numero_cajas = 150)

# Predicción del tiempo de entrega
prediccion <- predict(modelo, newdata = nuevos_datos, interval = "prediction", level = 0.97)
print(prediccion)

#______________________________________________________________________________________________________________________________________________________________
#f. Haga una interpretación del coeficiente de correlación para el modelo obtenido.

coeficiente_correlacion <- abs(coeficiente_correlacion)

# Evaluamos el valor absoluto del coeficiente de correlación para determinar la fuerza de la relación lineal
if (coeficiente_correlacion > 0.7) {
  print("Hay una buena relación lineal")
} else if (coeficiente_correlacion > 0.4) {
  print("Hay cierta relación")
} else {
  print("No hay relación")
}


#______________________________________________________________________________________________________________________________________________________________
#g. Pruebe que el coeficiente de correlación es diferente de cero. Use α=0.03.

cor_test <- cor.test(datos2$numero_cajas, datos2$tiempo_entrega)
print(paste("Valor p de la prueba de correlación: ", cor_test$p.value))
if (cor_test$p.value < 0.03) {
  print("El coeficiente de correlación es significativamente diferente de cero.")
} else {
  print("El coeficiente de correlación no es significativamente diferente de cero.")
}



#______________________________________________________________________________________________________________________________________________________________
#h. Haga una interpretación del coeficiente determinación obtenido para el modelo.

modelo <- lm(tiempo_entrega ~ numero_cajas, data = datos2)
print(paste("Coeficiente de determinación (R²): ", summary(modelo)$r.squared))



#______________________________________________________________________________________________________________________________________________________________
#i. Obtenga el error estándar de estimación para el modelo.

error_estandar_estimacion <- summary(modelo)$sigma
print(paste("Error estándar de estimación: ", error_estandar_estimacion))

#______________________________________________________________________________________________________________________________________________________________
#j. Aplique las pruebas de normalidad e independencia para los residuos. Comente los resultados
residuos <- residuals(modelo)
# Prueba de normalidad
shapiro_test <- shapiro.test(residuos)
print(paste("Valor p de la prueba de Shapiro-Wilk para normalidad de los residuos: ", shapiro_test$p.value))
# Prueba de independencia

# Load the required package 'car'
if (!require(car)) {
  install.packages("car")
  library(car)
}

durbin_watson_test <- durbinWatsonTest(modelo)
print(durbin_watson_test)



