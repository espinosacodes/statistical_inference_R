#parcial 3

#1. Se sospecha que la temperatura ambiente en que operan las baterías afecta su vida. Se probaron 30 baterías homogéneas,
#seis por cada una de cinco temperaturas, y los datos se presentan a continuación (vida activa, en segundos):

temperatura_0 <- c(55,55,57,54,54,56)

temperatura_25 <- c(60,61,60,60,60,60)

temperatura_50 <- c(70,72,72,68,77,77)

temperatura_75 <- c(72,72,72,70,68,69)

temperatura_100 <- c(65,66,60,64,65,65)

tabla_de_temperaturas <- data.frame(temp_0 = temperatura_0, temp_25 = temperatura_25, temp_50 = temperatura_50, temp_75 = temperatura_75, temp_100 = temperatura_100)
View(tabla_de_temperaturas)

#tempertatutra y vida activa en segundos 

#___________________________________________________________________________________________________________________________________________________________________
#a. Explora los datos de la muestra mediante gráficos y descriptivos. 
#¿Observamos diferencias en los valores promedios y de variabilidad por grupos?

#grafico de cajas
boxplot(tabla_de_temperaturas, main = "Vida activa de las baterías a diferentes temperaturas", xlab = "Temperatura", ylab = "Vida activa en segundos")

#¿Observamos diferencias en los valores promedios y de variabilidad por grupos?

print("Si, se observan diferencias en los valores promedios y de variabilidad por grupos")
print("Se observa que a mayor temperatura, la vida activa de las baterías disminuye , aunque en 50 grados el tiempo de vida es la más alta")
m <- mean(tabla_de_temperaturas$temp_50)
m



#___________________________________________________________________________________________________________________________________________________________________
#b. Identifique: el factor de estudio y la variable de respuesta. Explique porque los considera así.

factor_de_estudio <- "Temperatura"
variable_de_respuesta <- "Vida activa"

print("Se considera la temperatura como factor de estudio, porque se quiere determinar si la temperatura afecta la vida
      activa de las baterías, y la vida activa como variable de respuesta, porque es la variable que se quiere estudiar")


#___________________________________________________________________________________________________________________________________________________________________
#c. A un nivel de significancia del 5%, se puede asegurar que hay diferencia en la vida activa de las
#baterías a las diferentes temperaturas? Plantee, desarrolle e interprete adecuadamente la hipótesis.

#H0 No hay diferencia en la vida activa de las baterías a las diferentes temperaturas
#H1 Hay diferencia en la vida activa de las baterías a las diferentes temperaturas

#modelo anova
modelo1 <- aov(c(temperatura_0, temperatura_25, temperatura_50, temperatura_75, temperatura_100) ~ c(rep(0,6), rep(25,6), rep(50,6), rep(75,6), rep(100,6)))
summary(modelo1)

print(paste("Se rechaza la hipótesis nula, por lo que se puede asegurar que hay diferencia en la vida activa de
            las baterías a las diferentes temperaturas , debido a que el valor p es menor que 0.05"))


#___________________________________________________________________________________________________________________________________________________________________
# d. Si se rechaza la hipótesis nula de la del ANOVA ¿Cuál de los tratamientos son los significativamente diferentes? 
#Plantee, desarrolle e interprete adecuadamente las hipótesis.

#H0 No hay diferencia en la vida activa de las baterías a las diferentes temperaturas
#H1 Hay diferencia en la vida activa de las baterías a las diferentes temperaturas

#modelo anova
modelo1_1 <- aov(c(temperatura_0, temperatura_25, temperatura_50, temperatura_75, temperatura_100) ~ c(rep(0,6), rep(25,6), rep(50,6), rep(75,6), rep(100,6)))
summary(modelo1_1)

#tomo solo las temperaturas siginificativas

print("en este caso igual se rechaza h0")

print("#los tratamientos significativamente mayores son:
#50 grados
#75 grados
#100 grados")


#___________________________________________________________________________________________________________________________________________________________________
# e. A partir de los residuos obtenidos confirma la validez del ANOVA comprobando el cumplimiento de los supuestos.

#grafico de residuos
plot(modelo1_1, 1)

print("Se observa que los residuos no presentan un patrón claro, por lo que se puede decir que el modelo es adecuado")



#___________________________________________________________________________________________________________________________________________________________________
#2. Con el objetivo de evaluar el PRECIO de la harina (millones de pesos) con base en la PRODUCCIÓN de trigo (toneladas) 
#en el último año en el departamento de Nariño, se tomó una muestra para ajustar un modelo estadístico que relacione 
#las 2 variables y se presenta en la tabla B.  


produccion <- c(22,24,25,25,25,28,30,32,35,40)
    precio <- c(50,45,40,42,40,30,25,27,30,25)
    
tabla_de_precios <- data.frame(produccion = produccion, precio = precio)
View(tabla_de_precios)


#___________________________________________________________________________________________________________________________________________________________________
#a.Determine el coeficiente de correlación lineal entre las variables e interprételo

correlacion <- cor(produccion, precio)
correlacion

print("El coeficiente de correlación lineal entre las variables es de -0.84, lo que indica que existe una correlación negativa fuerte entre las variables")

#___________________________________________________________________________________________________________________________________________________________________
#b.Plantee y construya el modelo de regresión lineal ajustado entre las variables mencionadas e Interprete la pendiente 
#y el intercepto. 

modelo2 <- lm(precio ~ produccion, data = tabla_de_precios)
summary(modelo2)

print("como el valor P es menor que 0.05 se rechaza la hipótesis nula, por lo que se puede decir que el modelo es adecuado")

#___________________________________________________________________________________________________________________________________________________________________
#c.Calcule e interprete el coeficiente de determinación.

r_cuadrado <- summary(modelo2)$r.squared
r_cuadrado

print("El coeficiente de determinación es de 0.71, lo que indica que el 71% de la variabilidad de la variable dependiente es explicada por la variable independiente")



#___________________________________________________________________________________________________________________________________________________________________
#d.Verifique si el modelo pasa por el punto (x=0,y=0).

#modelo de regresión
modelo2 <- lm(precio ~ produccion, data = tabla_de_precios)
summary(modelo2)

print("El modelo no pasa por el punto (0,0), ya que el intercepto es de 74")


#___________________________________________________________________________________________________________________________________________________________________
#e.Plantee la hipótesis de linealidad del modelo y realice la hipótesis. Utilice  alpha = 5%

#H0: El modelo es lineal
#H1: El modelo no es lineal



# Modelo de regresión lineal
modeloLineal <- lm(precio ~ produccion, data = tabla_de_precios)

# Interceptar y pendiente
b0 <- coef(modeloLineal)[1]
b1 <- coef(modeloLineal)[2]

summary(modeloLineal)

# Instalar y cargar el paquete gvlma si aún no está instalado
if (!require(gvlma)) {
  install.packages("gvlma")
  library(gvlma)
}

# Realizar la prueba de linealidad
resultado_gvlma <- gvlma(modeloLineal)

# Imprimir los resultados de la prueba de linealidad
print(summary(resultado_gvlma)$tests[2,])
print("Como el valor P es < 0.05 en se concluye que el modelo no es lineal en los 3 de los parametros")


#___________________________________________________________________________________________________________________________________________________________________
#f.Suponiendo que el modelo es adecuado, determine el valor de Y cuando X tome los valores de 41, 43 y 45 toneladas de trigo, 
#construya e interprete los intervalos de estas predicciones.


# Predicción de los valores de Y
nuevos_valores <- data.frame(produccion = c(41, 43, 45))
predicciones <- predict(modelo2, nuevos_valores, interval = "prediction")
predicciones

print(paste("Cuando X toma el valor de 41 toneladas de trigo, el valor de Y es de 23.5, con un intervalo de predicción de ", predicciones[1]))

print(paste("Cuando X toma el valor de 43 toneladas de trigo, el valor de Y es de 21.5, con un intervalo de predicción de ", predicciones[2]))

print(paste("Cuando X toma el valor de 45 toneladas de trigo, el valor de Y es de 19.5, con un intervalo de predicción de ", predicciones[3]))


#___________________________________________________________________________________________________________________________________________________________________
#g. Pruebe los supuestos del modelo e interprete



# 1. NORMALIDAD de los errores

#h0 los errores siguen una distribución normal
#h1 los errores no siguen una distribución normal

#Prueba de normalidad de los errores
shapiro.test(residuals(modelo2))

print("Se rechaza la hipótesis nula, por lo que se puede decir que los errores no siguen una distribución normal")


#Kolgomorov-Smornov
residuales <- residuals(modelo2)
ks.test(residuales, "pnorm", mean = mean(residuales), sd = sd(residuales))


#Shapiro-Wilk

shapiro.test(residuales)

print("Se rechaza la hipótesis nula, por lo que se puede decir que los errores no siguen una distribución normal")


## 3. Homocedasticidad

#h0 los errores son homocedasticos
#h1 los errores no son homocedasticos

#Prueba de homocedasticidad

library(lmtest)
bptest(modelo2)

print("Se acepta la hipótesis nula, por lo que se puede decir que los errores son homocedasticos")




