#Ejemplo de analisis de varianza (ANOVA). 

#Ejemplo de un ejercicio de clase

#  ANOVA

# Haga que este ejemplo sea reproducible
set.seed (0)

#crear marcoteorico de datos

datos <- data.frame(programa = rep(c("A","B","C"), each = 30),
                   perdida_de_peso = c (runif(30,0,3),
                                        runif(30,0,5),
                                        runif(30,1,7)))


head(datos)
View(datos)

install.packages("dplyr")
library(dplyr)
datos %>%
  group_by(programa) %>%
  summarize(media = mean(perdida_de_peso),
            desviacion_estandar = sd(perdida_de_peso))

#Creacion de boxplot

boxplot(perdida_de_peso ~ programa, data = datos, main = "Distribucion de la perdida de peso por programa", 
        xlab = "Programa", ylab = "Perdida de peso", col = "lightgreen")

#Ajuste del modelo de ANOVA unidireccional
modelo <- aov(perdida_de_peso ~ programa, data = datos)

#Ver el resultado del modelo
summary(modelo)

#Post-ANOVA(Post-hoc)
#realizar la prueba de Tukey para comparaciones multiples

library(stats)
TukeyHSD(modelo, conf.level = 0.95)

#Conclusion
#El valor P indica si existe o no una diferencia estadisticamente significativa entre cada programa. 
#Podemos ver en el resultado que hay una diferencia estadisticamente significativa entre la perdida de 
#peso promedio de cada programa al nivel de significancia de 0.05.

