x <- c(1, 4, 5, 9, 11, 13, 23, 23, 28)
y <- c(64, 71, 54, 81, 76, 93, 77, 95, 109)
datos <- data.frame(x,y)

modelo <- lm(y ~ x, data = datos)
class(modelo)

modelo$coefficients

plot(x, y, main="Diagrama de dispersion", xlab="Incremento en gastos de publicidad"
     , ylab="Incremento en ventas (millones de pesos)")
abline(modelo, col="red")

summary(modelo)
