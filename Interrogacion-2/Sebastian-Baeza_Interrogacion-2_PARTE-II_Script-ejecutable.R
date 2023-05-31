## Pregunta 1

### Parte a).

### Parte b).

# Test Binomial
# Primero, calculamos el 40% de los encuestados 
# (los que están a favor de la aplicación del impuesto)
round(327*0.4)
# Se calcula el valor p del test Binomial
pbinom(round(327*0.4), 327, 0.5)
# Como el valor-p es muy pequeño, rechazamos $H_0$

# Test Z
# Calculamos el valor p del test
pnorm((137-327*0.5)/sqrt(327*0.5*0.5))
# Nuevamente el valor-p es pequeño

### Parte c).

### Parte d).

# Para tomar la decisión óptima:
# Guardamos al 40% de los encuestados a favor del impuesto:
sumx <- round(327*0.4)
# Guardamos el total de observaciones:
n <- 327
# Calculamos $P(\theta \geq 0.5 | \sum x_i)$,
# como la densidad a posteriori es beta usamos la función conocida de R
1 - pbeta(0.5, 1 + sumx, 1 + n - sumx)
# La probabilidad es muy baja, menor a 1/3, por lo tanto aceptamos $H_1$

## Pregunta 2

## Pregunta 3

### Parte a).

# Cargamos la librería:
library(tidyverse)
# Cargamos los datos en un dataframe llamado arboles
arboles <- read_delim("arboles.csv")
# Ajustamos el modelo, con -1 para no considerar el intercepto
modelo <- lm(altura ~ circunferencia - 1, arboles)
# Añadimos una columna al dataframe arboles 
# con los valores ajustados por la regresión.
# Los valores ajustados ayudarán a graficar la recta de la regresión
arboles$ajuste <- modelo$fitted.values
# Graficamos lo solicitado:
# La recta de la regresión de color rojo
# Los puntos correspondientes a los valores
# usados para ajustar la regresión en azul:
arboles %>%
  ggplot(aes(x = circunferencia, y = altura)) + 
  geom_line(aes(y = ajuste), colour = "#DE0000", lwd = 1) + 
  geom_point(colour = "#070B96") + 
  labs(title = "Longitud de árboles versus ancho del tronco", 
       y = "Altura del árbol", 
       x = "Longitud de la circunferencia del tronco",
       subtitle = "En pulgadas") + 
  theme(plot.title = element_text(size = 17), 
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))
# Y para el valor-p del test t, hacemos un resumen del modelo
# Nos interesa la parte de los coeficientes, para ver si el 
# que está asociado a la variable circunferencia es significativo:
summary(modelo)$coefficients
# La última columna nos muestra el valor-p (muy bajo).  
# Entonces consideramos significativo al regresor circunferencia.

### Parte b).

# Calcularemos el factor de Bayes con la expresión encontrada en 2.
x <- arboles$circunferencia # Valores dados por el regresor: x_i
y <- arboles$altura # Valores de y_i
sigma2 <- 250^2 # \sigma^2 especificado en el enunciado
tau2 <- 100^2 # \tau^2 especificado en el enunciado
# Varianza estimada (en realidad exacta) de la posteriori
varpost <- sigma2 * tau2 / (tau2 * sum(x) + sigma2) 
# Factor de Bayes:
(BF <- (varpost/tau2)^(-1/2) * exp(-varpost * (sum(x*y)/sigma2)^2 / 2))
# Si calculamos la exponencial, 
# el número es tan pequeño que se redondea a cero:
exp(-varpost * (sum(x*y)/sigma2)^2 / 2)
# En vez de calcular el Factor de Bayes, 
# podemos aplicar logaritmo en base 10 y evitar 
# el redondeo de la exponencial.
(logBF <- -1/2 * log10(varpost/tau2) - (varpost * (sum(x*y)/sigma2)^2 / 2)) * log10(exp(1))
# Por criterio de Kass y Raftery, 
# la evidencia es decisiva a favor de $H_1$, 
# Pues el logaritmo del Factor de Bayes es mucho menor a -2.