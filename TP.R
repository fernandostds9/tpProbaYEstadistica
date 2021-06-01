##EJERCICIO 1

#Fijo la semilla afuera del for
lambda = 1/6
set.seed(17)
promediosMuestrales1 = c()
for(i in 1:3000){
  muestraEjercicio1 = rexp(i, lambda)
  promediosMuestrales1 = c(promediosMuestrales1, sum(muestraEjercicio1)/i)
}

#plot(array(1:3000), promediosMuestrales, main = "Ejercicio 1", sub="Seed afuera del for", xlab="n", ylab="Promedios muestrales")

#Fijo la semilla adentro del for
lambda = 1/6
promediosMuestrales2 = c()
for(i in 1:3000){
  set.seed(17)
  muestraEjercicio1 = rexp(i, lambda)
  promediosMuestrales2 = c(promediosMuestrales2, sum(muestraEjercicio1)/i)
}

#plot(array(1:3000), promediosMuestrales, ylim = c(-1, 10), main = "Ejercicio 1", sub = "Seed adentro del for", xlab="n", ylab = "Promedios muestrales")

# comparación de todos los gráficos del punto 1
par(mfrow=c(1,2))
plot(array(1:3000), promediosMuestrales1, main = "Ejercicio 1", sub="Seed afuera del for", xlab="n", ylab="Promedios muestrales")
plot(array(1:3000), promediosMuestrales2, ylim = c(-1, 10), main = "Ejercicio 1", sub = "Seed adentro del for", xlab="n", ylab = "Promedios muestrales")

## EJERCICIO 2
# (a)
lambda = 1/6
promediosMuestrales1 = c()
for(i in 1:1000){
  muestra = rexp(2, lambda)
  promedio = sum(muestra)/2 
  promediosMuestrales1 = c(promediosMuestrales1, promedio)
}
hist(promediosMuestrales1, main = "Ejercicio 2a", sub = "Histograma con n=2", xlab = "Promedios muestrales", ylab="Frecuencia")
boxplot(promediosMuestrales1, main = "Ejercicio 2a", sub = "Boxplot con n=2")
qqplot(array(1:1000), promediosMuestrales1, main = "Ejercicio 2a", sub = "QQ plot con n=2", xlab = "n", ylab="Promedios muestrales")

# (b)

lambda = 1/6
promediosMuestrales2 = c()
for(i in 1:1000){
  muestra = rexp(5, lambda)
  promedio = sum(muestra)/5
  promediosMuestrales2 = c(promediosMuestrales2, promedio)
}
hist(promediosMuestrales2, main = "Ejercicio 2b", sub = "Histograma con n=5", xlab = "Promedios muestrales", ylab="Frecuencia")
boxplot(promediosMuestrales2, main = "Ejercicio 2b", sub = "Boxplot con n=5")
qqplot(array(1:1000), promediosMuestrales2, main = "Ejercicio 2b", sub = "QQ plot con n=5", xlab = "n", ylab="Promedios muestrales")

# (c)

# n = 30

lambda = 1/6
promediosMuestrales3 = c()
for(i in 1:1000){
  muestra = rexp(30, lambda)
  promedio = sum(muestra)/30
  promediosMuestrales3 = c(promediosMuestrales3, promedio)
}
hist(promediosMuestrales3, main = "Ejercicio 2c", sub = "Histograma con n=30", xlab = "Promedios muestrales", ylab="Frecuencia")
boxplot(promediosMuestrales3, main = "Ejercicio 2c", sub = "Boxplot con n=30")
qqplot(array(1:1000), promediosMuestrales3, main = "Ejercicio 2c", sub = "QQ plot con n=30", xlab = "n", ylab="Promedios muestrales")

# n = 500

lambda = 1/6
promediosMuestrales4 = c()
for(i in 1:1000){
  muestra = rexp(500, lambda)
  promedio = sum(muestra)/500
  promediosMuestrales4 = c(promediosMuestrales4, promedio)
}
hist(promediosMuestrales4, main = "Ejercicio 2c", sub = "Histograma con n=500", xlab = "Promedios muestrales", ylab="Frecuencia")
boxplot(promediosMuestrales4, main = "Ejercicio 2c", sub = "Boxplot con n=500")
qqplot(array(1:1000), promediosMuestrales4, main = "Ejercicio 2c", sub = "QQ plot con n=500", xlab = "n", ylab="Promedios muestrales")

# comparación de todos los gráficos del punto 2
par(mfrow=c(3,4))
hist(promediosMuestrales1, main = "Ejercicio 2a", sub = "Histograma con n=2", xlab = "Promedios muestrales", ylab="Frecuencia")
hist(promediosMuestrales2, main = "Ejercicio 2b", sub = "Histograma con n=5", xlab = "Promedios muestrales", ylab="Frecuencia")
hist(promediosMuestrales3, main = "Ejercicio 2c", sub = "Histograma con n=30", xlab = "Promedios muestrales", ylab="Frecuencia")
hist(promediosMuestrales4, main = "Ejercicio 2c", sub = "Histograma con n=500", xlab = "Promedios muestrales", ylab="Frecuencia")
boxplot(promediosMuestrales1, main = "Ejercicio 2a", sub = "Boxplot con n=2")
boxplot(promediosMuestrales2, main = "Ejercicio 2b", sub = "Boxplot con n=5")
boxplot(promediosMuestrales3, main = "Ejercicio 2c", sub = "Boxplot con n=30")
boxplot(promediosMuestrales4, main = "Ejercicio 2c", sub = "Boxplot con n=500")
qqplot(array(1:1000), promediosMuestrales1, main = "Ejercicio 2a", sub = "QQ plot con n=2", xlab = "n", ylab="Promedios muestrales")
qqplot(array(1:1000), promediosMuestrales2, main = "Ejercicio 2b", sub = "QQ plot con n=5", xlab = "n", ylab="Promedios muestrales")
qqplot(array(1:1000), promediosMuestrales3, main = "Ejercicio 2c", sub = "QQ plot con n=30", xlab = "n", ylab="Promedios muestrales")
qqplot(array(1:1000), promediosMuestrales4, main = "Ejercicio 2c", sub = "QQ plot con n=500", xlab = "n", ylab="Promedios muestrales")

# (e)
data <- data.frame(promediosMuestrales1, promediosMuestrales2, promediosMuestrales3, promediosMuestrales4)
boxplot(data, main = "Ejercicio 2e", sub = "Boxplots con n = 2, 5, 30 y 500", las = 2, names=c("n = 2", "n = 5", "n = 30", "n = 500"))

## EJERCICIO 3

# (a)

lambda = 1/6
# esperanza de una v.a con dist. exponencial = 1/lambda
esperanza = 1/lambda
# varianza de una v.a con dist. exponencial = 1/(lambda ^ 2)
varianza = 1/(lambda ^ 2)

# (b)

transformaciones1 = c()
for(i in 1:1000){
  muestra = rexp(2, lambda)
  transformacion = (sum(muestra)/2 - esperanza)/sqrt(varianza/2) 
  transformaciones1 = c(transformaciones1, transformacion)
}

transformaciones2 = c()
for(i in 1:1000){
  muestra = rexp(5, lambda)
  transformacion = (sum(muestra)/5 - esperanza)/sqrt(varianza/5) 
  transformaciones2 = c(transformaciones2, transformacion)
}

transformaciones3 = c()
for(i in 1:1000){
  muestra = rexp(30, lambda)
  transformacion = (sum(muestra)/30 - esperanza)/sqrt(varianza/30) 
  transformaciones3 = c(transformaciones3, transformacion)
}

transformaciones4 = c()
for(i in 1:1000){
  muestra = rexp(500, lambda)
  transformacion = (sum(muestra)/500 - esperanza)/sqrt(varianza/500) 
  transformaciones4 = c(transformaciones4, transformacion)
}

# data <- data.frame(transformaciones1, transformaciones2, transformaciones3, transformaciones4)
# boxplot(data, main = "Ejercicio 3b", sub = "Transformaciones con n = 2, 5, 30 y 500", las = 2, names=c("n = 2", "n = 5", "n = 30", "n = 500"))
# par(mfrow=c(1,4))
# qqplot(array(1:1000), transformaciones1, main = "Ejercicio 3b", sub = "QQ plot con n=2", xlab = "n", ylab="Promedios muestrales")
# qqplot(array(1:1000), transformaciones2, main = "Ejercicio 3b", sub = "QQ plot con n=5", xlab = "n", ylab="Promedios muestrales")
# qqplot(array(1:1000), transformaciones3, main = "Ejercicio 3b", sub = "QQ plot con n=30", xlab = "n", ylab="Promedios muestrales")
# qqplot(array(1:1000), transformaciones4, main = "Ejercicio 3b", sub = "QQ plot con n=500", xlab = "n", ylab="Promedios muestrales")

# (c)

par(mfrow=c(1,4))
hist(transformaciones1, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.6), main = "Ejercicio 3c", sub = "Histograma con n=2", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1), col="blue", add = TRUE)
hist(transformaciones2, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.6), main = "Ejercicio 3c", sub = "Histograma con n=5", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1),col="blue", add = TRUE)
hist(transformaciones3, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.6), main = "Ejercicio 3c", sub = "Histograma con n=30", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1),col="blue", add = TRUE)
hist(transformaciones4, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.6), main = "Ejercicio 3c", sub = "Histograma con n=500", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1),col="blue", add = TRUE)

## EJERCICIO 4

# (1)

#Fijo la semilla afuera del for
n = 5
p = 0.1
set.seed(17)
promediosMuestrales1 = c()
for(i in 1:3000){
  muestraEjercicio1 = rbinom(i, n, p)
  promediosMuestrales1 = c(promediosMuestrales1, sum(muestraEjercicio1)/i)
}

#plot(array(1:3000), promediosMuestrales, main = "Ejercicio 1", sub="Seed afuera del for", xlab="n", ylab="Promedios muestrales")

#Fijo la semilla adentro del for
promediosMuestrales2 = c()
for(i in 1:3000){
  set.seed(17)
  muestraEjercicio1 = rbinom(i, n, p)
  promediosMuestrales2 = c(promediosMuestrales2, sum(muestraEjercicio1)/i)
}

#plot(array(1:3000), promediosMuestrales, ylim = c(-1, 10), main = "Ejercicio 1", sub = "Seed adentro del for", xlab="n", ylab = "Promedios muestrales")

# comparación de todos los gráficos del punto 1
par(mfrow=c(1,2))
plot(array(1:3000), promediosMuestrales1, main = "Ejercicio 1", sub="Seed afuera del for", xlab="n", ylab="Promedios muestrales")
plot(array(1:3000), promediosMuestrales2, main = "Ejercicio 1", sub = "Seed adentro del for", xlab="n", ylab = "Promedios muestrales")

# (2.a)
n = 5
p = 0.1
promediosMuestrales1 = c()
for(i in 1:1000){
  muestra = rbinom(2, n, p)
  promedio = sum(muestra)/2 
  promediosMuestrales1 = c(promediosMuestrales1, promedio)
}
# hist(promediosMuestrales1, main = "Ejercicio 2a", sub = "Histograma con n=2", xlab = "Promedios muestrales", ylab="Frecuencia")
# boxplot(promediosMuestrales1, main = "Ejercicio 2a", sub = "Boxplot con n=2")
# qqplot(array(1:1000), promediosMuestrales1, main = "Ejercicio 2a", sub = "QQ plot con n=2", xlab = "n", ylab="Promedios muestrales")

# (2.b)

n = 5
p = 0.1
promediosMuestrales2 = c()
for(i in 1:1000){
  muestra = rbinom(5, n, p)
  promedio = sum(muestra)/5
  promediosMuestrales2 = c(promediosMuestrales2, promedio)
}
# hist(promediosMuestrales2, main = "Ejercicio 2b", sub = "Histograma con n=5", xlab = "Promedios muestrales", ylab="Frecuencia")
# boxplot(promediosMuestrales2, main = "Ejercicio 2b", sub = "Boxplot con n=5")
# qqplot(array(1:1000), promediosMuestrales2, main = "Ejercicio 2b", sub = "QQ plot con n=5", xlab = "n", ylab="Promedios muestrales")

# (2.c)

# n = 30

n = 5
p = 0.1
promediosMuestrales3 = c()
for(i in 1:1000){
  muestra = rbinom(30, n, p)
  promedio = sum(muestra)/30
  promediosMuestrales3 = c(promediosMuestrales3, promedio)
}
# hist(promediosMuestrales3, main = "Ejercicio 2c", sub = "Histograma con n=30", xlab = "Promedios muestrales", ylab="Frecuencia")
# boxplot(promediosMuestrales3, main = "Ejercicio 2c", sub = "Boxplot con n=30")
# qqplot(array(1:1000), promediosMuestrales3, main = "Ejercicio 2c", sub = "QQ plot con n=30", xlab = "n", ylab="Promedios muestrales")

# n = 500

n = 5
p = 0.1
promediosMuestrales4 = c()
for(i in 1:1000){
  muestra = rbinom(500, n, p)
  promedio = sum(muestra)/500
  promediosMuestrales4 = c(promediosMuestrales4, promedio)
}
# hist(promediosMuestrales4, main = "Ejercicio 2c", sub = "Histograma con n=500", xlab = "Promedios muestrales", ylab="Frecuencia")
# boxplot(promediosMuestrales4, main = "Ejercicio 2c", sub = "Boxplot con n=500")
# qqplot(array(1:1000), promediosMuestrales4, main = "Ejercicio 2c", sub = "QQ plot con n=500", xlab = "n", ylab="Promedios muestrales")

# comparación de todos los gráficos del punto 2
 par(mfrow=c(1,4))
# hist(promediosMuestrales1, main = "Ejercicio 2a", sub = "Histograma con n=2", xlab = "Promedios muestrales", ylab="Frecuencia")
# hist(promediosMuestrales2, main = "Ejercicio 2b", sub = "Histograma con n=5", xlab = "Promedios muestrales", ylab="Frecuencia")
# hist(promediosMuestrales3, main = "Ejercicio 2c", sub = "Histograma con n=30", xlab = "Promedios muestrales", ylab="Frecuencia")
# hist(promediosMuestrales4, main = "Ejercicio 2c", sub = "Histograma con n=500", xlab = "Promedios muestrales", ylab="Frecuencia")
# boxplot(promediosMuestrales1, main = "Ejercicio 2a", sub = "Boxplot con n=2")
# boxplot(promediosMuestrales2, main = "Ejercicio 2b", sub = "Boxplot con n=5")
# boxplot(promediosMuestrales3, main = "Ejercicio 2c", sub = "Boxplot con n=30")
# boxplot(promediosMuestrales4, main = "Ejercicio 2c", sub = "Boxplot con n=500")
 qqnorm(promediosMuestrales1, main = "Ejercicio 2a", sub = "QQ plot con n=2", xlab = "n", ylab="Promedios muestrales")
 qqline(promediosMuestrales1)
 qqnorm(promediosMuestrales2, main = "Ejercicio 2b", sub = "QQ plot con n=5", xlab = "n", ylab="Promedios muestrales")
 qqline(promediosMuestrales2)
 qqnorm(promediosMuestrales3, main = "Ejercicio 2c", sub = "QQ plot con n=30", xlab = "n", ylab="Promedios muestrales")
 qqline(promediosMuestrales3)
 qqnorm(promediosMuestrales4, main = "Ejercicio 2c", sub = "QQ plot con n=500", xlab = "n", ylab="Promedios muestrales")
 qqline(promediosMuestrales4)
 
# (2.e)
data <- data.frame(promediosMuestrales1, promediosMuestrales2, promediosMuestrales3, promediosMuestrales4)
boxplot(data, main = "Ejercicio 2e", sub = "Boxplots con n = 2, 5, 30 y 500", las = 2, names=c("n = 2", "n = 5", "n = 30", "n = 500"))

# (3.a)

n = 5
p = 0.1
# esperanza de una v.a con dist. binomial = n*p
esperanza = n*p
# varianza de una v.a con dist. binomial = n*p*(1-p)
varianza = n*p*(1-p)

# (3.b)

transformaciones1 = c()
for(i in 1:1000){
  muestra = rbinom(2, n, p)
  transformacion = (sum(muestra)/2 - esperanza)/sqrt(varianza/2) 
  transformaciones1 = c(transformaciones1, transformacion)
}

transformaciones2 = c()
for(i in 1:1000){
  muestra = rbinom(5, n, p)
  transformacion = (sum(muestra)/5 - esperanza)/sqrt(varianza/5) 
  transformaciones2 = c(transformaciones2, transformacion)
}

transformaciones3 = c()
for(i in 1:1000){
  muestra = rbinom(30, n, p)
  transformacion = (sum(muestra)/30 - esperanza)/sqrt(varianza/30) 
  transformaciones3 = c(transformaciones3, transformacion)
}

transformaciones4 = c()
for(i in 1:1000){
  muestra = rbinom(500, n, p)
  transformacion = (sum(muestra)/500 - esperanza)/sqrt(varianza/500) 
  transformaciones4 = c(transformaciones4, transformacion)
}

# data <- data.frame(transformaciones1, transformaciones2, transformaciones3, transformaciones4)
# boxplot(data, main = "Ejercicio 3b", sub = "Transformaciones con n = 2, 5, 30 y 500", las = 2, names=c("n = 2", "n = 5", "n = 30", "n = 500"))
# par(mfrow=c(1,4))
# qqplot(array(1:1000), transformaciones1, main = "Ejercicio 3b", sub = "QQ plot con n=2", xlab = "n", ylab="Promedios muestrales")
# qqplot(array(1:1000), transformaciones2, main = "Ejercicio 3b", sub = "QQ plot con n=5", xlab = "n", ylab="Promedios muestrales")
# qqplot(array(1:1000), transformaciones3, main = "Ejercicio 3b", sub = "QQ plot con n=30", xlab = "n", ylab="Promedios muestrales")
# qqplot(array(1:1000), transformaciones4, main = "Ejercicio 3b", sub = "QQ plot con n=500", xlab = "n", ylab="Promedios muestrales")

# (3.c)

par(mfrow=c(1,4))
hist(transformaciones1, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.8), main = "Ejercicio 3c", sub = "Histograma con n=2", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1),col="blue", add = TRUE)
hist(transformaciones2, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.8), main = "Ejercicio 3c", sub = "Histograma con n=5", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1),col="blue", add = TRUE)
hist(transformaciones3, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.8), main = "Ejercicio 3c", sub = "Histograma con n=30", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1),col="blue", add = TRUE)
hist(transformaciones4, prob=TRUE, xlim = c(-5,5), ylim = c(0,0.8), main = "Ejercicio 3c", sub = "Histograma con n=500", xlab = "Valor aproximado", ylab="Frecuencia")
curve(dnorm(x, mean = 0, sd = 1),col="blue", add = TRUE)
