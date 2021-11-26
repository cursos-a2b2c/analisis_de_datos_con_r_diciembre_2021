#################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biologíca Computacional
#                 Fundación Instituto Leloir
#                        Marzo 2021
#                   Práctica Test de hipótesis
#################################################################

#Primero le decimos a R en qué directorio queremos que trabaje
setwd("~/trabajo/cursos/analisis_de_datos_con_r_octubre_2020/clases/clase_3")

#1) Distribuciones y probabilidad

#a)
#dbinom nos da la probabilidad de que salgan exactamente x caras en size tiradas, con la probabilidad de que salga cara prob
dbinom(x = 5, size = 10, prob = 0.5) #Una moneda justa
dbinom(x = 5, size = 10, prob = 0.75) #Una moneda cargada

#b)
#Usamos lo que vimos en a), con x = 20, size = 50 y prob = 0.25 para obtener exactamente la probabilidad de 20 A
dbinom(x = 20, size = 50, prob = 0.25)

#Podemos sumar la probabilidad con x = 0, 1, 2...20, size = 50 y prob = 0.25 para obtener la probabilidad de 20 A o menos
sum(dbinom(x = 0:20, size = 50, prob = 0.25))

#O podemos usar pbinom, que nos dice exactamente eso, la probabilidad de que salga q o menos
pbinom(q = 20, size = 50, prob = 0.25) #Las dos formas dan lo mismo

#c)
#generamos el vector de x
x <- 0:50
probabilidad <- dbinom(x, size = 50, prob = 0.25)
plot(x, probabilidad)

#d)
#Iniciamos el generador de números aleatorios
set.seed(123457)

#Armamos una función para devuelve una secuencia al azar. Recordar el replace = T
generarSecuencia <- function(){
  bases <- c("A", "C", "G", "T")
  return(sample(bases, 50, replace = T))
}

#Usamos replicate para correr 10000 veces la función que genera las secuencias
secuencias <- replicate(n = 10000, generarSecuencia())
#Tenemos que transponer para obtener las secuencias en filas (formato largo) en lugar de en columnas (formato ancho)
secuencias <- t(secuencias)
#Contamos cuantas veces salió A con rowSums y recordanto que R puede sumar TRUE (1) y FALSE (0)
total <- rowSums(secuencias == "A")

#Dice que grafiquemos un histograma supuerpuesto al gráfico del punto 2 (es un typo, debería decir punto c)
hist(total, add = T, freq = F) #Podemos graficarlo sobre el grafico anterior o graficar un histograma y después superponer. Hacemos la primera opción, la segunda queda de tarea

#¡Se superponen bastante bien! Tomando cada vez más muestras, la propoción de cantidad de "A" debería recuperar la probabilidad real.

#e)
unif <- runif(n = 100000, min = 0, max = 1)
hist(unif)
#Elegimos un p, por ejemplo, p = 0.6
p <- 0.6
sum(unif < p)/length(unif)
#Efectivamente, la probabilidad de que al sacar un número al azar de la distribución esté entre 0 y 0.6 es 0.6

#f)
norm <- rnorm(n = 10000, mean = 1, sd = 2)
mean(norm)
sd(norm)
hist(norm)
#Da todo bastante parecido a la distribución real, pero un poquito diferente porque es una muestra. En una muestra infinita deberíamos obtener los valores reales

#g)
norm <- rnorm(n = 100000, mean = 1, sd = 2)
mean(norm)
sd(norm)
hist(norm)
#Cuanto mayor es la muestra más parecidos esperamos que sean nuestros estimadores a los valores reales.

#h)
#Cargamos el dataset antropometria
antropometria <- read.csv("../clase_3/datasets/antropometria.csv", stringsAsFactors=FALSE)
adultosVarones <- antropometria[antropometria$sex == "M" & antropometria$age > 20, ]
hist(adultosVarones$height, freq = F)
media <- mean(adultosVarones$height, na.rm = T)
desvio <- sd(adultosVarones$height, na.rm = T)
x <- seq(140, 190, by = 1)
y <- dnorm(x, mean = media, sd = desvio)
lines(x, y)
#La altura parece tener efectivamente distribución normal. Obviamente que una normal se extiende para menos y más infinito, pero es una buena aproximación.

#2) Test de hipótesis (siempre voy a suponer significancia de 0.05)
#a)
sleep <- datasets::sleep
head(sleep)#Veamos de qué se trata el dataset
boxplot(extra ~ group, sleep) #Son bastante diferentes ambos grupos.
#Veamos si vale normalidad e igualdad de varianza
shapiro.test(sleep$extra[sleep$group == 1])
shapiro.test(sleep$extra[sleep$group == 2])

#Parece que son normales, veamos varianza. Recordar para el bartlett test que necesitamos pasarle una list con cada grupo.
grupos <- list(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2])
bartlett.test(grupos)

#Vale igualdad de varianza, hagamos el t-test de dos muestras
t.test(sleep$extra[sleep$group == 1], sleep$extra[sleep$group == 2], var.equal = T)
#El pvalue es menor a 0.05, podemos rechazar la hipótesis nula de que las dos medias son iguales. El tratamiento parece funcionar.

#b)
cd4 <- boot::cd4 
head(cd4)#Veamos de qué se trata el dataset
boxplot(cd4$baseline, cd4$oneyear) #Son bastante diferentes ambos grupos. Hay que tener en cuenta que se trata de muestras pareadas.
#Veamos si vale normalidad e igualdad de varianza
shapiro.test(cd4$baseline)
shapiro.test(cd4$oneyear)

#Parece que son normales, veamos varianza. Recordar para el bartlett test que necesitamos pasarle una list con cada grupo.
grupos <- list(cd4$baseline, cd4$oneyear)
bartlett.test(grupos)

#Vale igualdad de varianza, hagamos el t-test pariado (paired = T)
t.test(cd4$baseline, cd4$oneyear, var.equal = T, paired = T)
#El pvalue es menor a 0.05, podemos rechazar la hipótesis nula de que las dos medias son iguales. El tratamiento parece funcionar.

#c)
#Para cargar iris no hace falta maś que usarlo, viene precargado en un data.frame que se llama iris
head(iris)
#Veamos si son normales las medidas

shapiro.test(iris$Petal.Length[iris$Species == "setosa"])
#Casi casi, pero no podemos rechazar que sea normal, así que podemos usar t-test de una muestra para compararlo con la media previa
t.test(iris$Petal.Length[iris$Species == "setosa"], mu = 1.5)
#No podemos rechazar que la media de las nuevas mediciones sea distinta a las anteriores
Petal.Length
#Veamos el largo del sepalo, testeamos normalidad y homogeneidad de varianzas
shapiro.test(iris$Sepal.Length[iris$Species == "setosa"])
shapiro.test(iris$Sepal.Length[iris$Species == "virginica"])
grupos <- list(iris$Sepal.Length[iris$Species == "setosa"], iris$Sepal.Length[iris$Species == "virginica"])
bartlett.test(grupos)
#Las dos varianzas no parecen ser iguales, podemos usar welch (var.equal = F)
t.test(iris$Sepal.Length[iris$Species == "setosa"], iris$Sepal.Length[iris$Species == "virginica"], var.equal = F)
#Son significativamente distintas

#d)
#Instalamos titanic para tener los datos de titanic
install.packages('titanic')
titanic <- titanic::titanic_train 
head(titanic)

#Armemos la tabla de contingencia entre Pclass y Survived
table(titanic$Survived, titanic$Pclass)
#A simple vista, pareciera que los de tercera clase murieron mucho más en proporción que los de segunda, y los de segunda mucho más que los de primera.
#Veamos si es así.
chisq.test(titanic$Survived, titanic$Pclass)
#Rechazamos la hipótesis nula de que las propociones de supervivientes sean iguales entre clases.

#Las otras dos variables quedan de tarea

#e)
#Armamos los datos. De una muestra de 241 personas, se tienen 118 pacientes tratados con el medicamente y 123 con el placebo, de los cuales se curaron 106 y 104 respectivamente.
pacientes <- data.frame(tratado = c(rep(TRUE, 118), rep(FALSE, 123)), 
                        curado = c(rep(TRUE, 106), rep(FALSE, 12), rep(TRUE, 104), rep(FALSE, 19)))
head(pacientes)
#Armemos la tabla de contingencia
table(pacientes$tratado, pacientes$curado)
chisq.test(pacientes$tratado, pacientes$curado)
#No podemos rechazar que no hay efecto.