#################################################################
#              Curso de análisis de datos con R
#                   Práctica Test de hipótesis
#################################################################

#Test de hipótesis (siempre voy a suponer significancia de 0.05)
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
#Levantamos los datos de pacientes
read.table("pacientes.txt", sep = "\t")
View(pacientes)

#Armemos la tabla de contingencia
table(pacientes$tratado, pacientes$curado)
chisq.test(pacientes$tratado, pacientes$curado)

#No podemos rechazar que no hay efecto.

#f)
cosechas <- read.csv("cosechas.csv")
head(cosechas)
boxplot(yield ~ fertilizer, data = cosechas)
#Veamos si son normales
shapiro.test(cosechas$yield[cosechas$fertilizer == 1])
shapiro.test(cosechas$yield[cosechas$fertilizer == 2])
shapiro.test(cosechas$yield[cosechas$fertilizer == 3])

#Son normales

#Veamos si tienen iguales varianzas
var(cosechas$yield[cosechas$fertilizer == 1])
var(cosechas$yield[cosechas$fertilizer == 2])
var(cosechas$yield[cosechas$fertilizer == 3])

#Son bastante parecidas, usamos anova
one.way <- aov(yield ~ fertilizer, data = cosechas)
summary(one.way)
