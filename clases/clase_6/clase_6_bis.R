#################################################################
#              Curso de análisis de datos con R
#          Machine learning: aprendizaje supervisado
#################################################################

#Veamos datos de iris
#This famous (Fisher's or Anderson's) iris data set gives the measurements 
#in centimeters of the variables sepal length and width and petal length and 
#width, respectively, for 50 flowers from each of 3 species of iris. 
#The species are Iris setosa, versicolor, and virginica.

#Exploremos el dataset
View(iris)
colnames(iris)
head(iris)
nrow(iris)
table(iris$Species)
summary(iris)

#Exploremos el dataset graficamente
pairs(iris)
#Alguna de las variables alcanza para distinguir cada especie?

#Grafiquemos los puntos, pero tenemos 4 variables, como podemos hacer?
#PCA!

#Ojo que cada variable esta en una escala diferente (e incluso podrian estar en unidades diferentes m, g, s, etc).
#Tenemos que escalear las variables para que sean comparables
boxplot(iris[, c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")])

#resta la media para centrar y divide por el desvio estandar para que todo quede en unidades de desvio estandar
#z = (x-media)/desvio estandar
iris_escalado <- scale(iris[, c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width")])

#Veamos que informacion hay en iris_escalado
iris_escalado
boxplot(iris_escalado)

#Hacemos PCA
pca <- prcomp(iris_escalado)

#Vemos que porcentaje de varianza explica cada nueva variable
plot(pca$sdev/sum(pca$sdev)*100, xlab = "# variable", ylab = "Porcentaje de variable explicada")
#Con las dos primeras variables ya explicamos mas de un 90%, asi que el grafico en dos dimensiones deberia ser bastante representativo

plot(pca$x[, 1:2])
#Que se observa? a que se puede deber?

#Grafiquemos pero pintemos de un color distinto cada especie
plot(pca$x[, 1:2], col=iris$Species)

#Midamos algunas plantas nuevas, cómo podríamos clasificarlas?
plantas_nuevas <- data.frame(Sepal.Length = c(5.7, 6, 2), Sepal.Width = c(3.2, 2.5, 4), Petal.Length = c(4, 2.5, 6), Petal.Width = c(0.8, 0.75, 2))

#No nos olvidemos de pasarlas a las escalas nuevas, usamos de nuevo scale pero con los datos que obtuvimos antes
plantas_nuevas_escaleadas <- scale(plantas_nuevas, center = attr(iris_escalado, "scaled:center"), scale = attr(iris_escalado, "scaled:scale"))

as.matrix(plantas_nuevas_escaleadas)%*%pca$x

points(plantas_nuevas[, 1:2], col=c("violet", "cyan", "brown"), pch = 19)


#Idea! Podemos usar los vecinos más cercanos al punto para clasificar la nueva medición
#K Nearest Neighbors - K vecinos más cercanos
#Buscamos los K vecinos más próximos a la nueva medición. Qué medida usamos?

#Calculamos la distancia entre la primera planta nueva y las plantas viejas. Pero que es la distancia aca?
distancia <- dist()

#Buscamos los vecinos más próximos, para eso ordenamos las distancias de menor a mayor.
#Qué devuelve order? cuál es la diferencia con sort?
orden <- order(distancia, decreasing = F)

#Qué falta ahora?
K <- 3

#Veamos a qué especies corresponden los vecinos más próximos. 
iris$Species[orden[1:K]]
#Cuantos vecinos tiene de cada especie
vecinos <- table(iris$Species[orden[1:K]])
maximo_vecinos <- which.max(vecinos)
#A qué especie asignaríamos la nueva medición?
especie <- names(maximo_vecinos)
especie