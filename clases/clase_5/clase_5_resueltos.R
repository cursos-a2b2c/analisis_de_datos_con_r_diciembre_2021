#################################################################
#              Curso de análisis de datos con R
#          Machine learning: aprendizaje no supervisado
#                    Ejercicios resueltos
#################################################################
#Observamos el dataset
View(iris)
head(iris)
ncol(iris) #Cuantas columnas
nrow(iris) #Cuantas filas

#Guardamos la especie y nos quedamos solo con las variables numericas
especies <- iris$Species
iris_datos <- iris[, -5]

#Hacemos PCA
iris.pca <- prcomp(iris_datos, center = T, scale. = T)
plot(iris.pca$sdev/sum(iris.pca$sdev)*100, xlab = "# variable", ylab = "Porcentaje de variabilidad explicada", type="b")

#Usamos dos variables
plot(iris.pca$x[, 1:2], main = "Iris") #Pareciera haber solo dos grupos
points(iris.pca$x[, 1:2], col = especies) #Pintamos por especie y se ven los tres grupos, pero hay dos que se mezclan un poco.

#Hacemos kmeans pero primero reescalamos
iris_escalado <- as.data.frame(scale(iris_datos, center = T, scale = T))
library(cluster)
set.seed(1234567)
sse <- c()
clusters <- kmeans(iris_escalado, centers = 2)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(iris_escalado, centers = 3)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(iris_escalado, centers = 4)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(iris_escalado, centers = 5)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(iris_escalado, centers = 6)
sse      <- c(sse, sum(clusters$withinss))
plot(2:6, sse, type = "b", xlab = "k") #Criterio del codo
#Nos quedamos con 3 grupos
clusters <- kmeans(iris_escalado, centers = 3)

#Graficamos
plot(iris.pca$x[, 1:2], main = "Iris", col = clusters$cluster) 

#Se los ve bastante bien, comparemos contra las especies
layout(matrix(1:2, nrow = 2))
plot(iris.pca$x[, 1:2], main = "Iris especies", col = especies) 
plot(iris.pca$x[, 1:2], main = "Iris clusters", col = clusters$cluster) 
layout(1)

#Los puntos centrales se clasifican facil, los que estan en las fronteras de los grupos o medio perdidos, no. 
#Kmeans solo puede encontrar grupos compactos

#Veamos clustering jerarquico
distancia    <- dist(iris_escalado, method = "euclidean")
dendrograma  <- hclust(distancia, method = "complete")
plot(dendrograma, labels = F, hang = -1)

#Cortamos en 3.5 a ojimetro
abline(h = 3.5, col = "red")
clusters <- cutree(dendrograma, h = 3.5)
table(clusters)

#Encontramos 5 clusters, comparemos nuevamente contra las especies
layout(matrix(1:2, nrow = 2))
plot(iris.pca$x[, 1:2], main = "Iris especies", col = especies) 
plot(iris.pca$x[, 1:2], main = "Iris clusters", col = clusters) 
layout(1)

#Los divide mucho, probemos cortar mas arriba
clusters <- cutree(dendrograma, h = 5)
table(clusters)
layout(matrix(1:2, nrow = 2))
plot(iris.pca$x[, 1:2], main = "Iris especies", col = especies) 
plot(iris.pca$x[, 1:2], main = "Iris clusters", col = clusters) 
layout(1)

#Un poco mejor que ntes pero definitivamente para este tipo de clusters funciona mejor kmeans, encuentra objetos mas compactos

###################################################
#Observamos el dataset
mamiferos <- read.csv("mamiferos.csv")
View(mamiferos)
head(mamiferos)
ncol(mamiferos) #Cuantas columnas
nrow(mamiferos) #Cuantas filas

#Guardamos el nombre del animal y nos quedamos solo con las variables numericas
animales <- mamiferos$name
animales_datos <- mamiferos[, -1]

#Hacemos PCA
animales.pca <- prcomp(animales_datos, center = T, scale. = T)
plot(animales.pca$sdev/sum(animales.pca$sdev)*100, xlab = "# variable", ylab = "Porcentaje de variabilidad explicada", type="b")

#Usamos dos variables aunque convendria mas 3, pero dificil entender algo en 3
plot(animales.pca$x[, 1:2], main = "Mamiferos") #Pareciera haber solo dos grupos
text(animales.pca$x[, 1:2], animales, cex=0.65, pos=3,col="red") 

#Hacemos kmeans pero primero reescalamos
animales_escalado <- as.data.frame(scale(animales_datos, center = T, scale = T))
library(cluster)
set.seed(1234567)
sse <- c()
clusters <- kmeans(animales_escalado, centers = 2)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(animales_escalado, centers = 3)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(animales_escalado, centers = 4)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(animales_escalado, centers = 5)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(animales_escalado, centers = 6)
sse      <- c(sse, sum(clusters$withinss))
plot(2:6, sse, type = "b", xlab = "k") #Criterio del codo
#Nos quedamos con 4 grupos
clusters <- kmeans(animales_escalado, centers = 4)

#Graficamos
plot(animales.pca$x[, 1:2], main = "Mamiferos", col = clusters$cluster) 
text(animales.pca$x[, 1:2], animales, cex=0.65, pos=3,col="red") 

#No tenemos en este caso un estandar contra el qué comparar, como en el caso de las especies en iris.
#Tenemos que compararlo contra nuestro conocimiento previo. No parecen estar agrupadas de forma muy razonable.
#Por ejemplo, gato y perro estan en clusters distintos, y rata esta en el mismo cluster que ballena

#Veamos clustering jerarquico
distancia    <- dist(animales_escalado, method = "euclidean")
dendrograma  <- hclust(distancia, method = "complete")
plot(dendrograma, labels = animales, hang = -1)

#Cortamos en 2.6 a ojimetro
abline(h = 2.6, col = "red")
clusters <- cutree(dendrograma, h = 2.6)
table(clusters)

#Encontramos 5 clusters, veamos que contienen y si les podemos dar una interpretacion biologica
#Graficamos
plot(animales.pca$x[, 1:2], main = "Mamiferos", col = clusters) 
text(animales.pca$x[, 1:2], animales, cex=0.65, pos=3, col = clusters) 

#Pareciera haber estructura dentro del cluster mas grande, veamos si lo podemos separar
animales_datos_1 <- animales_escalado[clusters == 1, ]
animales_1       <- animales[clusters == 1]
distancia    <- dist(animales_datos_1, method = "euclidean")
dendrograma  <- hclust(distancia, method = "complete")
plot(dendrograma, labels = animales_1, hang = -1)

#Cortamos en 1
abline(h = 1, col = "red")
clusters_1 <- cutree(dendrograma, h = 1)
table(clusters_1)
#Vemos que hay
animales_1[clusters_1 == 3] #Parecen animales resistentes tipo camelidos
animales_1[clusters_1 == 1] #Donkey, mule y horse van bien juntos, son todos parientes. Orangutan y monkey podrian haber quedado en otro grupo

#Un poco mejor que antes, definitivamente no son objetos tan compactos
