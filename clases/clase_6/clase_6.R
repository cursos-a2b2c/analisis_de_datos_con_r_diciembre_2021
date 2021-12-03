#¿Qué tienen en común estos dos clasificadores?
#Veamos otros datasets
#write.csv(data.frame(height = fish$height, width = fish$width), file="fish.csv", row.names = F)
library(cluster)
fish <- read.csv("~/cursos/analisis_de_datos_con_r_diciembre_2021/clases/clase_6/fish.csv")
plot(fish$height, fish$width)

#Qué podemos decir de estos peces viendo sus medidas? cómo podemos agruparlos?
#.
#.
#.
#Idea! un grupo o cluster es un conjunto de puntos que está más cerca entre si que del resto.
#Usamos K-Means para encontrar los centros de estos grupos. Cada punto es asignado al centro más cercano
K <- 3
clusters <- kmeans(fish, centers = K)
#Veamos que devuelve kmeans.
clusters
plot(silhouette(clusters$cluster, dist(fish)))

#Graficamos los centros que encontró
points(clusters$centers, col=c("red", "blue", "green"), pch = 19)

#Grafiquemos a que grupo quedo asignado cada punto
points(fish, col=c("red", "blue", "green")[clusters$cluster])

#Qué hubiera pasado si elegíamos un K diferente?
plot(fish$height, fish$width)
K <- 5
clusters <- kmeans(fish, centers = K)
#Veamos que devuelve kmeans
clusters
plot(silhouette(clusters$cluster, dist(fish)))

#Graficamos los centros que encontró
points(clusters$centers, col=1:nrow(clusters$centers), pch = 19)

#Grafiquemos a que grupo quedo asignado cada punto
points(fish, col=clusters$cluster)

#Veamos otro dataset. Pokemon
write.csv(Pokemon[, c("Name", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed")], file="pokemon.csv", row.names = F)
pokemon <- read.csv("pokemon.csv")
View(pokemon)
#Ponemos los nombres como rownames y los sacamos del dataset
rownames(pokemon) <- pokemon$Name
pokemon <- pokemon[1:100, -1]

#Grafiquemos algunos pokemon en función de sus parametros
matplot(t(pokemon[1:50, ]), type="l")

#Hay patrones en los datos? difícil de ver a simple vista.
#Calculemos la distancia entre cada uno y veamos si podemos ordenarlos en un arbol
distancia <- dist(pokemon[, -1])
dendrograma<-hclust(distancia)
plot(dendrograma)
#Como encontramos los clusters? Hay que cortar en algún lado!
abline(h = 145, col = "red")
clusters <- cutree(dendrograma, h = 145)
table(clusters)
plot(silhouette(clusters, distancia))

#Grafiquemos los parametros por cluster
layout(matrix(1:6, ncol=2, nrow=3))
for(i in 1:5){
  matplot(t(pokemon[clusters == i, ]), type="l")  
}
layout(1)

#En qué se parecen estos dos datasets y qué tienen de diferente con los dos anteriores? Podemos agruparlos? 
#en qué clases los agruparías?

#############################################
#Solución de los ejercicios, no vale espiar!!
#############################################

#1)
distancia <- sqrt((plantas_nuevas$Petal.Width[2] - iris$Petal.Width)^2 + (plantas_nuevas$Petal.Length[2] - iris$Petal.Length)^2)

#Buscamos los vecinos más próximos, para eso ordenamos las distancias de menor a mayor.
#Qué devuelve order? cuál es la diferencia con sort?
orden <- order(distancia, decreasing = F)

#Qué falta ahora?
K <- 2

#Veamos a qué especies corresponden los vecinos más próximos. 
iris$Species[orden[1:K]]
#Cuantos vecinos tiene de cada especie
vecinos <- table(iris$Species[orden[1:K]])
maximo_vecinos <- which.max(vecinos)
#A qué especie asignaríamos la nueva medición?
especie <- names(maximo_vecinos)
especie

#2) 
distancias <- as.matrix(dist(iris[, c("Sepal.Length", "Sepal.Width")], method = "euclidean"))
accuracy <- c()
for(K in 1:10){
  
  #Como podemos medir cuán bien clasifica nuestro método?
  aciertos <- 0
  for(i in 1:nrow(iris)){
    #Nos quedamos con todas las distancias a la planta i pero sacamos la planta i de 
    #las distancias y ordenamos
    orden <- order(distancias[i, -i])[1:K]
    
    #Vemos a que especies pertenecen los vecinos
    vecinos <- table(iris$Species[-i][orden])
    maximo_vecinos <- which.max(vecinos)
    #Cual es la especie con la que clasificaríamos la observación i
    especie <- names(maximo_vecinos)
    if(especie == iris$Species[i]){
      aciertos <- aciertos + 1
    }
  }
  
  #Medimos la exactitud o accuracy como los aciertos sobre el total
  accuracy <- c(accuracy, aciertos/nrow(iris))
}
plot(1:10, accuracy)

#3) 
boston <- read.csv("boston.csv")
#Exploremos el dataset
View(boston)
colnames(boston)
head(boston)
nrow(boston)
pairs(boston[, c("rm", "ptratio", "medv")])
ajuste <- lm(medv ~ rm + ptratio, boston)
predict.lm(ajuste, data.frame(rm=4, ptratio=21))

#4) 
corruption_vs_gdp <- read.csv("corruption_vs_gdp.csv")
View(corruption_vs_gdp)
colnames(corruption_vs_gdp)
plot(corruption_vs_gdp$CPI, corruption_vs_gdp$GDP)
rownames(corruption_vs_gdp) <- corruption_vs_gdp[, 1]

distancia <- dist(corruption_vs_gdp[, -1])
dendrograma<-hclust(distancia)
plot(dendrograma)
abline(h = 1.5, col = "red")
clusters <- cutree(dendrograma, h = 1.5)
table(clusters)
plot(silhouette(clusters, distancia))
for(i in 1:max(clusters)){
  print(rownames(corruption_vs_gdp)[clusters == i])
  print("--------------------")
}

#5)
mamiferos <- read.csv("mamiferos.csv")
View(mamiferos)
rownames(mamiferos) <- mamiferos[, 1]
matplot(t(mamiferos[, -1]), type="l")
clusters <- kmeans(mamiferos[, -1], centers = 3)
clusters
plot(silhouette(clusters$cluster, dist(mamiferos[, -1])))
for(i in 1:max(clusters$cluster)){
  print(rownames(mamiferos)[clusters$cluster == i])
  print("--------------------")
}


