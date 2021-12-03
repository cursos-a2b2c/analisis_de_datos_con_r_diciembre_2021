#################################################################
#              Curso de análisis de datos con R
#          Machine learning: aprendizaje no supervisado
#################################################################
#A veces queremos irnos un poco de la estadistica y hacer otro tipo de analisis.
#En particular, nos va a interesar no solo modelar nuestros datos, sino tambien
#poder encontrar patrones que no son obvios y poder predecir el comportamiento
#de nuevas observaciones.

#Veamos un dataset de medidas de peces. Ancho y alto (?) o largo
fish <- read.csv("~/cursos/analisis_de_datos_con_r_diciembre_2021/clases/clase_6/fish.csv")
plot(fish$height, fish$width)

#Qué podemos decir de estos peces viendo sus medidas? Tendran algo en comun? Hay grupos de peces?
#.
#.
#.
#Idea! un grupo o cluster es un conjunto de puntos que está más cerca entre si que del resto (o algo por el estilo).
#Pero ojo con la escala. Esta todo en la misma escala? Deberiamos pasar todo a la misma escala para que sean comparables...
plot(fish$height, fish$width, xlim = c(10, 50), ylim = c(10, 50))

fish_escaleado <- as.data.frame(scale(fish, center = T, scale = T))
plot(fish_escaleado$height, fish_escaleado$width, xlim = c(-2, 2), ylim = c(-2, 2))

#Este dataset era facil, un poco de juguete, veamos alguno mas real, con mas dimensiones

mamiferos <- read.csv("~/cursos/analisis_de_datos_con_r_diciembre_2021/clases/clase_6/mamiferos.csv")
View(mamiferos)
nombres <- mamiferos$name #Me guardo los nombres
mamiferos <- mamiferos[, -1] #Me quedo solo con los datos
pairs(mamiferos) #Se ve algun patron?

#Como podriamos graficar en 5 dimensiones?
#PCA al rescate
mamiferos.pca <- prcomp(mamiferos, center = T, scale. = T) #Centramos y escalamos para que todas las variables esten en la misma escala y dimension
plot(mamiferos.pca$sdev/sum(mamiferos.pca$sdev)*100, xlab = "# variable", ylab = "Porcentaje de variable explicada")
#Como explicamos alrededor de un 80% de la variabilidad con las primeras dos componentes, deberia ser una buena proyeccion de como se ven nuestros datos

plot(mamiferos.pca$x[, 1:2], main = "Mamiferos")
text(mamiferos.pca$x[, 1:2],  nombres, cex=0.65, pos=3,col="red") 

#A simple vista se ven algunos grupos cuando agregamos los nombres. Con PCA proyectamos muchas dimensiones en solo dos, transformando nuestras medidas 
#como con cualquier mapa.
#Habra patrones en nuestros datos que no podemos ver, por nuestra incapacidad de ver mas de 3 dimensiones? O porque los espacios no siempre son tan obvios?



#Volvamos al dataset de peces
plot(fish_escaleado$height, fish_escaleado$width, xlim = c(-2, 2), ylim = c(-2, 2))

#Usamos K-Means para encontrar los centros de estos grupos. Cada punto es asignado al centro más cercano
library(cluster)
K <- 3
clusters <- kmeans(fish_escaleado, centers = K)
#Veamos que devuelve kmeans.
clusters

#Grafiquemos a que grupo quedo asignado cada punto
points(fish_escaleado, col=c("red", "blue", "green")[clusters$cluster])

#Graficamos los centros que encontró
points(clusters$centers, col=c("red", "blue", "green"), pch = 19)

#¿Qué onda todo esto? 

#¿El K fue el correcto u otro hubiera funcionado mejor? ¿Funciono bien? ¿Como podríamos medirlo?
#Podemos usar alguna propiedad de los grupos que sea de interés y encontrar el k que la maximice (o minimice).
#En este caso, queremos encontrar grupos compactos, donde todos los elementos de un grupo estén lo más cerca posible de su centro. 
#Podemos sumar las distancias de cada punto a su respectivo centro (SSE o suma de los cuadrados de los residuos)
#y usar eso como medida.
clusters$betweenss

plot(silhouette(clusters$cluster, dist(fish_escaleado)))


#Qué hubiera pasado si elegíamos un K diferente?
plot(fish_escaleado$height, fish_escaleado$width)
K <- 5

clusters <- kmeans(fish_escaleado, centers = K)
#Veamos que devuelve kmeans
clusters

#Grafiquemos a que grupo quedo asignado cada punto
plot(fish_escaleado, col=clusters$cluster)

#Graficamos los centros que encontró
points(clusters$centers, col=1:nrow(clusters$centers), pch = 19)

#Veamos el silhouette
plot(silhouette(clusters$cluster, dist(fish_escaleado)))


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


