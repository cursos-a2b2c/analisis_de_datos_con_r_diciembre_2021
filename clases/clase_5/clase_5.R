#################################################################
#              Curso de análisis de datos con R
#          Machine learning: aprendizaje no supervisado
#################################################################
#A veces queremos irnos un poco de la estadistica y hacer otro tipo de analisis.
#En particular, nos va a interesar no solo modelar nuestros datos, sino tambien
#poder encontrar patrones que no son obvios y poder predecir el comportamiento
#de nuevas observaciones.

#Veamos un dataset de medidas de peces. Ancho y alto (?) o largo
fish <- read.csv("fish.csv")
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

mamiferos <- read.csv("mamiferos.csv")
View(mamiferos)
nombres <- mamiferos$name #Me guardo los nombres
mamiferos <- mamiferos[, -1] #Me quedo solo con los datos
pairs(mamiferos) #Se ve algun patron?

#Como podriamos graficar en 5 dimensiones?
#PCA al rescate
mamiferos.pca <- prcomp(mamiferos, center = T, scale. = T) #Centramos y escalamos para que todas las variables esten en la misma escala y dimension
mamiferos.pca
plot(mamiferos.pca$sdev/sum(mamiferos.pca$sdev)*100, xlab = "# variable", ylab = "Porcentaje de variabilidad explicada", type="b")
#Como explicamos alrededor de un 80% de la variabilidad con las primeras dos componentes, deberia ser una buena proyeccion de como se ven nuestros datos

plot(mamiferos.pca$x[, 1:2], main = "Mamiferos")
text(mamiferos.pca$x[, 1:2], nombres, cex=0.65, pos=3,col="red") 

#A simple vista se ven algunos grupos cuando agregamos los nombres. Con PCA proyectamos muchas dimensiones en solo dos, transformando nuestras medidas 
#como con cualquier mapa.
#Habra patrones en nuestros datos que no podemos ver, por nuestra incapacidad de ver mas de 3 dimensiones? O porque los espacios no siempre son tan obvios?



#Volvamos al dataset de peces
plot(fish_escaleado$height, fish_escaleado$width, xlim = c(-2, 2), ylim = c(-2, 2))

#Usamos K-Means para encontrar los centros de estos grupos. Cada punto es asignado al centro más cercano
library(cluster)
K <- 3
#Antes de correrlo, una pregunta conceptual: dado un conjunto de datos y un k, kmeans siempre me da lo mismo? es decir, es determinista?
#o hay elementos aleatorios en el metodo? cual?
#.
#.
#.
#.
#Seteamos el generador de numeros aleatorios para poder reproducir los mismos resultados en un futuro
set.seed(1234567)
clusters <- kmeans(fish_escaleado, centers = K)
#Veamos que devuelve kmeans.
clusters

#Grafiquemos a que grupo quedo asignado cada punto
plot(fish_escaleado, col=c("red", "blue", "green")[clusters$cluster])

#Graficamos los centros que encontró
points(clusters$centers, col=c("red", "blue", "green"), pch = 17)

#¿Qué onda todo esto? 

#¿El K fue el correcto u otro hubiera funcionado mejor? ¿Funciono bien? ¿Como podríamos medirlo?
#.
#.
#.
#.
#Podemos usar alguna propiedad de los grupos que sea de interés y encontrar el k que la maximice (o minimice).
#En este caso, queremos encontrar grupos compactos, donde todos los elementos de un grupo estén lo más cerca posible de su centro. 
#Podemos sumar las distancias de cada punto a su respectivo centro (SSE o suma de los cuadrados de los residuos)
#y usar eso como medida.
clusters$withinss
sum(clusters$withinss) #Idealmente, cuanto deberia ser la distancia? Tiene sentido eso?

#Qué hubiera pasado si elegíamos un K diferente?
plot(fish_escaleado$height, fish_escaleado$width)
K <- 2

clusters <- kmeans(fish_escaleado, centers = K)
#Veamos que devuelve kmeans
clusters

#Grafiquemos a que grupo quedo asignado cada punto
plot(fish_escaleado, col=clusters$cluster)

#Graficamos los centros que encontró
points(clusters$centers, col=1:nrow(clusters$centers), pch = 17)
sum(clusters$withinss)

#probemos varios k y grafiquemos
set.seed(1234567)
sse <- c()
clusters <- kmeans(fish_escaleado, centers = 2)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(fish_escaleado, centers = 3)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(fish_escaleado, centers = 4)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(fish_escaleado, centers = 5)
sse      <- c(sse, sum(clusters$withinss))
clusters <- kmeans(fish_escaleado, centers = 6)
sse      <- c(sse, sum(clusters$withinss))
plot(2:6, sse, type = "b", xlab = "k") #Criterio del codo

#Otra forma de ver esto es usando un grafico de siluetas 
#-------------------------Este codigo no importa!
require(jpeg)
img<-readJPEG("sil.jpg")
plot(1:10,ty="n", xlim = c(0, 10), ylim = c(0, 10))
rasterImage(img,0,0,10,10)
#-------------------------
#Grafico un silhouette
set.seed(1234567)
clusters <- kmeans(fish_escaleado, centers = 3)
plot(silhouette(clusters$cluster, dist(fish_escaleado)))

clusters <- kmeans(fish_escaleado, centers = 2)
plot(silhouette(clusters$cluster, dist(fish_escaleado)))


#Veamos otro dataset. Pokemon
#write.csv(Pokemon[, c("Name", "HP", "Attack", "Defense", "Sp..Atk", "Sp..Def", "Speed")], file="pokemon.csv", row.names = F)
pokemon <- read.csv("pokemon.csv")
View(pokemon)
#Nos quedamos con los nombres y los sacamos del dataset
nombres <- pokemon$Name
pokemon <- pokemon[, -1]

#Grafiquemos a los pokemon. 6 variable, cómo hacemos?
#PCA al rescate
pokemon.pca <- prcomp(pokemon, center = T, scale. = T) #Centramos y escalamos para que todas las variables esten en la misma escala y dimension
plot(pokemon.pca$sdev/sum(pokemon.pca$sdev)*100, xlab = "# variable", ylab = "Porcentaje de variabilidad explicada", type = "b")
#Como explicamos alrededor de un 50% de la variabilidad con las primeras dos componentes 
#no va a ser tan buena la proyeccion de como se ven nuestros datos pero es lo que tenemos
#Graficamos PCA
plot(pokemon.pca$x[, 1:2], main = "Pokemon")
#Hay patrones en los datos? difícil de ver a simple vista.

#Calculemos la distancia entre cada uno y veamos si podemos ordenarlos en un arbol
pokemon_escaleado <- as.data.frame(scale(pokemon, center = T, scale = T)) #Recuerden escalear!
?dist
distancia    <- dist(pokemon_escaleado, method = "euclidean")
?hclust
dendrograma  <- hclust(distancia, method = "complete")

#Pagina con buena data para hacer lindos dendrogramas
#http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning

plot(dendrograma, labels = nombres[dendrograma$order])

#Sacamos los labels
plot(dendrograma, hang = -1, labels = F)

#Como encontramos los clusters? Hay que cortar en algún lado!
abline(h = 6.5, col = "red")
clusters <- cutree(dendrograma, h = 6.5)
clusters
table(clusters)

nombres[clusters == 4] #Qué tipo de grupo es?
nombres[clusters == 1] #Y este?
#.
#.
#.
#.
#Veamos si hay más mega en el cluster 4 que lo que cabria esperar por azar. Usamos un test de hipotesis hipergeometrico, tambien llamado de sobrerepresentacion
cantidad_total_de_pokemon     <- 800
cantidad_total_de_mega        <- 49 #Los habia contado antes
cantidad_en_cluster_4         <- 59
cantidad_de_mega_en_cluster_4 <- 22 #Tambien ya los habia contado antes
#Se usa asi, solo a modo de ejemplo, no se preocupen
pvalue <- phyper(cantidad_de_mega_en_cluster_4-1, cantidad_total_de_mega, cantidad_total_de_pokemon - cantidad_total_de_mega, cantidad_en_cluster_4, lower.tail = F)
pvalue #Rechazamos la hipotesis de que podriamos haber obtenido esto por azar, claramente este cluster es de megas


#Estamos en distintas escalas, podemos iterar
pokemon_normales <- pokemon_escaleado[clusters == 1, ]
nombres_normales <- nombres[clusters == 1]
distancia    <- dist(pokemon_normales, method = "euclidean")
dendrograma  <- hclust(distancia, method = "complete")
plot(dendrograma, labels = F, hang = -1)

#Cortamos
abline(h = 3.8, col = "red")
clusters_normales <- cutree(dendrograma, h = 3.8)
table(clusters_normales)
#Vemos que hay
nombres_normales[clusters_normales == 6]
nombres_normales[clusters_normales == 1]

#Siempre lo mas interesante es ver que queda en los grupos y tratar de encontrarles un sentido biologico, fisico o lo que fuere

#Existen otros algoritmos de clusterizacion que miden otras cosas, como por ejemplo, dbscan. En general, cuanto mas complejos
#los algoritmos, mas parametros tienen y mas decisiones tenemos que tomar. Exploren!