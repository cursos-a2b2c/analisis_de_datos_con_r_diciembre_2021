#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------Estadistica con R-------------------------------------------#
#--------------------------------------------Día 3: Estadística descriptiva-------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#R nos propone un conjunto de datasets famosos que ya se encuentran disponibles sin tener que importarlos. Uno de estos es iris.
#Pero la primer pregunta: de qué trata iris? Bueno... manos a la obra!

#Veamos cuantos datos tiene nuestro dataset, representados por las filas de la tabla
nrow(iris)

#Veamos cuantos atributos tiene nuestro dataset, representados por las columnas de la tabla
ncol(iris)

#Lo mismo podriamos hacer preguntando las dimensiones de la tabla
dim(iris)

#Veamos qué tipo de atributos son
str(iris)
class(iris)

#Imprimamos las primeras 10 filas del dataset
head(iris, n = 10)

#Imprimamos las últimas 10 filas del dataset
tail(iris, n = 10)

#Imprimamos los nombres de las variables
colnames(iris)

#Entonces... de qué trata?
#-----------------------------------------------------------------------------------------------

#Ahora una exploración medio rápida.

mean(iris$Sepal.Length)
min(iris$Sepal.Length)
max(iris$Sepal.Length)
median(iris$Sepal.Length)
IQR(iris$Sepal.Length)

summary(iris) #Me permite realizar un resumen de todas las columnas del data.frame

#Cuales son las primeras conclusiones?

#Ahora otro ejemplito... para esto importamos el dataset datasaurius.

library("datasauRus")
mydsdz <- datasaurus_dozen[datasaurus_dozen$dataset %in% c("dino","star","away","slant_down"),] #Fijarse en el %in%

layout(matrix(1:4, ncol=2))
plot(mydsdz$x[mydsdz$dataset == "dino"],mydsdz$y[mydsdz$dataset == "dino"],col="green",main="Dino",xlab="",ylab="")
plot(mydsdz$x[mydsdz$dataset == "star"],mydsdz$y[mydsdz$dataset == "star"],col="violet",main="Star",xlab="",ylab="")
plot(mydsdz$x[mydsdz$dataset == "away"],mydsdz$y[mydsdz$dataset == "away"],col="black",main= "Away",xlab="",ylab="")
plot(mydsdz$x[mydsdz$dataset == "slant_down"],mydsdz$y[mydsdz$dataset == "slant_down"],col="blue",main="Slant_down",xlab="",ylab="")
layout(1)

#Entonces... me basta con la estadística descriptiva? 
#A plotear!
#Lo más importante es preguntarnos, qué queremos plotear.

#Primero exploremos las variables categóricas:

tabla_especies <-table(iris$Species)

# Y si queremos plotearlo?

barplot(tabla_especies)

#Ahora le damos un poco de color
barplot(tabla_especies,col = c("red","blue","green"))

#Otros gráficos que también están buenos 
pie(tabla_especies,main = "Grafico de tortas especies") #Le agregamos titulo con main=...

#Y ahora si tengo dos variables categoricas?

iris2 <- iris
iris2$Exterior <- sample(c(0,1),size = nrow(iris2),replace = T)

tabla_especiesxExterior <- table(iris2$Exterior,iris2$Species)
print(tabla_especiesxExterior)
barplot(tabla_especiesxExterior,col=c("red","blue"))

tabla_especiesxExterior <- table(iris2$Species,iris2$Exterior)
print(tabla_especiesxExterior)

barplot(tabla_especiesxExterior,col=c("red","blue","green"),main = "Exterior x Especie")
barplot(tabla_especiesxExterior,col=c("red","blue","green"),main = "Exterior x Especie",beside = T)

#Variables numéricas ----

hist(iris$Sepal.Length,main="Longitud del sepalo",col="blue")

plot(density(iris$Sepal.Length),main="Longitud del sepalo",col="blue")
#Si quiero tener dos plots uno al lado del otro

layout(matrix(1:2,ncol=2))
hist(iris$Sepal.Length,main="histograma de frecuecias",xlab="Longitud sepalo",ylab="Frecuencia absoluta")
hist(iris$Sepal.Length,freq=F,main="histograma de densidad",xlab="Longitud sepalo",ylab="Densidad")
layout(1)
#Si quiero representar estos datos, qué elijo plotear? Longitud y ancho de sepalo, de petalo... puedo todas juntas?

#Boxplot ----

boxplot(iris$Sepal.Length)

#Ahora quiero separarlo por categorias
boxplot(iris$Sepal.Length~iris$Species, xlab="especies",ylab="Longitud sepalo",main="longitud del sepalo",col=c("red","blue","green"))

#Y si le paso dos variables numericas?

boxplot(iris$Sepal.Length,iris$Sepal.Width,names=c("longitud","ancho"))

### Scatter plots -----

plot(iris$Sepal.Length,iris$Petal.Length)
abline(a=-2,b=1.1,col="red",lty=2)

plot(iris$Sepal.Length[1:50],iris$Petal.Length[1:50],xlim = c(4,8),ylim=c(0,7),xlab="Longitud sepalo",ylab="Longitud petalo")
points(iris$Sepal.Length[51:100],iris$Petal.Length[51:100],col="red")
points(iris$Sepal.Length[101:150],iris$Petal.Length[101:150],col="green")

#Coloreo por especies 
plot(iris$Sepal.Length,iris$Petal.Length,col=iris$Species)

#Y si quiero hacerlo directo desde el dataset?
plot(Petal.Length ~Sepal.Length,data=iris,col=Species)

#Y si quiero ver todos contra todos?

pairs(iris,col=iris$Species)

plot(iris$Sepal.Width,type="l",xlim=c(0,150),ylim=c(2,8),col="blue")
lines(iris$Sepal.Length,type="l",col="red")
legend("topleft",legend=c("Ancho","Longitud"),col=c("blue","red"))



### PCA ----

#Vamos a cargar un nuevo tipo de archivo conocido como .Rdata

load("exp_mtx.Rdata") 


res.pca <- prcomp(exp_mtx)

rownames(exp_mtx) #Vamos a ver como renombrar los experimentos
experimento <- c("frio","frio","control","control","frio","frio","control","control","frio","frio","control","control","frio","frio","control","control","frio","control","control") 
pc1 <- res.pca$x[,1]
pc2 <- res.pca$x[,2]
#Armo el data.frame
df <-data.frame(PC1=pc1,PC2=pc2,exp = experimento)
df$exp <- as.factor(df$exp)

plot(x=df$PC1,y=df$PC2,col=df$exp,xlab="Comp1",ylab="Comp2") #Qué se puede observar?
legend("topleft",legend=c("Frio","Control"),col=c("black","red"),pch = 0.8)
abline(h=0)
#Que podemos concluir de esto?
