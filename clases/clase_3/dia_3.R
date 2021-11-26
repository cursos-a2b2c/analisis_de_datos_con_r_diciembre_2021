#################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biologíca Computacional
#                 Fundación Instituto Leloir
#                        Marzo 2021
#                Estadística descriptiva y AED
#################################################################

#Gran parte de los datos que vamos a utilizar vienen en formato de tabla.
#R tiene muchas funcionalidades para leer distintos formatos de tabla.
#Por ejemplo, puede leer comma separated values (csv), tab separated values (tsv) o usar cualquier separador.
#Las funciones de lectura de archivos suelen llamarse read.tipo_de_tabla
#Leamos un csv

#Primero le decimos a R en qué directorio queremos que trabaje
getwd()
setwd("~/trabajo/cursos/analisis_de_datos_con_r_octubre_2020/clases/clase_3")
setwd("c://users//andy//") 

#setwd("trabajo/cursos/analisis_de_datos_con_r_octubre_2020/clases/clase_3")
#Leemos un archivo csv. Usamos header = T para que tome la primera fila del archivo como los nombres de los atributos
#y stringsAsFactors = F para que no modifique los atributos tipo character o string
casos_covid <- read.csv(file = "datasets/cases-covid-19.csv", stringsAsFactors = F, header = T)
antropometria <- read.csv("~/trabajo/cursos/analisis_de_datos_con_r_octubre_2020/clases/clase_3/datasets/antropometria.csv")
View(casos_covid)
#casos_covid <- cases.covid.19

#Veamos cuantos datos tiene nuestro dataset, representados por las filas de la tabla
nrow(casos_covid)

#Veamos cuantos atributos tiene nuestro dataset, representados por las columnas de la tabla
ncol(casos_covid)

#Veamos qué tipo de atributos son
str(casos_covid)
class(casos_covid)

casos_covid
#Imprimamos las primeras 10 filas del dataset
head(casos_covid, n = 10)

#Imprimamos las últimas 10 filas del dataset
tail(casos_covid, n = 10)

#Imprimamos los nombres de las variables
colnames(casos_covid)

#Podemos abrirlo tipo excel
View(casos_covid)

#¿Qué representa cada fila del dataset?

#----------------------------------------------------------------------
#Leamos un archivo tabulado usando otra función de R, read.delim. Esta función permite leer
#un archivo separado por cualquier delimitador, coma, tab, espacios, etc.
casos_covid_secuenciados <- read.delim("datasets/casos_covid_secuenciados.txt", sep = "\t", 
                                       header = T, stringsAsFactors = F)

#Entendamos el dataset
#Veamos cuantos datos tiene nuestro dataset, representados por las filas de la tabla
nrow(casos_covid_secuenciados)

#Veamos cuantos atributos tiene nuestro dataset, representados por las columnas de la tabla
ncol(casos_covid_secuenciados)

#Imprimamos las primeras 10 filas del dataset
head(casos_covid_secuenciados, n = 10)

#Imprimamos las últimas 10 filas del dataset
tail(casos_covid_secuenciados, n = 10)

#Imprimamos los nombres de las variables
colnames(casos_covid_secuenciados)

#Podemos abrirlo tipo excel
View(casos_covid_secuenciados)

#Veamos qué tipo de atributos son
str(casos_covid_secuenciados)

#Sequence.Length, a, c, g, t son enteros (int) y an, cn, gn y tn son valores "reales" (num), mientras que el resto son characters
#Veamos un resumen de Sequence.Length
casos_covid_secuenciados$Virus.Strain.Name
casos_covid_secuenciados[, "Virus.Strain.Name"]
casos_covid_secuenciados[, c(1, 5)]
head(casos_covid_secuenciados[, c("Virus.Strain.Name", "Sequence.Length")])
casos_covid_secuenciados[c(1, 10), c("Virus.Strain.Name", "Sequence.Length")]
class(casos_covid_secuenciados)
dim(casos_covid_secuenciados)
colnames(casos_covid_secuenciados)
casos_covid_secuenciados$Virus.Strain.Name

class(casos_covid_secuenciados$Virus.Strain.Name)
length(casos_covid_secuenciados$Virus.Strain.Name)
nrow(casos_covid_secuenciados)
dim(casos_covid_secuenciados)
length(casos_covid_secuenciados)
nrow(casos_covid_secuenciados$Virus.Strain.Name)

#nrow(casos_covid_secuenciados)
nas <- is.na(casos_covid_secuenciados$Originating.Lab)
table(nas)

casos_covid_secuenciados_sin_na <- casos_covid_secuenciados[which(nas == FALSE), ]
casos_covid_secuenciados_sin_na <- casos_covid_secuenciados[nas == FALSE, ]
casos_covid_secuenciados_sin_na <- casos_covid_secuenciados[!nas, ]

nas <- is.na(casos_covid_secuenciados)

casos_covid_secuenciados$Sequence.Length

summary(casos_covid_secuenciados$Sequence.Length)
summary(casos_covid_secuenciados[, c("a", "c", "g", "t")])

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#64   29780   29852   26310   29882   29981
#¿Les suenan los valores que devuelve summary?
#¿Qué les parece, tiene mucha dispersión o poca dispersión esta variable? ¿Las medidas de centralidad son parecidas? ¿Cómo se verá la distribución?

#Calculemos las distintas medidas de centralidad
media <- mean(casos_covid_secuenciados$Sequence.Length)
media

#hist(casos_covid_secuenciados$Sequence.Length)

mediana <- median(casos_covid_secuenciados$Sequence.Length)
mediana

#Dejamos la moda para después
#Calculemos las distintas medidas de dispersión
rango <- max(casos_covid_secuenciados$Sequence.Length)-min(casos_covid_secuenciados$Sequence.Length)
rango

varianza <- var(casos_covid_secuenciados$Sequence.Length)
varianza

desvio <- sd(casos_covid_secuenciados$Sequence.Length)
desvio
media

desvio*desvio

iqr <- IQR(casos_covid_secuenciados$Sequence.Length)
iqr
mediana
#Hay mucha diferencia entre el rango, el desvío y el IQR. Esto nos dice que seguramente hay valores muy extremos que vamos a tener que analizar por separado.

#Grafiquemos un histograma con la distribución
hist(casos_covid_secuenciados$Sequence.Length, xlab="Longitud de la secuencia", main="Histograma de casos", breaks=20)

#Efectivamente la mayor parte de los valores está cerca de los 30000 pero tenemos un grupo
#de valores que están cerca del 0. Son dos poblaciones distintas, vamos a tener que analizarlas
#por separado!!
#Elegimos los valores por arriba de 29000
class(casos_covid_secuenciados$Sequence.Length)
casos_covid_secuenciados$Sequence.Length[9]
ids_secuencias_largas <- which(casos_covid_secuenciados$Sequence.Length > 29000)
secuencias_largas     <- casos_covid_secuenciados$Sequence.Length[ids_secuencias_largas]
casos_covid_secuenciados[9,2]
casos_covid_secuenciados$Sequence.Length[ids_secuencias_largas]

#Veamos que pasa nuevamente con el resumen y la dispersión
summary(secuencias_largas)
sd(secuencias_largas)
IQR(secuencias_largas)

#Ahora todos los valores son más parecidos
hist(secuencias_largas)

#Podemos graficar un boxplot y comparar la información de ambos gráficos
tiff(filename = "../../../analisis_de_datos_con_r_marzo_2021/clases/clase_3/boxplot.tiff", res = 300, width = 1400, height = 1400)
boxplot(secuencias_largas)
dev.off()

#R permite graficar tipo mosaico, así podemos graficar los dos juntos
#La función layout recibe una matriz y grafica un mosaico en función de la cantidad de filas
#y columnas que tiene esa matriz
#dev.new()
mosaico_layout <- matrix(1:4, ncol=2, nrow = 2, byrow = F)
layout(mosaico_layout)
hist(secuencias_largas)
boxplot(secuencias_largas, horizontal = T) #Graficamos el boxplot horizontal para comparar
x <- seq(-pi, pi, 0.1)
y <- sin(x)
plot(x, y)
hist(secuencias_largas)
boxplot(secuencias_largas, horizontal = T) #Graficamos el boxplot horizontal para comparar
hist(secuencias_largas)
boxplot(secuencias_largas, horizontal = T) #Graficamos el boxplot horizontal para comparar

#dev.off()

#Volvemos al layout convencional
layout(1)
plot(1, 1)

#Volvamos a la moda. La moda es el valor que más veces aparece. Para calcularla tenemos que contar 
#cuantas veces aparece cada valor y quedarnos con el que más veces aparezca.
#Usamos table para contar y which.max para que nos diga cual es el valor que más aparece
cuantas_veces_aparece_cada_valor <- table(secuencias_largas)
cuantas_veces_aparece_cada_valor
class(cuantas_veces_aparece_cada_valor)
nombresDeLaTabla <- names(cuantas_veces_aparece_cada_valor)

max(cuantas_veces_aparece_cada_valor)
cuantas_veces_aparece_cada_valor[which.max(cuantas_veces_aparece_cada_valor)]
median(secuencias_largas)
mean(secuencias_largas)

#------------------------------------------------------------------------------------
#Ejercicios: 
#casos_covid_secuenciados$a
#1-Caractericen la variable "a" y en caso de detectar valores extremos
#elegir un criterio para removerlos.
#2-Encuentren cuantas secuencias hay de cada país.
#------------------------------------------------------------------------------------
hist(casos_covid_secuenciados$a)
boxplot(casos_covid_secuenciados$a, horizontal = T)
a_largos <- casos_covid_secuenciados$a[casos_covid_secuenciados$a > 4000]
iqr <- IQR(casos_covid_secuenciados$a)
primer_cuartil <- quantile(casos_covid_secuenciados$a, probs = 0.25)
valor_de_corte <- primer_cuartil - 1.5*iqr
a_largos <- casos_covid_secuenciados$a[casos_covid_secuenciados$a > valor_de_corte]
hist(a_largos)
boxplot(a_largos)

#Podemos ver cuantas secuencias hay por ubicación
table(casos_covid_secuenciados$Location)


#Si queremos ver por país, vamos a tener que procesar las ubicaciones, quedándonos únicamente con la parte del país
#Usamos strsplit para partir la cadena, lapply para quedarnos con la primera parte y unlist para obtener finalmente
#un vector
head(casos_covid_secuenciados$Location, n = 10)
paises <- strsplit(casos_covid_secuenciados$Location, " / ", fixed = F)
head(paises)
class(paises)

#Con R podemos aplicar una misma función a cada elemento de la lista 
primerItem <- function(v){
  return(v[1])
}

hist()

#Veamos como funciona primerItem
vectorDePrueba <- c("Curso", "De", "R")
primerItem(vectorDePrueba)

v <- paises[[1]]
primerItem(v)
v <- paises[[2]]
primerItem(v)
v <- paises[[3]]
primerItem(v)
v <- paises[[550]]
primerItem(v)

#Apliquemos primerItem a cada elemento de la lista para obtener los países
paises <- lapply(paises, primerItem)
paises
class(paises)

#Sigue siendo una lista, podemos "desenlistarla"
paises <- unlist(paises)
table(paises)

#Podemos agregarlo como columna del dataframe para usarlo si lo necesitamos después
casos_covid_secuenciados$pais <- paises

#Encontremos la cantidad de secuencias por país
table(casos_covid_secuenciados$pais)

#En lugar de caracterizar cada variable por separado, nos puede interesar comparar
#distintas variables entre si. Por ejemplo, podemos querer comparar su distribución y
#sus medias y varianzas o encontrar relaciones entre los mismos.
#Veamos qué pasa con a, c, g y t. Pero primero, eliminemos todas las muestras
#de secuencias cortas. Podemos usar varios criterios para esto, por ejemplo, el de los bigotes del 
#boxplot. Nos quedamos con las secuencias que sean mayores que primer cuartil - 1.5*IQR
primer_cuartil <- quantile(casos_covid_secuenciados$Sequence.Length, probs = 0.25)
iqr            <- IQR(casos_covid_secuenciados$Sequence.Length)
ids_secuencias_largas <- which(casos_covid_secuenciados$Sequence.Length > (primer_cuartil-1.5*iqr))
acgt <- casos_covid_secuenciados[ids_secuencias_largas, c("a", "c", "g", "t")]
summary(acgt)

#podemos graficar un boxplot por cada uno rapidamente
dim(acgt)
boxplot(acgt)

#Vemos que las 4 bases tienen distribuciones diferentes.
#¿Existirá relación entre ellas? Veamos que pasa con c y t. Podemos agregar titulo, nombrar los ejes y jugar con los límites.
plot(acgt$c, acgt$t, main = "c vs. t", xlab = "c (bases)", ylab="t (bases)", xlim=c(5400, 5500), ylim=c(9500, 9600))
plot(acgt$c, acgt$t, xlim=c(5400, 5500), ylim=c(9500, 9600), main = "c vs. t", xlab = "c (#bases)", ylab = "t (#bases)")

#Pareciera haber una relación lineal creciente entre ambas
#Veamos todas contra todas
pairs(acgt)

#Todas tienen una relación lineal creciente.
#¿Descubrimos nueva biología o qué puede estar pasando?
#Agreguemos la longitud de la secuencia
plot(casos_covid_secuenciados[ids_secuencias_largas, c("Sequence.Length", "a")], main = "Longitud de secuencia vs. a", xlab = "Longitud de secuencia", ylab="a")
#Parece haber una relación lineal creciente también, pero la escala no ayuda
#Cortemos el gráfico
plot(casos_covid_secuenciados[ids_secuencias_largas, c("Sequence.Length", "a")], ylim=c(8500, 9000))

#¿Todas se comportan igual?
pairs(casos_covid_secuenciados[ids_secuencias_largas, c("Sequence.Length", "a", "c", "g", "t")])

#Veamos como son los perfiles de secuencias. 
#Usamos matplot para graficar las coordenadas paralelas. Tenemos que transponer
#el dataframe para que tome las columnas como variables, y le decimos que el type
#sea "l" para que grafique segmentos
matplot(t(casos_covid_secuenciados[ids_secuencias_largas, c("an", "cn", "gn", "tn")]), type="l", col = "black")
summary(casos_covid_secuenciados[ids_secuencias_largas, c("Sequence.Length", "an", "cn", "gn", "tn")])
x <- seq(-pi, pi, 0.1)
y <- sin(x)
plot(x, y, pch = 15)
legend("bottomright", legend = c("Seno x"), pch = 15)

#------------------------------------------------------------------------------------
#Ejercicios: 
#casos_covid_secuenciados
#1-Caractericen las variable "an", "cn", "gn" y "tn".
#2-Encuentren las secuencias de alta calidad y completas. ¿Cuantas hay de cada una? 
#Sugerencia, table puede recibir dos variables y realizar la tabla cruzada:
#table(var1, var2)
#------------------------------------------------------------------------------------




table(casos_covid_secuenciados$Sequence.Quality)
table(casos_covid_secuenciados$Nuc.Completeness)
table(casos_covid_secuenciados$Sequence.Quality, casos_covid_secuenciados$Nuc.Completeness)
