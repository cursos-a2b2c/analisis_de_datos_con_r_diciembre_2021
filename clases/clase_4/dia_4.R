#################################################################
#              Curso de análisis de datos con R
#                      Test de hipótesis
#################################################################
#Las gallinas son mas valiosas para las granjas que los gallos porque
#pueden poner huevos. Un laboratorio quiere probar tres drogas que supuestamente aumentan la probabilidad
#de que una gallina nazca gallina en lugar de gallo
#Aplica cada tipo de droga en tres grupos de 48 gallinas (una droga distinta por grupo) y obtiene los siguientes resultados:
#Droga 1: 25 H 23 M
#Droga 2: 47 H 1 M
#Droga 3: 31 H 17 M
#¿Qué dirían respecto a cada droga?
#------------------------------------
#¿cuál es la probabilidad de obtener el ultimo resultado si la droga no tuviera efecto, simplemente por azar?
#Para resolver esto, podemos empezar pensando que pasa con un solo huevo. Si la droga no tiene efecto, el huevo va a dar H o M con 50% de probabilidad cada uno.
#Para dos huevos, tenemos 4 opciones: H H, H M, M H, M M. Entonces, la probabilidad de dos H es 25%, de dos M es 25% y de una H y un M es 50%.
#Entonces, hacemos esto pero para 48 huevos.
#Si pensamos que lo que le pasa a cada gallina es independiente de lo que le pasa a la otra, simplemente tenemos que preguntarnos de cuantas
#formas posibles podemos obtener 17 M de 48 totales.
#Esta distribucion se conoce como binomial, y responde al resultado de un conjunto de preguntas dicotomicas independientes todas con la misma probabilidad. 

#R tiene una funcion que nos devuelve esta probabilidad
dbinom(x = 1, size = 1, prob = 0.5) #¿Que esperamos que de?
dbinom(x = 0, size = 1, prob = 0.5) #¿Que esperamos que de?

cuantos_machos         <- c(0, 1, 2)
probabilidad_de_machos <- dbinom(x = cuantos_machos, size = 2, prob = 0.5) #¿Que esperamos que de?
probabilidad_de_machos
plot(cuantos_machos, probabilidad_de_machos, type='h', ylim = c(0, 1))

#Veamos ahora para nuestra muestra particular
cuantos_machos         <- 0:48
probabilidad_de_machos <- dbinom(cuantos_machos, 48, prob = 0.5)
plot(cuantos_machos, probabilidad_de_machos, type='h', col = c(rep("red", 17),
                                                               rep("black", 31)))

#Si aceptamos 17 como evidencia, también hubieramos aceptado 16, 15, 14...0, entonces,
#calculemos la probabilidad de obtener 17 o menos "a mano".
sum(dbinom(0:17, 48, prob = 0.5))

#esta probabilidad de observar lo que observamos, suponiendo que no hay efecto (hipotesis nula) 
#es lo que se conoce como pvalue. En este caso, la hipotesis alternativa, que es la que nos interesa, 
#es que la droga hace que nazcan menos gallinas M que H.
#Como es "bastante" raro haber observado 17 M o menos, descartamos que no haya efecto (rechazamos la hipotesis nula).
#En general, en biología, se rechaza la hipotesis nula cuando el pvalue es menor a 0.05.

#Otra hipotesis que podriamos haber testeado es que haya mas H que las esperadas por azar
cuantas_hembras         <- 0:48
probabilidad_de_hembras <- dbinom(cuantas_hembras, 48, prob = 0.5)
plot(cuantas_hembras, probabilidad_de_hembras, type='h')
plot(cuantos_machos, probabilidad_de_machos, type='h', col = c(rep("black", 31),
                                                               rep("red", 17)))

#Si aceptamos 31 como evidencia, también hubieramos aceptado 32, 33, 34...48, entonces,
#calculemos la probabilidad de obtener 31 o mas "a mano".
sum(dbinom(31:48, 48, prob = 0.5)) 

#Otra hipotesis alternativa podria ser que la droga cambia la probabilidad de 0.5 de obtener F o M, pero sin
#indicar cual tiene mas probabilidad y cual menos (sin indicar la direccion del cambio).
#Entonces ademas de probar 17 o menos, deberiamos probar 31 o mas.
sum(dbinom(0:17, 48, prob = 0.5)) + sum(dbinom(31:48, 48, prob = 0.5))

#esto se conoce como test a dos colas, mientras que en el caso anterior, se conoce como test a una cola

#Veamos como hacer este test usando funciones de R en lugar de hacerlo a mano.
binom.test(17, 48, p = 0.5, "less") #tenemos menos chances que 50%

binom.test(17, 48, p = 0.5, "two.sided") #tenemos distinta chances que 50%

binom.test(31, 48, p = 0.5, alternative = "greater") #tenemos mas chances que 50%
#Veamos las salidas de estos tests.
#¿Que indica el pvalue?
#¿Que indica el intervalo de confianza?
#¿Que indica sample estimates: probability of success?


#Este test se llama "test exacto de bondad de ajuste" o "exact test of goodness-of-fit" y se usa cuando tenemos
#una variable nominal con dos niveles (por ejemplo F y M), pocas observaciones y un modelo teorico de lo que 
#esperamos que de.
#-------------------------------------------
#Si no podemos rechazar la hipotesis nula, eso significa que no hay efecto? o no necesariamente?
#Probemos con una muestra mas grande, de 96
#Droga 3: 62 F 34 M
binom.test(34, 96, p = 0.5, "two.sided") #a dos colas
#¿Qué podemos decir del pvalue y del intervalo de confianza comparado con la muestra de 48?


#Veamos otros tests que se suelen utilizar y como hacerlos.

#Mucho se habló de la publicación (o no publicación) de los resultados de la vacuna sputnik v.
#Finalmente, los resultados se publicaron el 2 de febrero en The Lancet, en
#Safety and efficacy of an rAd26 and rAd5 vector-based heterologous prime-boost COVID-19 vaccine: an interim analysis 
#of a randomised controlled phase 3 trial in Russia. Logunov et al.
#https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)00234-8/fulltext
#En el texto nos cuentan que hicieron un ensayo doble ciego con 21977 adultos, de los cuales 16501 fueron vacunados con la sputnik y 5476 recibieron placebo.
#Luego de 21 días, de 14964 participantes vacunados, 16 contrajeron COVID-19, mientras que de 4902 del grupo placebo, 62 lo contrajeron.  
#Carguemos la tabla de pacientes

#H0: los dos grupos son iguales, no hay relacion entre las variables
#H1: los dos grupos no son iguales, hay relacion entre las variables

pacientes <- read.table(file = "sputnik.txt", sep = "\t", header = T)
head(pacientes)
View(pacientes)

#Contemos cuantos pacientes fueron tratados
table(pacientes$tratado)

#Contemos cuantos pacientes se infectaron
table(pacientes$infectado)

#Veamos como es la relacion entre ambas, llamada tabla de contingencia:
table(pacientes$tratado, pacientes$infectado)

#Podemos utilizar un test de chi cuadrado para ver si son independientes la infección y la vacuna. El test
#de chi cuadrado requiere que todos los valores esperados en la tabla de contingencia sean mayores a 5.
#R nos va a avisar si no se cumple la condición. En ese caso, podemos usar el test exacto de fisher fisher.test()
#H0: Las dos variables son independientes
#H1: Las dos variables no son independientes

chisq.test(pacientes$tratado, pacientes$infectado, correct=FALSE) #Si da < 0.05 rechazamos la hipotesis nula
#Descartamos que sea independiente la infección de la vacuna.


#Se quiere probar una droga que supuestamente incrementa el peso corporal en mamiferos.
#Para ello se inyecto a 50 ratoncitos mus musculus la droga y se los peso luego de dos semanas.
#El peso promedio de un ratoncito es de 19 g.
#Cargamos los datos
ratoncitos <- read.csv("ratoncitos1.csv")
View(ratoncitos)
#Graficamos histograma, boxplot y calculamos medidas
hist(ratoncitos$weight)
boxplot(ratoncitos$weight)
mean(ratoncitos$weight)
sd(ratoncitos$weight)

#La media es diferente a 19 g, pero sera significativamente diferente?
#Usamos el t-test. Este test nos permite comparar la media de nuestra muestra con la media esperada.
#Para usarlo, tenemos que tener una muestra grande (> 30) o tener una muestra con distribucion normal.
#Tenemos una muestra grande, lo usamos.
#H0: La media muestral de los pesos es 19 g
#H1: La media muestral de los pesos es mayor a 19 g
t.test(ratoncitos$weight, alternative = "greater", mu = 19)

#Queremos estudiar el efecto de dos tratamientos en el crecimiento de una planta. Para ello contamos con plantas a las que se las trató con un placebo, plantas
#tratadas con la droga1 y plantas tratadas con la droga 2. ¿Cómo podemos saber si el tratamiento 1 o el tratamiento 2 fue efectivo?
#Usemos un t Test de dos muestras independientes para comparar el control con tratamiento 1 y el control con tratamiento 2. Además de ser normales e independientes,
#la varianza de los dos grupos tiene que ser la misma.

#Cargamos los datos
plantas <- datasets::PlantGrowth
View(plantas)
#Veamos como se distribuyen las plantas dentro de cada grupo
weight ~ group
boxplot(weight ~ group, data = plantas)

#Para testear que las dos varianzas sean iguales (homogeneidad de varianzas) podemos usar el test de bartlett.
#H0: Los datos tienen igual varianza
#H1: Los datos no tienen igual varianza
bartlett.test(list(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt1"]))
bartlett.test(list(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt2"]))

#Para testear que cada variable sea normal
#H0: Los datos son normales
#H1: Los datos no son normales
shapiro.test(plantas$weight[plantas$group == "ctrl"])
shapiro.test(plantas$weight[plantas$group == "trt1"])
shapiro.test(plantas$weight[plantas$group == "trt2"])

shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))

#Ahora si, podemos usar el t test de dos muestras
#H0: las medias de los dos grupos son iguales
#H1: las medias de los dos grupos son distintas

t.test(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt1"], var.equal = TRUE)
t.test(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt2"], var.equal = TRUE)
t.test(plantas$weight[plantas$group == "trt1"], plantas$weight[plantas$group == "trt2"], var.equal = TRUE)

#¿Qué pasa si nuestros datos no cumplen homogeneidad de varianza?
#Veamos los datos de iris
View(iris)

#Comparamos setosa con versicolor en el largo del sépalo
shapiro.test(iris$Sepal.Length[iris$Species == "setosa"])
shapiro.test(iris$Sepal.Length[iris$Species == "versicolor"])

#Veamos si cumplen homogeneidad de varianza
#H0: Los datos tienen igual varianza
#H1: Los datos no tienen igual varianza
bartlett.test(list(iris$Sepal.Length[iris$Species == "setosa"], iris$Sepal.Length[iris$Species == "versicolor"]))

#Ups! Qué podemos hacer? Usar el test de welch, que testea lo mismo que un t-test pero soporta varianzas distintas
t.test(iris$Sepal.Length[iris$Species == "setosa"], iris$Sepal.Length[iris$Species == "versicolor"], var.equal = F)

#Veamos un ejemplo con muchos valores de categorias.
#Cargamos el archivo de tamanios (Aam) de mejillones en distintos lugares (Location)
mejillones <- read.csv(file = "mejillones.csv")
View(mejillones)
table(mejillones$Location)

#Usamos anova para comparar la media de los distintos grupos. Necesita normalidad e igualdad de varianzas.
boxplot(Aam ~ Location, mejillones)
shapiro.test(mejillones$Aam[mejillones$Location == "Magadan"])
shapiro.test(mejillones$Aam[mejillones$Location == "Newport"])
shapiro.test(mejillones$Aam[mejillones$Location == "Petersburg"])
shapiro.test(mejillones$Aam[mejillones$Location == "Tillamook"])
shapiro.test(mejillones$Aam[mejillones$Location == "Tvarminne"])

#Descartamos Magadan y Tvarminne porque no son normales
mejillones <- mejillones[mejillones$Location != "Magadan", ]
mejillones <- mejillones[mejillones$Location != "Tvarminne", ]
table(mejillones$Location)

#Veamos las varianzas de esas localidades
var(mejillones$Aam[mejillones$Location == "Newport"])
var(mejillones$Aam[mejillones$Location == "Petersburg"])
var(mejillones$Aam[mejillones$Location == "Tillamook"])

#Son practicamente iguales, podemos usar anova
aov(Aam ~ Location, mejillones)
summary(aov(Aam ~ Location, mejillones))


#Tukey para ver cuales son los diferentes

#Hay muchisimos tests para muchisimos casos distintos, les recomendamos fuertemente que lean
#McDonald J. (2014). HANDBOOK OF BIOLOGICAL STATISTICS, SPARKY HOUSE PUBLISHING. (http://www.biostathandbook.com/HandbookBioStatThird.pdf)
#Esta online y es gratuito, junto con como realizar los distintos tests en r
#Mangiafico S. (2015), AN R COMPANION FOR THE HANDBOOK OF BIOLOGICAL STATISTICS, New Brunswick, Rutgers University (http://rcompanion.org/documents/RCompanionBioStatistics.pdf)


