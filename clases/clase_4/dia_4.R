#################################################################
#              Curso de análisis de datos con R
#                      Test de hipótesis
#################################################################
#Las gallinas femeninas son mas valiosas para las granjas que las masculinas porque
#pueden poner huevos. Un laboratorio quiere probar tres drogas que supuestamente aumentan la probabilidad
#de que una gallina nazca femenina en lugar de masculina.
#Aplica cada tipo de droga en tres grupos de 48 gallinas (una droga por grupo) y obtiene los siguientes resultados:
#Droga 1: 25 F 23 M
#Droga 2: 47 F 1 M
#Droga 3: 31 F 17 M
#¿Qué dirían respecto a cada droga?

#Si suponemos que sin la droga, la probabilidad de que sea M o F es 50%, ¿cuál es la probabilidad de obtener
#cada uno de esos resultados por azar? 
#Esta distribucion se conoce como binomial, y responde al resultado de un conjunto de preguntas dicotomicas 
#independientes todas con la misma probabilidad. 
#En este caso, la pregunta es si salio de un huevo M o F, y si vemos todos los huevos, sumamos todos las veces 
#que sale M, obtenemos la binomial.

#R tiene una funcion que nos devuelve esta probabilidad
dbinom(x = 1, size = 1, prob = 0.5)
dbinom(x = 2, size = 2, prob = 0.5)
dbinom(x = 1, size = 10, prob = 0.5)
dbinom(x = 1, size = 10, prob = 0.1)

cuantos_masculinos         <- 0:48
probabilidad_de_masculinos <- dbinom(cuantos_masculinos, 48, prob = 0.5)
plot(cuantos_masculinos, probabilidad_de_masculinos, type='h')

#Si aceptamos 17 como evidencia, también hubieramos aceptado 16, 15, 14...0, entonces,
#calculemos la probabilidad de obtener 17 o menos "a mano".
sum(dbinom(0:17, 48, prob = 0.5))

#esta probabilidad de observar lo que observamos, suponiendo que no hay efecto (hipotesis nula) 
#es lo que se conoce como pvalue. En este caso, la hipotesis alternativa, que es la que nos interesa, 
#es que la droga hace que nazcan mas gallinas F que M.
#Como es "bastante" raro haber observado 17 M o menos, descartamos que no haya efecto (rechazamos la hipotesis nula).
#En general, en biología, se rechaza la hipotesis nula cuando el pvalue es menor a 0.05.

#Otra hipotesis alternativa podria ser que la droga cambia la probabilidad de 0.5 de obtener F o M, pero sin
#indicar cual tiene mas probabilidad y cual menos (sin indicar la direccion del cambio).
#Entonces ademas de probar 17 o menos, deberiamos probar 31 o mas.
sum(dbinom(0:17, 48, prob = 0.5)) + sum(dbinom(31:48, 48, prob = 0.5))

#esto se conoce como test a dos colas, mientras que en el caso anterior, se conoce como test a una cola

#Veamos como hacer este test usando funciones de R en lugar de hacerlo a mano.
binom.test(17, 48, p = 0.5, "less") #a una cola

binom.test(17, 48, p = 0.5, "two.sided") #a dos colas

#Veamos las salidas de estos tests.
#Este test se llama "test exacto de bondad de ajuste" o "exact test of goodness-of-fit" y se usa cuando tenemos
#una variable nominal con dos niveles (por ejemplo F y M), pocas observaciones y un modelo teorico de lo que 
#esperamos que de.

#Veamos otros tests que se suelen utilizar y como hacerlos.


#Veamos que estos datos cumplen normalidad. Usamos el test de shapiro-wilk que testea justamente eso

#H0: Los datos son normales
#H1: Los datos no son normales
shapiro.test(alturasHolanda)

#Cumplen normalidad, podemos usar t Test. Pero antes, elijamos un nivel de significancia (usualmente 0.05)
#H0: alturaMedia = 175 
#H1: alturaMedia != 175
?t.test
t.test(alturasHolanda, mu = 175)
mean(alturasHolanda)

#Algunas personas interpretan el pvalue como una medida del efecto observado. ¿Será correcta esta interpretación? Veamos
#Simulemos un efecto, por ejemplo, el de una hormona de crecimiento aplicada a un hongo.
#Simulemos la población no tratada y la tratada, y supongamos que hubo un efecto en la tratada
set.seed(123456)
no_tratada <- rnorm(10, mean = 10, sd = 1)
tratada    <- rnorm(10, mean = 11, sd = 1)
shapiro.test(tratada)
shapiro.test(no_tratada)
t.test(no_tratada, tratada)
#H0 las medias son iguales
#H1 las medias son distintas
#¿Qué pasó? ¿Por qué? ¿recuerdan el nombre de este tipo de errores?

#Aumentemos el tamaño de la muestra
set.seed(123456)
no_tratada <- rnorm(15, mean = 10, sd = 1)
tratada    <- rnorm(15, mean = 11, sd = 1)
shapiro.test(tratada)
shapiro.test(no_tratada)
t.test(no_tratada, tratada)

#y ahora, ¿Qué pasó? ¿Por qué? ¿Cambió el efecto acaso?

#Aumentemos aún más el tamaño de la muestra
set.seed(123456)
no_tratada <- rnorm(100, mean = 10, sd = 1)
tratada    <- rnorm(100, mean = 11, sd = 1)
shapiro.test(tratada)
shapiro.test(no_tratada)
t.test(no_tratada, tratada)

#Entonces, ojo, un pvalue más chico no nos dice que tenemos un efecto más grande! 
#Cambiando nuestro experimento podemos manipular el pvalue, manteniendo el mismo tamaño de efecto
#Veamos que pasa si el efecto fuera más chico
set.seed(123456)
no_tratada <- rnorm(100, mean = 10, sd = 1)
tratada    <- rnorm(100, mean = 10.1, sd = 1)
shapiro.test(tratada)
shapiro.test(no_tratada)
t.test(no_tratada, tratada)

#¿Qué pasó?
#Mejoremos el experimento
set.seed(123456)
no_tratada <- rnorm(1000, mean = 10, sd = 1)
tratada    <- rnorm(1000, mean = 10.1, sd = 1)
shapiro.test(tratada)
shapiro.test(no_tratada)
t.test(no_tratada, tratada)

set.seed(123456)
no_tratada <- rnorm(10000, mean = 10, sd = 1)
tratada    <- rnorm(10000, mean = 10.1, sd = 1)
shapiro.test(tratada)
shapiro.test(no_tratada)
t.test(no_tratada, tratada)

#¿Y si no hubiera efecto?
set.seed(123456)
no_tratada <- rnorm(100000, mean = 10, sd = 1)
tratada    <- rnorm(100000, mean = 10, sd = 1)
t.test(no_tratada, tratada)

#Bueno, por suerte no era un artefacto del experimento. Pero con una significancia de 0.05, qué pasa si repetimos el experimento 100 veces?
set.seed(123456)
pvalues <- c()
for(i in 1:100){
  no_tratada <- rnorm(1000, mean = 10, sd = 1)
  tratada    <- rnorm(1000, mean = 10, sd = 1)
  pvalues <- c(pvalues, t.test(no_tratada, tratada)$p.value)
}
table(pvalues < 0.05)
#¿Qué pasó? ¿Qué tipo de errores observamos?

#Sigamos analizando la salida del test, volviendo a las alturas en Holanda
t.test(alturasHolanda, mu = 175)

#¿Qué es el intervalo de confianza de 95%?
#El mismo test nos estima la media de la población de la muestra que tomamos. Nos dice que la estima en 181.3 cm pero nos da algo mejor, nos da un intervalo
#de confianza del 98%, entre 180.23 cm hasta 182.37 cm. ¿Esto significa que hay un 95% de probabilidades de que la media real de la población de Holanda esté
#en este intervalo? No! La media de la población no es una variable aleatoria, o está en el intervalo o no está. Lo que nos dice ese intervalo es que si 
#tomamos 100 muestras y construimos el intervalo de confianza de 95%, esperamos que en el 95% de los casos la media de la población 
#esté contenida en el intervalo.
#Probemosló con nuestro laboratorio. Supongamos que la media de altura de la población de Holanda es 182 cm con un desvío de 7cm y tomemos 100 muestras.
set.seed(1234567)
mediaReal <- 182
aciertos <- 0
aciertos_pval <- 0
muestras <- 100
for(i in 1:muestras){
  alturasHolanda <- rnorm(10, mean = mediaReal, sd = 7)
  testDeAltura <- t.test(alturasHolanda, mu = 175)
  if((testDeAltura$conf.int[1] < mediaReal & mediaReal < testDeAltura$conf.int[2]) & testDeAltura$p.value < 0.05){
    aciertos <- aciertos + 1
  }
}
aciertos


#Queremos estudiar el efecto de dos tratamientos en el crecimiento de una planta. Para ello contamos con plantas a las que se las trató con un placebo, plantas
#tratadas con la droga1 y plantas tratadas con la droga 2. ¿Cómo podemos saber si el tratamiento 1 o el tratamiento 2 fue efectivo?
#Usemos un t Test de dos muestras independientes para comparar el control con tratamiento 1 y el control con tratamiento 2. Además de lo que le habíamos
#pedido al t Test de una muestra (independencia, datos continuos, normalidad y sin outliers), la varianza de los dos grupos tiene que ser la misma.

#Cargamos los datos
plantas <- datasets::PlantGrowth
View(plantas)
#Veamos como se distribuyen las plantas dentro de cada grupo
boxplot(weight ~ group, data = plantas)

#Para testear que las dos varianzas sean iguales (homogeneidad de varianzas) podemos usar el test de bartlett.
#H0: Los datos tienen igual varianza
#H1: Los datos no  tienen igual varianza
bartlett.test(list(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt1"]))
bartlett.test(list(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt2"]))

#Chequeamos previamente normalidad de cada variable
shapiro.test(plantas$weight[plantas$group == "ctrl"])
shapiro.test(plantas$weight[plantas$group == "trt1"])
shapiro.test(plantas$weight[plantas$group == "trt2"])

#Ahora si, podemos usar el t test de dos muestras
#H0: las medias de los dos grupos son iguales
#H1: las medias de los dos grupos son distintas

t.test(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt1"], var.equal = TRUE)
t.test(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt2"], var.equal = TRUE)

#¿Qué pasa si nuestros datos no cumplen homogeneidad de varianza?
#Veamos los datos de iris
View(iris)

#Comparamos setosa con versicolor en el largo del sépalo
shapiro.test(iris$Sepal.Length[iris$Species == "setosa"])
shapiro.test(iris$Sepal.Length[iris$Species == "versicolor"])
#Veamos si cumplen homogeneidad de varianza
bartlett.test(list(iris$Sepal.Length[iris$Species == "setosa"], iris$Sepal.Length[iris$Species == "versicolor"]))

#Ups! Qué podemos hacer? Usar el test de welch
t.test(iris$Sepal.Length[iris$Species == "setosa"], iris$Sepal.Length[iris$Species == "versicolor"], var.equal = F)

#Mucho se habló de la publicación (o no publicación) de los resultados de la vacuna sputnik v.
#Finalmente, los resultados se publicaron el 2 de febrero en The Lancet, en
#Safety and efficacy of an rAd26 and rAd5 vector-based heterologous prime-boost COVID-19 vaccine: an interim analysis 
#of a randomised controlled phase 3 trial in Russia. Logunov et al.
#https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)00234-8/fulltext
#En el texto nos cuentan que hicieron un ensayo doble ciego con 21977 adultos, de los cuales 16501 fueron vacunados con la sputnik y 5476 recibieron placebo.
#Luego de 21 días, de 14964 participantes vacunados, 16 contrajeron COVID-19, mientras que de 4902 del grupo placebo, 62 lo contrajeron.  
#Armemos la tabla de datos
pacientes <- data.frame(tratado = c(rep(TRUE, 14964), rep(FALSE, 4902)), 
                        infectado = c(rep(TRUE, 16), rep(FALSE, (14964-16)), rep(TRUE, 62), rep(FALSE, (4902-62))))
head(pacientes)
#Se obtuvo la siguiente tabla (llamada tabla de contingencia):
table(pacientes$tratado, pacientes$infectado)

#Podemos utilizar un test de chi cuadrado para ver si son independientes la infección y la vacuna. El test
#de chi cuadrado requiere que todos los valores esperados en la tabla de contingencia sean mayores a 5.
#R nos va a avisar si no se cumple la condición. En ese caso, podemos usar el test exacto de fisher fisher.test()
#H0: Las dos variables son independientes
#H1: Las dos variables no son independientes

chisq.test(pacientes$tratado, pacientes$infectado, correct=FALSE)
#Descartamos que sea independiente la infección de la vacuna.