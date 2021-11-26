#################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biologíca Computacional
#                         Octubre 2020
#                      Test de hipótesis
#################################################################

#Usemos R como un simulador. Simulemos tirar un dado miles de veces. La función sample va a venir en
#nuestra ayuda para esto.
?sample

#Las computadoras son máquinas deterministas, ante la misma entrada siempre devuelven la misma salida.
#Si queremos simular azar, necesitamos un generador de números pseudo-aleatorios. 
#Los generadores de números pseudo-aleatorios necesitan un lugar desde donde comenzar a producir
#los números y eso se lo decimos a R usando set.seed
#Un poco más de información para los curiosos: https://es.wikipedia.org/wiki/Generador_de_n%C3%BAmeros_pseudoaleatorios
set.seed(123457)

#Fabriquemos un dado y tirémoslo varias veces
dado <- 1:6
dado
sample(x = dado, size = 1)
sample(x = dado, size = 1)
sample(x = dado, size = 1)
sample(x = dado, size = 1)
#Cada vez que lo tiramos da algo distinto!

#Ahora tiremos dos dados
sample(dado, 2, replace=T)
sample(dado, 2, replace=T)
sample(dado, 2, replace=T)
sample(dado, 2, replace=T)

#Tiramos diez veces los dos dados. Podríamos usar un for pero r nos ayuda usando replicate
?replicate

#Fabriquemos una función que tire los dos dados. Fijensé que una función no necesariamente requiere
#un parámetro
dosDados <- function(){
  #Fabrico el dado
  dado <- 1:6
  #Lo tiro dos veces
  tirada <- sample(dado, 2, replace=T)
  #Devuelvo la tirada
  return(tirada)
}  

#Tiremos los dados diez veces
diez_tiradas <- replicate(n = 10, dosDados())

#Veamos qué devuelve R
diez_tiradas

#Transponemos para que cada columna sea un dado y cada fila una tirada 
?t
diez_tiradas <- t(diez_tiradas)
diez_tiradas

#Sumemos los dos dados de cada tirada. Podríamos usar un for pero por rowSums viene a nuestra ayuda
#para sumar los elementos de cada fila entre si, fila por fila.
?rowSums
suma_de_las_caras <- rowSums(diez_tiradas)
suma_de_las_caras

#¿Es igual de probable sacar cualquier número? Contemos cuantas veces aparece cada número.
table(suma_de_las_caras)

#¿Si corremos esto de nuevo obtenemos el mismo resultado?
diez_tiradas <- replicate(10, dosDados())
diez_tiradas <- t(diez_tiradas)
suma_de_las_caras2 <- rowSums(diez_tiradas)

#Comparemos con la tirada anterior
table(suma_de_las_caras)
table(suma_de_las_caras2)

#¿Cómo podemos intentar estimar la probabilidad real de que salga cada número?
#Si tiramos muchas veces y contamos cuantas veces 
#da cada resultado nos podemos acercar bastante al valor real
diezmil_tiradas <- replicate(10000, dosDados())
diezmil_tiradas <- t(diezmil_tiradas)
head(diezmil_tiradas)
suma_de_las_caras <- rowSums(diezmil_tiradas)

#Calculemos la probabilidad estimada de cada número. Esto lo podemos pensar como el porcentaje de
#veces que salió cada uno
probabilidad_estimada <- table(suma_de_las_caras)/length(suma_de_las_caras)

#La posta se puede calcular (¿cómo?) y es la siguiente:
probabilidad_real <- c(1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36)

#Grafiquemos la probabilidad estimada y la probabilidad real
plot(probabilidad_estimada, xlab="x", ylab = "P", main="Distribución (estimada) de probabilidad de 'suma de las caras'")
points(2:12, probabilidad_real, col = "red")

#Bastante bien la simulación, no? Esto es lo que se conoce como la ley de los grandes números.

#Para pensar: probar con otros valores de replicate, por ejemplo: 10, 100, 1000, 1000000 y ver cuántas tiradas
#necesitamos para acercarnos.

#¿Cómo calcularían la probabilidad de que la suma de entre 3 y 7?
entre <- suma_de_las_caras >= 3 & suma_de_las_caras <= 7
head(entre)
sum(entre)/length(suma_de_las_caras)

#¿Dará lo mismo si sumamos las probabilidades de que salga cada número, desde el 3 al 7?
probabilidad_estimada
sum(probabilidad_estimada[c("3", "4", "5", "6", "7")])

#¿Y cuanto da la probabilidad de que salga cualquiera de los números desde el 2 al 12?
sum(probabilidad_estimada)

#Veamos como podemos simular otras distribuciones con R. Exploremos la función runif a ver qué devuelve.
#Primero reiniciemos el generador de números aleatorios
set.seed(123457)
runif(n = 1)
runif(n = 1)
runif(n = 1)
runif(n = 1)

#Corramos 100 veces runif con replicate y grafiquemos un histograma de lo que obtuvimos
unif <- replicate(100, runif(n = 1))
hist(unif)

#Corramos 1000 veces runif con replicate y grafiquemos un histograma de lo que obtuvimos
unif <- replicate(1000, runif(n = 1))
hist(unif)

#runif viene con su propio replicate incorporado, podemos pedirle 100000 números directamente. Grafiquemos un histograma de lo que obtuvimos.
unif <- runif(100000)
hist(unif)

#¿Qué devuelve entonces runif?
#¿Qué pasa al aumentar la muestra?

#Veamos cuántos números hay menores a 0.5. ¿Cuántos números esperan?
menores <- unif < 0.5
head(menores)
table(menores)

#¿Y menores a 0.95?.
menores <- unif < 0.95
table(menores)

#¿Y entre 0.25 y 0.75? ¿Qué porcentaje tiene?.
entre <- unif > 0.25 & unif < 0.75
table(entre)/length(unif)

#Veamos una función más, rnorm. ¿Qué es cada parámetro?
?rnorm

#Generemos 10000 números con esta funcion y grafiquemos un histograma
norm <- rnorm(n = 10000, mean = 0, sd = 1)
hist(norm)
#¿Reconocen esta distribución?

#¿Cuál debería ser la media y el desvío estandar si los estimamos con la muestra que generamos?
mean(norm)
sd(norm)
#¿Son exactamente las esperadas? ¿Por qué?

#Veamos como obtener la densidad de probabilidad real (no la estimada) de una normal con R. 
#Elijamos una secuencia de x.
x <- seq(-4, 4, by = 0.1)
head(x)
tail(x)

#Usamos la función dnorm que devuelve la densidad de probabilidad para cada valor de x que usamos
d <- dnorm(x, mean = 0, sd = 1)

#Grafiquemos cada par x, d de la densidad de probabilidad real.
plot(x, d, main = "Densidad de probabilidad normal (mean = 0, sd = 1)")

#¿Cómo podemos calcular la probabilidad de que al sacar una x cualquiera, la misma sea menor a -1? pnorm al rescate
abline(v = -1, col="red")
p1 <- pnorm(q = -1, mean = 0, sd = 1)
p1
table(norm < -1)/length(norm)

#¿Y menor a 1?
abline(v = 1, col="red")
p2 <- pnorm(q = 1, mean = 0, sd = 1)
#¿Cómo podemos calcular la probabilidad de que la x esté entre -1 y 1?
p2 - p1
#El 68% de las x están entre -1 y 1, es decir, el 68% de las x están a un desvío estandar de la media!

#¿Cuál es el valor de x tal que la probabilidad de sacar una x menor sea de 0.025%? qnorm al rescate
x3 <- qnorm(p = 0.25, mean = 0, sd = 1)
pnorm(q = x3, mean = 0, sd = 1)

x1 <- qnorm(p = 0.025, mean = 0, sd = 1)
abline(v = x1, col="blue")

#¿Cuál es el valor de x tal que la probabilidad de sacar una x menor sea de 97.5%? qnorm al rescate
x2 <- qnorm(p = 0.975, mean = 0, sd = 1)
abline(v = x2, col="blue")
#Entonces, ¿cuál es la probabilidad de sacar una x entre x1 y x2?
0.975-0.025
#Con estas herramientas ya estamos en condiciones de probar algunas cosas.
#Juguemos a ser Levi-Strauss y el tipo de los aliens. Simulemos nuestros propios seres humanos. 
#Supongamos que la distribución de la altura es normal, con media 175 cm y desvío estandar 7 cm. Tomemos una muestra de esa población.
set.seed(123457)
muestra1 <- rnorm(n = 10, mean = 175, sd = 7)
muestra1

#Con esta muestra, ¿Qué valor estimamos para la media?
mean(muestra1)

#¿Y si tomamos una nueva muestra?
muestra2 <- rnorm(n = 10, mean = 175, sd = 7)
muestra2

#Con esta muestra, ¿Qué valor estimamos para la media?
mean(muestra2)

#Volviendo a los aliens, ¿podemos cuantificar de alguna forma la sorpresa (o falta de sorpresa) que nos dio cada uno de los esqueletos encontrados?
#Podríamos preguntarnos cuál es la probabilidad de encontrar una persona de 170 cm o más por azar. Usamos pnorm que nos da la probabilidad de 170 o menos
#así que eso se lo tenemos que restar a la probabilidad total que es 1
x <- seq(150, 200, by = 1)
y <- dnorm(x, mean = 175, sd = 7)
plot(x, y)

pnorm(170, mean = 175, sd = 7)
abline(v = 170)

pvalue1 <- 1 - pnorm(170, mean = 175, sd = 7)
pvalue1

#A este valor se lo llama pvalue o pvalor y cuantifica la probabilidad de obtener el resultado que obtuvimos o uno mayor por azar, suponiendo que nuestro
#dato provino de una cierta población (en este caso, suponiendo que el esqueleto era humano).
#Calculemos el pvalue para el de 400 cm
pvalue3 <- 1 - pnorm(400, mean = 175, sd = 7)
pvalue3

#¿Y qué pasa con el de 185 cm?
pvalue2 <- 1 - pnorm(185, mean = 175, sd = 7)
pvalue2

#¿Qué pasó? ¿Nos estaremos equivocando?

pvalue2 <- 1 - pnorm(190, mean = 175, sd = 7)
pvalue2

#Veamos cuál es el valor de altura crítico para descartar que un esqueleto sea humano con una significancia de 0.01. qnorm al rescate. 
maxNoSignificativo <- qnorm(0.99, mean = 175, sd = 7)
maxNoSignificativo
abline(v = maxNoSignificativo, col = "red")

#En este caso pudimos hacer un test de hipótesis porque conocíamos la distribución de alturas para personas (es decir, la hipótesis nula). 
#Pero ¿qué pasa en casos donde no conocemos esa distribución? 
#Necesitamos construirnos un estadístico a partir de nuestros datos cuya distribución, si vale la hipótesis nula, sea conocida. 
#Por suerte para nosotros, hay muchísimos tests que podemos usar en R dependiendo de lo que necesitamos hacer.

#Tomamos una muestra de la altura de personas en Holanda y queremos saber si la altura de esas personas es significativamente diferente de la
#media mundial. Usamos un t Test de una muestra. El t Test require que los datos sean continuos, independientes, aproximadamente normales y sin outliers
#(aunque es bastante robusto si esto no se cumple del todo).
alturasHolanda <- c(182, 183, 182, 180, 181, 180, 182, 181, 182, 181)

#Veamos outliers
boxplot(alturasHolanda)

#Veamos que estos datos cumplen normalidad. Usamos el test de shapiro-wilk que testea justamente eso

#H0: Los datos son normales
#H1: Los datos no son normales
shapiro.test(alturasHolanda)

#Cumplen normalidad, podemos usar t Test. Pero antes, elijamos un nivel de significancia (usualmente 0.05, 0.01)
#H0: alturaMedia = 175 
#H1: alturaMedia != 175
?t.test
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
muestras <- 100
for(i in 1:muestras){
  alturasHolanda <- rnorm(10, mean = mediaReal, sd = 7)
  testDeAltura <- t.test(alturasHolanda, mu = 175)
  if(testDeAltura$conf.int[1] < mediaReal & mediaReal < testDeAltura$conf.int[2]){
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
class(weight ~ group)
boxplot(weight ~ group, data = plantas)

#Para testear que las dos varianzas sean iguales (homogeneidad de varianzas) podemos usar el test de bartlett.
#H0: Los datos tienen igual varianza
#H1: Los datos no tienen igual varianza
bartlett.test(list(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt1"]))
bartlett.test(list(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt2"]))

#Chequeamos previamente normalidad de cada variable
shapiro.test(plantas$weight[plantas$group == "ctrl"])
shapiro.test(plantas$weight[plantas$group == "trt1"])
shapiro.test(plantas$weight[plantas$group == "trt2"])

#Ahora si, podemos usar el t test de dos muestras
#H0: las medias de los dos grupos son iguales
#H1: las medias de los dos grupos son distintas

t.test(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt1"], var.equal = T)
t.test(plantas$weight[plantas$group == "ctrl"], plantas$weight[plantas$group == "trt2"], var.equal = T)

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

#Queremos estudiar el efecto de una droga para tratar una cierta enfermedad.
#Tenemos 105 pacientes, de los cuales 50 fueron tratados con la droga y 55 no. Después de un mes se controló el estado de salud de todos los pacientes.
pacientes <- read.csv("https://goo.gl/j6lRXD")
head(pacientes)

#Se obtuvo la siguiente tabla (llamada tabla de contingencia):
table(pacientes$treatment, pacientes$improvement)

#Podemos utilizar un test de chi cuadrado para ver si son independientes la mejora del tratamiento. El test
#de chi cuadrado requiere que todos los valores esperados en la tabla de contingencia sean mayores a 5.
#R nos va a avisar si no se cumple la condición. En ese caso, podemos usar el test exacto de fisher fisher.test()
#H0: Las dos variables son independientes
#H1: Las dos variables no son independientes

chisq.test(pacientes$treatment, pacientes$improvement, correct=FALSE)
