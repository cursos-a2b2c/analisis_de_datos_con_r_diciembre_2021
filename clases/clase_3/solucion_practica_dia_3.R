#################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biologíca Computacional
#                 Fundación Instituto Leloir
#                        Marzo 2021
#            Práctica Estadística descriptiva y AED
#################################################################

#Primero le decimos a R en qué directorio queremos que trabaje
setwd("clases/clase_3") #reemplazalo con tu directorio de trabajo

#1) Medidas de resumen

#a)
#Cargamos antropometria
antropometria <- read.csv("datasets/antropometria.csv", stringsAsFactors=FALSE)

#b)
#Cantidad de datos
nrow(antropometria)

#Cantidad de atributos
ncol(antropometria)

#Tipo de los atributos
str(antropometria)

#Nombre de los atributos
colnames(antropometria)

#Primeros 5 y últimos 5 datos
head(antropometria, n = 5)
tail(antropometria, n = 5)

#El dataset consiste en 572 observaciones con 4 atributos cada una. El sexo es un atributo tipo 
#character, mientras que la altura, el peso y la edad son numéricos.

#c)
table(antropometria$sex)
#Vemos que hay algunos datos que están mal cargados, en lugar de F o M figuran números, podríamos sacarlos antes de seguir trabajando.
datos_validos <- antropometria$sex == "F" | antropometria$sex == "M" #(solo nos interesan los que digan F o M. o lógico se escribe con | en R)
antropometria <- antropometria[datos_validos, ] #Subseteamos los datos válidos
table(antropometria$sex) #Vemos si se resolvió el problema

#d)
#Separamos las alturas en hombre y mujer
alturaHombres <- antropometria$height[antropometria$sex == "M"]
alturaMujeres <- antropometria$height[antropometria$sex == "F"]

#Podemos usar summary o cada función por separado
summary(alturaHombres) #Observamos que hay un dato NA, a cada función le vamos a tener que pedir que retire los NAs, si no van a contagiar.

max(alturaHombres, na.rm = T) #na.rm remueve los NA
min(alturaHombres, na.rm = T) #na.rm remueve los NA
mean(alturaHombres, na.rm = T) #na.rm remueve los NA
median(alturaHombres, na.rm = T) #na.rm remueve los NA
var(alturaHombres, na.rm = T) #na.rm remueve los NA
sd(alturaHombres, na.rm = T) #na.rm remueve los NA

#Hacemos lo mismo para las mujeres
summary(alturaMujeres) #Observamos que no hay NAs

max(alturaMujeres)
min(alturaMujeres)
mean(alturaMujeres)
median(alturaMujeres)
var(alturaMujeres)
sd(alturaMujeres)

#Los hombres en promedio tienen mayor altura pero mayor varianza en las alturas que las mujeres.

#e)
#Cargamos los casos de covid por país
cases.covid.19.by.country <- read.csv("datasets/cases-covid-19-by-country.csv", stringsAsFactors=FALSE)

#f, g, h, i)
#Cantidad de datos
nrow(cases.covid.19.by.country)

#Cantidad de atributos
ncol(cases.covid.19.by.country)

#Tipo de los atributos
str(cases.covid.19.by.country)

#Nombre de los atributos
colnames(cases.covid.19.by.country)

#Primeros 5 y últimos 5 datos
head(cases.covid.19.by.country, n = 5)
tail(cases.covid.19.by.country, n = 5)

#El dataset consiste en 213 observaciones con 3 atributos cada una. El nombre del país es un atributo tipo 
#character, mientras que el acumulado de casos y el acumulado de fallecimientos es numérico.

#j)
#Medidas de centralidad, media, mediana y moda
mean(cases.covid.19.by.country$cum_conf)
median(cases.covid.19.by.country$cum_conf)
which.max(table(cases.covid.19.by.country$cum_conf))

mean(cases.covid.19.by.country$cum_death)
median(cases.covid.19.by.country$cum_death)
which.max(table(cases.covid.19.by.country$cum_death))

#Son muy diferentes las tres medidas

#k)
#Medidas de dispersión, rango, desvío estandar e IQR
max(cases.covid.19.by.country$cum_conf)-min(cases.covid.19.by.country$cum_conf)
sd(cases.covid.19.by.country$cum_conf)
IQR(cases.covid.19.by.country$cum_conf)

max(cases.covid.19.by.country$cum_death)-min(cases.covid.19.by.country$cum_death)
sd(cases.covid.19.by.country$cum_death)
IQR(cases.covid.19.by.country$cum_death)

#l)
#Los datos son muy dispersos. Convendría usar mediana e IQR como medidas de resumen

#2) Visualización
#a)
antropometria <- read.csv("datasets/antropometria.csv", stringsAsFactors=FALSE)

#b)
plot(antropometria$age, antropometria$height, xlab = "Edad (años)", ylab = "Altura (cm)")
#Parece existir relación entre la altura y edad hasta aproximadamente los 20 años. A partir de ahí, la altura no pareciera cambiar con la edad.

#Grafiquemos por sexo
plot(antropometria$age[antropometria$sex == "M"], antropometria$height[antropometria$sex == "M"], col = "red", xlab = "Edad (años)", ylab = "Altura (cm)")
points(antropometria$age[antropometria$sex == "F"], antropometria$height[antropometria$sex == "F"], col = "green")
legend("bottomright", legend = c("Hombres", "Mujeres"), col = c("red", "green"), pch = 1)
#La relación entre altura y edad no parece depender del sexo

#c)
#Esto lo hacemos a ojo, elegimos alrededor de los 20 años. El ejercicio no pretende que calculen esto de forma automática.
abline(v = 20, col = "blue", lty = 2)

#d) Subseteamos y nos quedamos con los mayores a 20 años. La guía tiene un typo, en lugar de vector debería decir data.frame
adultos <- antropometria[antropometria$age > 20, ]
#Eliminemos los NAs que hayan quedado para sacarnos ese problema de encima. Podemos usar un for, un apply o vectorizado, la función complete.cases que devuelve
#solamente las observaciones que no tienen NA
adultos <- adultos[complete.cases(adultos), ]

#e) 
#Podemos usar fórmulas para hacer esto más fácil
boxplot(height ~ sex, data = adultos)
legend("topleft", legend = c("Altura de mujeres y hombres"))
#Parece variar la altura dependiendo del sexo, como era de esperar.

boxplot(weight ~ sex, data = adultos)
legend("topleft", legend = c("Peso de mujeres y hombres"))
#Parece variar el peso dependiendo del sexo, como era de esperar.

#f)
#Graficamos altura vs peso para mujeres y para hombres por separado.
plot(adultos$weight[adultos$sex == "F"], adultos$height[adultos$sex == "F"], xlab = "Peso (kg)", ylab = "Altura (cm)", main = "Peso y Altura para Mujeres")
#Calculamos la correlación entre esas dos variables usando cor. La correlación vale como máximo 1.
cor(adultos$weight[adultos$sex == "F"], adultos$height[adultos$sex == "F"])
#Parece existir correlación entre ambas variables. Queda el ejercicio para Hombre.

#g) 
hist(adultos$height[adultos$sex == "M"], col = "red", breaks = 20, xlab = "Altura", main = "Histograma de altura")
hist(adultos$height[adultos$sex == "F"], col = "green", add = T, breaks = 20)
legend("topleft", legend = c("Mujeres", "Hombres"), col = c("green", "red"), fill = c("green", "red"))
#Vemos que las distribuciones se solapan pero las medias son bien distintas.

#h) 
cases.covid.19.by.country <- read.csv("datasets/cases-covid-19-by-country.csv", stringsAsFactors=FALSE)

#i)
#Hacemos histogramas. Voy a usar el total de los casos confirmados, ustedes pueden usar otras que gusten.
hist(cases.covid.19.by.country$cum_conf, main = "Histograma de casos confirmados COVID", xlab = "Casos confirmados")

#Hay mucha diferencia entre países, vamos a analizar los países con menos casos.
#Nos podemos quedar con los datos que estén por debajo del "bigote", tercer cuartil + 1.5*IQR
#Calculamos los valores que nos interesan
tercer_cuartil <- summary(cases.covid.19.by.country$cum_conf)[5]
iqr            <- IQR(cases.covid.19.by.country$cum_conf)

#Vemos cuales cumplen con la condición y nos quedamos solo con esos
ids_casos <- which(cases.covid.19.by.country$cum_conf < tercer_cuartil + 1.5*iqr)
cum_conf <- cases.covid.19.by.country$cum_conf[ids_casos]
summary(cum_conf)
hist(cum_conf, main = "Histograma de casos confirmados COVID", xlab = "Casos confirmados")

#j)
#Grafiquemos un histograma y un boxplot para com_conf
layout(matrix(1:2, ncol=1, nrow=2)) #Con esto podemos graficar dos gráficos en una sola columna, dos filas
hist(cum_conf, main = "Casos confirmados COVID", xlab = "Casos confirmados")
boxplot(cum_conf, horizontal = T)

#Siguen apareciendo valores extremos en el boxplot porque al sacar algunos datos, cambian todas las
#medidas y las escalas. En particular, cambia la mediana y el IQR

#k)
#Grafiquemos un scatterplot
layout(1) #Volvemos a un sólo gráfico por vez
plot(cases.covid.19.by.country$cum_conf, cases.covid.19.by.country$cum_death, xlab = "Casos", ylab = "Fallecimientos", main = "Casos de COVID-19 por país")

#Los casos extremos no nos permiten visualizar con detalle la relación. Debemos sacarlos.
tercer_cuartil_conf <- summary(cases.covid.19.by.country$cum_conf)[5]
iqr_conf            <- IQR(cases.covid.19.by.country$cum_conf)

tercer_cuartil_death <- summary(cases.covid.19.by.country$cum_death)[5]
iqr_death            <- IQR(cases.covid.19.by.country$cum_death)

#Vemos cuales cumplen con las dos condiciones y nos quedamos solo con esos
ids_casos <- which(cases.covid.19.by.country$cum_conf < tercer_cuartil_conf + 1.5*iqr_conf &
                   cases.covid.19.by.country$cum_death < tercer_cuartil_death + 1.5*iqr_death)
casos_de_interes <- cases.covid.19.by.country[ids_casos, ]

#Grafiquemos
plot(casos_de_interes$cum_conf, casos_de_interes$cum_death, xlab = "Casos", ylab = "Fallecimientos", main = "Casos de COVID-19 por país")

#Sigue siendo dificil de identificar que le pasa a la mayor parte de los casos, recortemos nuevamente
#hagamos un zoom a la parte más llena
ids_casos <- which(cases.covid.19.by.country$cum_conf < 500 &
                   cases.covid.19.by.country$cum_death < 10)
casos_de_interes <- cases.covid.19.by.country[ids_casos, ]

#Grafiquemos
plot(casos_de_interes$cum_conf, casos_de_interes$cum_death, main = "Casos confirmados vs. fallecimientos", xlab = "Casos confirmados", ylab="Fallecimientos")

#Si bien para valores grandes de casos parece haber una relación entre cantidad de casos y cantidad
#de fallecimientos, no parece haber una relación inmediata entre la cantidad de casos y la cantidad
#de fallecimientos en los casos más chicos. ¿Por qué podría estar pasando esto?
