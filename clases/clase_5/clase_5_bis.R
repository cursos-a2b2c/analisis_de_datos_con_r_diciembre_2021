#################################################################
#              Curso de análisis de datos con R
#          Machine learning: aprendizaje supervisado
#################################################################
#A veces queremos irnos un poco de la estadistica y hacer otro tipo de analisis.
#En particular, nos va a interesar no solo modelar nuestros datos, sino tambien
#poder encontrar patrones que no son obvios y poder predecir el comportamiento
#de nuevas observaciones.

#Veamos el dataset trees
#This data set provides measurements of the diameter, height and volume of 
#timber in 31 felled black cherry trees. 
#Note that the diameter (in inches) is erroneously labelled Girth in the data.
#It is measured at 4 ft 6 in above the ground.

#Exploremos el dataset
View(trees)
colnames(trees)
head(trees)
nrow(trees)
plot(trees$Girth, trees$Volume, xlab = "diametro", ylab = "volumen", main = "Cerezos")

#Cómo parece ser la relación entre el diámetro y el volumen? Por que?
#.
#.
#.


#R permite ajustar un modelo lineal a los datos usando un objeto de tipo "formula", que es una forma facil
#de especificar la relacion entre variables
formula_para_ajuste <- Volume ~ Girth
formula_para_ajuste
class(formula_para_ajuste)

#Podemos hacer que dependa de todas las variables que querramos
Volume ~ Girth + Height

#E incluso usar interacción
Volume ~ Girth + Height + Girth:Height

#Ajustemos un modelo lineal
ajuste <- lm(formula = formula_para_ajuste, data = trees)

#Veamos qué devuelve el ajuste? Que es cada cosa? Que significan?
summary(ajuste)

#La interpretacion fisica o biologica se la tenemos que dar nosotros, no viene de los datos
#Grafiquemos todo junto
abline(ajuste, col="red")

#Ademas de modelar una relacion, podemos usar este modelo para encontrar el volumen de un arbol solamente midiendo su diametro 
#Medimos el diametro de un nuevo árbol, que volumen esperamos que tenga?
nuevo_arbol <- data.frame(Girth = 13.7)
volumen <- predict.lm(ajuste, nuevo_arbol)
points(nuevo_arbol$Girth[1], volumen, col="blue", pch=19)

#Esta prediccion es buena? Como podriamos saberlo si no medimos el volumen?
#Podemos usar los valores conocidos y compararlos con como los predice mi modelo 
fitted(ajuste)

#Y con esto calcular el error cuadratico medio de mi prediccion
mean((trees$Volume-fitted(ajuste))^2)

#Y si en lugar de usar una funcion lineal, usaramos un polinomio de grado 17?
ajuste_polinomial <- lm(Volume ~ poly(Girth,17), trees)
mean((trees$Volume-fitted(ajuste_polinomial))^2)

#Bastante menos, pero esta bueno? por que?
lines(trees$Girth, fitted(ajuste_polinomial), col = "green")

volumen <- predict.lm(ajuste_polinomial, nuevo_arbol)
points(nuevo_arbol$Girth[1], volumen, col="violet", pch=19)
#Se ajusta demasiado a nuestros puntos, no nos permite generalizar a nuevos datos, estamos sobreajustando!
#Querer saber como se comporta nuestro modelo usando los mismos datos con los que lo "entrenamos" es mordernos la cola.
#Una solucion es separar un conjunto de datos de validacion, un 20% de los datos, por ejemplo, y "entrenar" nuestro modelo con el 80%
#restante.
id_datos_de_validacion <- c(3, 7, 10, 15, 19, 26)
datos_de_entrenamiento <- trees[-id_datos_de_validacion, ]
datos_de_validacion    <- trees[id_datos_de_validacion, ]

#Ajustemos un modelo lineal
ajuste_con_subconjunto <- lm(formula = formula_para_ajuste, data = datos_de_entrenamiento)
abline(ajuste_con_subconjunto, col = "brown")

#Veamos como cambiaron las cosas
summary(ajuste)
summary(ajuste_con_subconjunto)
prediccion_volumen_validacion <- predict.lm(ajuste, datos_de_validacion)

mean((datos_de_validacion$Volume-prediccion_volumen_validacion)^2)
mean((trees$Volume-fitted(ajuste))^2) #Comparado con el anterior
#Esta es una buena estimacion de como generaliza nuestro modelo a nuevos datos. Siempre conviene entrenar un modelo con unos datos 
#y validarlo con otros para tener una estimacion real de lo que hace.


#Veamos otro problema. Tenemos datos de diabetes en mujeres mayores de 21 anios de ascendencia Pima residentes en Phoenix, Arizona.
#De cada mujer se controlo si eran diabéticas o no de acuerdo a los criterios de la OMS. Los datos fueron recogidos por 
#la US National Institute of Diabetes and Digestive and Kidney Diseases 
#y se pueden acceder aca: https://www.kaggle.com/kumargh/pimaindiansdiabetescsv
# 1. Number of times pregnant
# 2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test
# 3. Diastolic blood pressure (mm Hg)
# 4. Triceps skin fold thickness (mm)
# 5. 2-Hour serum insulin (mu U/ml)
# 6. Body mass index (weight in kg/(height in m)^2)
# 7. Diabetes pedigree function
# 8. Age (years)
# 9. Class variable (0 or 1)

#Problema: nos interesa poder predecir en base a alguna/s de estas variables, si una mujer pima es o no diabetica, sin necesidad de 
#ir a controlarla.
#Veamos que podemos hacer

#Cargamos los datos

pima_indians_diabetes <- read.csv("~/cursos/analisis_de_datos_con_r_diciembre_2021/clases/clase_5/pima-indians-diabetes.csv", header = T)

#Los exploramos y vemos si hay relaciones entre las variables
View(pima_indians_diabetes)
summary(pima_indians_diabetes)

#Sacamos valores tipo NA (que en este caso son 0)
casos_a_sacar <- which(pima_indians_diabetes$Glucose == 0 | pima_indians_diabetes$BloodPressure == 0 | pima_indians_diabetes$BMI == 0)
casos_a_sacar
length(casos_a_sacar) #42 casos con valores faltantes
pima_indians_diabetes <- pima_indians_diabetes[-casos_a_sacar, c("Pregnancies", "Glucose", "BloodPressure", 
                                                                 "BMI", "DiabetesPedigreeFunction", "Age", "Class")] #los sacamos y ademas nos quedamos solo con las columnas de interes

#Veamos si podemos graficar algo y encontrar relaciones ahi
pairs(pima_indians_diabetes) #Esta dificil, muchas variables!

#Grafiquemos los puntos, pero tenemos 6 variables, como podemos hacer para graficar en seis dimensiones?
#PCA!

#Ojo que cada variable esta en una escala diferente y en unidades diferentes.
#Tenemos que pasar todas las variables a una escala comun para que sean comparables. Esto se llama estandarizar
boxplot(pima_indians_diabetes[, c("Pregnancies", "Glucose", "BloodPressure", "BMI", "DiabetesPedigreeFunction", "Age")])
pima_indians_diabetes_estandarizado <- scale(pima_indians_diabetes[, c("Pregnancies", "Glucose", "BloodPressure", "BMI", "DiabetesPedigreeFunction", "Age")])

#veamos que informacion nos da la estandarizacion
pima_indians_diabetes_estandarizado

boxplot(pima_indians_diabetes_estandarizado)

#Ahora si, calculamos pca
pca <- prcomp(pima_indians_diabetes_estandarizado)

#Vemos que porcentaje de varianza explica cada nueva variable
plot(pca$sdev/sum(pca$sdev)*100, xlab = "# variable", ylab = "Porcentaje de variable explicada")
#Con las dos primeras variables ya explicamos mas de un 60%, asi que el grafico en dos dimensiones deberia ser bastante representativo

#Usamos la clase para pintar de color. Armamos un vector con el color
color <- rep("green", nrow(pima_indians_diabetes))
color[pima_indians_diabetes$Class == 1] <- "red"
plot(pca$x[, 1:2], col = color, cex = 0.9)

#Que pasa si tenemos nuevas observaciones
pima_indians_diabetes_nuevas <- read.csv("~/cursos/analisis_de_datos_con_r_diciembre_2021/clases/clase_5/pima-indians-diabetes-nuevas.csv", header = T)

#Sacamos los casos incompletos
casos_a_sacar <- which(pima_indians_diabetes_nuevas$Glucose == 0 | pima_indians_diabetes_nuevas$BloodPressure == 0 | pima_indians_diabetes_nuevas$BMI == 0)
casos_a_sacar
length(casos_a_sacar) #42 casos con valores faltantes
pima_indians_diabetes_nuevas <- pima_indians_diabetes_nuevas[-casos_a_sacar, c("Pregnancies", "Glucose", "BloodPressure", 
                                                                                "BMI", "DiabetesPedigreeFunction", "Age", "Class")] #los sacamos y ademas nos quedamos solo con las columnas de interes

#Las estandarizamos usando la estandarizacion de los datos anteriores
pima_indians_diabetes_nuevas_estandarizado <- scale(pima_indians_diabetes_nuevas[, c("Pregnancies", "Glucose", "BloodPressure", "BMI", "DiabetesPedigreeFunction", "Age")], center = attr(pima_indians_diabetes_estandarizado, "scaled:center"), scale = attr(pima_indians_diabetes_estandarizado, "scaled:scale"))

#Rotamos los datos nuevos de acuerdo a lo que sale en pca
pima_indians_diabetes_nuevas_estandarizado_pca <- predict(pca, pima_indians_diabetes_nuevas_estandarizado)

#Los graficamos.
points(pima_indians_diabetes_nuevas_estandarizado_pca[, 1:2], cex = 0.9, pch = 19, col = "black")

#Como podemos clasificarlos?

#Idea! Podemos usar los vecinos más cercanos al punto para clasificar la nueva medición.
#A los puntos que usamos para aprender el patron, se los llama "datos de entrenamiento", y se dice que entrenamos un "modelo". Dejamos que la computadora
#encuentre los patrones en nuestros datos que nosotros no podemos ver.
#A los puntos que no usamos para entrenar, los llamamos "datos de validacion" y nos sirven para ver cuan bien funciona nuestro modelo.
#K Nearest Neighbors - K vecinos más cercanos
#Buscamos los K vecinos más próximos a la nueva medición.
#Cuantos vecinos tendriamos que usar?
#Usamos la funcion knn de la libreria class
library(class)

clasificacion <- knn(train = pima_indians_diabetes_estandarizado, test = pima_indians_diabetes_nuevas_estandarizado, k = 3, cl = pima_indians_diabetes$Class)
#Comparemos con lo anterior
clasificacion
pima_indians_diabetes_nuevas$Class
#Detecta solo 2 de las 5

clasificacion <- knn(train = pima_indians_diabetes_estandarizado, test = pima_indians_diabetes_nuevas_estandarizado, k = 7, cl = pima_indians_diabetes$Class)
#Comparemos con lo anterior
clasificacion
pima_indians_diabetes_nuevas$Class
#Detecta 3 de las 5

#Al porcentaje de verdaderos positivos o de positivos correctos lo llamamos sensibilidad
3/5

#Al porcentaje de falsos positivos lo llamamos 1-especificidad
#En este caso, tenemos 2 falsos positivos de nuestros 5 positivos
2/5