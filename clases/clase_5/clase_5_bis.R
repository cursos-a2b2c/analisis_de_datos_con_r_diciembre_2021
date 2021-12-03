#################################################################
#              Curso de análisis de datos con R
#          Machine learning: aprendizaje supervisado
#################################################################

#Veamos datos de diabetes en mujeres mayores de 21 anios de ascendencia Pima residentes en Phoenix, Arizona.
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
pima_indians_diabetes <- read_csv("~/cursos/analisis_de_datos_con_r_diciembre_2021/clases/clase_5/pima-indians-diabetes.csv", col_names = T)

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
pima_indians_diabetes_nuevas <- read_csv("~/cursos/analisis_de_datos_con_r_diciembre_2021/clases/clase_5/pima-indians-diabetes-nuevas.csv", col_names = T)

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
#Se qeuivoca en 3

clasificacion <- knn(train = pima_indians_diabetes_estandarizado, test = pima_indians_diabetes_nuevas_estandarizado, k = 5, cl = pima_indians_diabetes$Class)
#Comparemos con lo anterior
clasificacion
pima_indians_diabetes_nuevas$Class
#Mejoro de k = 3

#A los aciertos de nuestro modelo lo llamamos accuracy o precision


#Veamos otro tipo de datos. Dataset trees
#This data set provides measurements of the diameter, height and volume of 
#timber in 31 felled black cherry trees. 
#Note that the diameter (in inches) is erroneously labelled Girth in the data.
#It is measured at 4 ft 6 in above the ground.

#Exploremos el dataset
View(trees)
colnames(trees)
head(trees)
nrow(trees)
plot(trees$Girth, trees$Volume)

#Cómo parece ser la relación entre el diámetro y el volumen?
#.
#.
#.
#R permite ajustar un modelo lineal a los datos usando un objeto de tipo "formula"
#Podemos hacer que dependa de todas las variables que querramos
Volume ~ Girth + Height

#E incluso usar interacción
Volume ~ Girth*Height

#Por ahora solo vamos a querer ver si Girth depende de Volume
formula_para_ajuste <- Volume ~ Girth
formula_para_ajuste

#Ajustemos un modelo lineal
ajuste <- lm(formula = formula_para_ajuste, data = trees)

#Veamos qué devuelve el ajuste? Que es cada cosa? Que significan?
ajuste
summary(ajuste)

#Grafiquemos todo junto
abline(ajuste, col="red")

#Ademas de modelar una relacion, podemos usar este modelo para encontrar el volumen de un arbol solamente midiendo su circunferencia 
#Medimos la circunferencia de un nuevo árbol, que volumen esperamos que tenga?
nuevo_arbol <- data.frame(Girth = 15)
volumen <- predict.lm(ajuste, nuevo_arbol)
points(nuevo_arbol$Girth[1], volumen, col="blue", pch=19)

