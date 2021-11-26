#############################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biología Computacional
#                 Fundación Instituto Leloir
#                         Marzo 2021
#                      Evaluación del curso
#
#Para organizarnos mejor: 
#Entregar un script de R cuyo nombre sea apellido_nombre.R con la resolución
#de los ejercicios a esc.bioinf.a2b2c.leloir@gmail.com y con el subject "Evaluación R".
#Pueden realizarnos consultas a ese mismo email con el subject "Consultas Evaluación R". 
#############################################################################

##################################################################################################
#ABSTRACT
#Los animales durante su desarrollo se enfrentan con problemas en la asignación de recursos cuando reciben cantidades subóptimas
#de los mismos en ambientes naturales muy variables.
#Estos problemas se manifiestan posiblemente en el intercambio (trade-off) entre crecimiento y mantenimiento somático.
#Sin embargo, la medida en que se manifiesta este intercambio sigue siendo una pregunta abierta.
#Para intentar responder a esta pregunta, se inyectó a pichones de papamoscas cerrojillo (Ficedula hypoleuca) con 
#IGF-1 (insulin-like growth factor 1) para observar como IGF-1 afecta el despliegue de una respuesta antioxidante mediada por 
#la enzima glutathione peroxidasa (GPx). 
#Se midió la actividad de GPx y el peso en pichones tratados y en pichones no tratados (control)

#################Consignas########################
#Cada ejercicio otorga 1 punto.
#Los bonus "crack de R" otorgan un punto.

#Problema 1: Abrir el dataset nestlings.csv en RStudio y describirlo exhaustivamente para comprender el contenido del mismo.
#ej: cantidad de observaciones, cantidad de atributos, tipo de atributos, etc. 

setwd("~/curso/evaluacion/datos") #Elegimos el directorio de trabajo

nestlings <-  read.csv("nestlings.csv") #Leemos el dataset

class(nestlings) #Tipo de objeto
nrow(nestlings) #Cantidad de filas u observaciones del dataset
ncol(nestlings) #Cantidad de columnas o atributos
str(nestlings) #Visualizamos la estructura del dataset
colnames(nestlings) #Los nombres de los atributos
View(nestlings) #Lo abrimos tipo excel para darle una mirada general rápida
#El dataset contiene 66 observaciones con 6 atributos cada una. En particular son de interés manipulation (chr), 
#que contiene el tratamiento realizado, GPx_activity (num) que contiene la actividad de la enzima y sample_weight (num) que contiene el peso de cada pichón

#Problema 2: Calcular cuántas observaciones hay de Control y cuántas de IGF.¿Los datos están
#balanceados (cantidades similares de observaciones de ambos tipos)?
table(nestlings$manipulation) #Usamos table para contar cuantos elementos hay de Control-injected y cuantos de IGF-1-injected. Toda esa información está en manipulation
#Hay 33 datos de cada tratamiento, están balanceados

#Problema 3: Calcular media, mediana, desvío estándar y distancia inter cuartil para GPx_activity
#y sample_weight. Realizar esta descripción para cada tratamiento por separado.
#¿Las medidas de centralidad son similares para cada tratamiento y para cada atributo? ¿Y las de 
#dispersión? 
#GPx_activity
GPx_activityControlInjected <- nestlings$GPx_activity[nestlings$manipulation=="Control-injected"]#Podemos subsetear y guardar en una nueva variable 
GPx_activityIGF1injected    <- nestlings$GPx_activity[nestlings$manipulation=="IGF-1-injected"]#Podemos subsetear y guardar en una nueva variable 
mean(GPx_activityControlInjected) #Calculamos media
mean(GPx_activityIGF1injected) #Calculamos media
median(GPx_activityControlInjected) #Calculamos mediana
median(GPx_activityIGF1injected) #Calculamos mediana
sd(GPx_activityControlInjected) #Calculamos desvío estándar
sd(GPx_activityIGF1injected) #Calculamos desvío estándar
IQR(GPx_activityControlInjected) #Distancia intercuartil
IQR(GPx_activityIGF1injected) #Distancia intercuartil
summary(GPx_activityControlInjected) #Un resumen rápido de todo
summary(GPx_activityIGF1injected) #Un resumen rápido de todo

#Vemos que la media y mediana para control y para IGF son diferentes. ¿Serán significativamente diferentes? ¿Cómo podríamos saberlo?
#Lo mismo ocurre para el desvío y el IQR. 

#sample_weight
mean(sample_weightControlInjected) #Calculamos media
mean(sample_weightIGF1injected) #Calculamos media
median(sample_weightControlInjected) #Calculamos mediana
median(sample_weightIGF1injected) #Calculamos mediana
sd(sample_weightControlInjected) #Calculamos desvío estándar
sd(sample_weightIGF1injected) #Calculamos desvío estándar
IQR(sample_weightControlInjected) #Distancia intercuartil
IQR(sample_weightIGF1injected) #Distancia intercuartil
summary(sample_weightControlInjected) #Un resumen rápido de todo
summary(sample_weightIGF1injected) #Un resumen rápido de todo

#Nuevamente vemos que la media y mediana para control y para IGF son diferentes. ¿Serán significativamente diferentes? ¿Cómo podríamos saberlo?
#Lo mismo ocurre para el desvío y el IQR.


#Problema 4: Realizar un boxplot y un histograma de GPx_activity para cada tratamiento. ¿Existen datos atípicos?
#En caso de existir, retirar esas observaciones y mostrar el criterio utilizado para decidirlo.
#(Ayuda: no hay que retirar solamente el valor de GPx_activity, hay que retirar toda la fila del valor atípico)
#Por ejemplo, supongamos que la observación 100 tiene un GPx_activity atípico, entonces:
#nestlingsSinAtipicos <- nestlings[-100, ] #Es decir, sacamos toda la fila.
#Bonus crack de R: Realizar esos 4 gráficos en un mismo layout, con el boxplot en forma horizontal por debajo
#del histograma correspondiente a cada tratamiento. 
#A partir de acá, seguir trabajando con el nuevo dataframe con los datos atípicos removidos.
matriz_layout <- matrix(1:4, nrow=2, ncol=2, byrow = F) #Armamos el layout
layout(matriz_layout)

#Usamos las variables que ya habíamos definido antes para graficar, pero para sacar los valores atípicos, vamos a querer hacerlo en el dataframe completo
hist(GPx_activityControlInjected, main = "GPx Control", breaks = 10) #Le ponemos 10 breaks para visualizar mejor la distribución
boxplot(GPx_activityControlInjected, horizontal = T)
hist(GPx_activityIGF1injected, main = "GPx Tratamiento", breaks = 10)
boxplot(GPx_activityIGF1injected, horizontal = T)
#Tratamiento no parece muy normal. ¿Será significativamente no normal? ¿Cómo podríamos saberlo?
layout(1) #Volvemos al layout original

#Saquemos outliers con el criterio de IQR
iqr    <- IQR(GPx_activityControlInjected)
minimo <- quantile(GPx_activityControlInjected, 0.25) - 1.5*iqr #Calculamos el mínimo para control injected que consideramos no outlier
maximo <- quantile(GPx_activityControlInjected, 0.75) + 1.5*iqr #Calculamos el máximo para control injected que consideramos no outlier
#Pedimos en simultaneo que sean control injected y la actividad sea menor al mínimo o mayor al máximo. Eso cubre todo. Lo hacemos en el original y guardamos
#La fila correspondiente. 
a_remover <- which(nestlings$manipulation == "Control-injected" & (nestlings$GPx_activity < minimo | nestlings$GPx_activity > maximo))

#Saquemos outliers con el criterio de IQR
iqr    <- IQR(GPx_activityIGF1injected)
minimo <- quantile(GPx_activityIGF1injected, 0.25) - 1.5*iqr #Calculamos el mínimo para control injected que consideramos no outlier
maximo <- quantile(GPx_activityIGF1injected, 0.75) + 1.5*iqr #Calculamos el máximo para control injected que consideramos no outlier
#Pedimos en simultaneo que sean IGF injected y la actividad sea menor al mínimo o mayor al máximo. Eso cubre todo. Lo hacemos en el original.
#Se lo agregamos a los que ya queríamos remover
a_remover <- c(a_remover, which(nestlings$manipulation == "IGF-1-injected" & (nestlings$GPx_activity < minimo | nestlings$GPx_activity > maximo)))
a_remover

#Listo, tenemos todas las filas que queremos remover, las sacamos
nestlingsSinOutliers <- nestlings[-a_remover, ]

#Seguramente si volvemos a graficar volvamos a encontrar outliers, en general alcanza con remover los primeros que suelen ser los más gruesos.


#Problema 5: Visualizar en un mismo gráfico de boxplot el sample_weight para cada tratamiento. ¿Existen datos atípicos? 
#En caso de existir, retirar esas observaciones y mostrar el criterio utilizado para decidirlo.
#Ayuda, para decirle a un boxplot que grafique una variable 'y' pero separando por la variable 'x', pueden usar
#formulas: boxplot(y ~ x).
#A partir de acá, seguir trabajando con el nuevo dataframe con los datos atípicos removidos.

#Tenemos que trabajar con el dataset sin outliers de GPx
boxplot(nestlingsSinOutliers$sample_weight ~ nestlingsSinOutliers$manipulation)
#Pareciera haber outliers en control
#Saquemos outliers con el criterio de IQR
sample_weightControlInjected <- nestlingsSinOutliers$sample_weight[nestlingsSinOutliers$manipulation == "Control-injected"]
iqr    <- IQR(sample_weightControlInjected)
minimo <- quantile(sample_weightControlInjected, 0.25) - 1.5*iqr #Calculamos el mínimo para control injected que consideramos no outlier
maximo <- quantile(sample_weightControlInjected, 0.75) + 1.5*iqr #Calculamos el máximo para control injected que consideramos no outlier
#Pedimos en simultaneo que sean IGF injected y la actividad sea menor al mínimo o mayor al máximo. Eso cubre todo. Lo hacemos en el original.
#Se lo agregamos a los que ya queríamos remover
a_remover <- which(nestlingsSinOutliers$manipulation == "Control-injected" & (nestlingsSinOutliers$sample_weight < minimo | nestlingsSinOutliers$sample_weight > maximo))
a_remover

#Los sacamos
nestlingsSinOutliers <- nestlingsSinOutliers[-a_remover, ]

#A partir de acá, uno debería trabajar con nestlingsSinOutliers, pero por la dificultad que tiene, podían trabajar con el original, no había problema.
#En datos reales, sin embargo, deberían trabajar sinOutliers.

#Problema 6: Para poder determinar si existe una diferencia significativa en el peso entre estos los grupos, control y tratamiento, realizar un T test. 
#¿A qué conclusión se llega?
#Realizar previamente todos los tests necesarios para garantizar que es válido aplicar un t test.
#Ayuda: para el test de homogeneidad de varianza, bartlett.test, es necesario previamente
#agrupar las observaciones en una lista. Si control es una variable con los pesos de los polluelos
#de control y tratamiento es una variable con los pesos de los polluelos tratados, generar una lista de la siguiente
#manera: total <- list(control, tratamiento) y utilizar esa lista en el test.

#Hacemos los tests de homocedasticidad, normalidad y t
total <-list(nestlingsSinOutliers$sample_weight[nestlingsSinOutliers$manipulation=="Control-injected"], nestlingsSinOutliers$sample_weight[nestlingsSinOutliers$manipulation=="IGF-1-injected"])
bartlett.test(total)
#Rechazamos homocedasticidad por pvalue < 0.05
#normalidad de cada variable
#Test de Shapiro
shapiro.test(nestlingsSinOutliers$sample_weight[nestlingsSinOutliers$manipulation=="Control-injected"])
shapiro.test(nestlingsSinOutliers$sample_weight[nestlingsSinOutliers$manipulation=="IGF-1-injected"])
# No rechazamos normalidad
# t test pero hay que especificar que las dos varianzas son distintas
t.test(nestlingsSinOutliers$sample_weight[nestlingsSinOutliers$manipulation=="Control-injected"], nestlingsSinOutliers$sample_weight[nestlingsSinOutliers$manipulation=="IGF-1-injected"], var.equal = FALSE)
# Rechazamos la hipótesis nula, por lo que que existen diferencias significativas entre control y tratamiento.

#Problema 7: Realizar un scatter plot entre GPx activity y sample weight ¿Puede observar algún tipo de relación entre estas dos variables?
plot(nestlingsSinOutliers$GPx_activity, nestlingsSinOutliers$sample_weight)
#Pareciera existir una relación lineal creciente entre ambas

#Problema 9: realizar una regresión lineal donde sample_weight dependa de GPx_activity. ¿Es un buen ajuste? ¿Por qué?
#Ayuda: la función lm permite realizar un ajuste lineal donde 'y' depende de 'x' usando fórmulas: lm(y ~ x, data = misdatos).
#Esta función devuelve un objeto de tipo lm que puede ser pasado a la función summary para que devuelva todos los estadísticos de 
#interés (R2, coeficientes, pvalues, etc): summary(lm(y ~ x, data = misdatos))
#Bonus crack de R: Realizar la misma regresión lineal pero agregando como variable el tratamiento.
#¿Mejora la estimación o el tratamiento no parece afectar la relación?
#Ayuda: una fórmula dónde 'y' depende de dos variables se escribe como y ~ x + z (o y ~ x*z en caso de interacción entre las variables)
ajuste <- lm(sample_weight ~ GPx_activity, nestlingsSinOutliers) #Ajustemos
plot(nestlingsSinOutliers$GPx_activity, nestlingsSinOutliers$sample_weight)
abline(ajuste, col = "red")
summary(ajuste) #Veamos cómo da
#Si bien observamos una relación entre ambas en el scatter plot y el pvalue de GPx_activity nos confirma esa relación, el ajuste no parece ser muy bueno, con un R2 de 0.19. 

#Agreguemos otra variable y después la interacción
ajuste <- lm(sample_weight ~ GPx_activity + manipulation, nestlingsSinOutliers) #Ajustemos
summary(ajuste) #Veamos cómo da
#Practicamente no hay cambios, el pvalue de manipulation parece indicar que no suma nada haberlo agregado

ajuste <- lm(sample_weight ~ GPx_activity*manipulation, nestlingsSinOutliers) #Ajustemos con interacción
summary(ajuste) #Veamos cómo da
#Practicamente no hay cambios, el pvalue de manipulation y de la interacción parece indicar que no suma nada haberlo agregado
#el modelo inicial termina siendo el más adecuado

