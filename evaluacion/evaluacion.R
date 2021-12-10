#############################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biología Computacional
#                         Diciembre 2021
#                      Evaluación del curso
#
#Para organizarnos mejor:
#Entregar un script de R cuyo nombre sea apellido_nombre.R con la resolución
#de los ejercicios a andresrabinovich@gmail.com y con el subject "Evaluación R".
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

#Problema 1: Abrir el dataset nestlings.csv en RStudio y describirlo exhaustivamente para comprender el contenido del mismo.
#ej: cantidad de observaciones, cantidad de atributos, tipo de atributos, etc.

#Problema 2: Calcular cuántas observaciones hay de Control y cuántas de IGF.¿Los datos están
#balanceados (cantidades similares de observaciones de ambos tipos)?

#Problema 3: Calcular media, mediana, desvío estándar y distancia inter cuartil para GPx_activity
#y sample_weight. Realizar esta descripción para cada tratamiento por separado.
#¿Las medidas de centralidad son similares para cada tratamiento y para cada atributo? ¿Y las de
#dispersión?

#Problema 4: Realizar un boxplot y un histograma de GPx_activity para cada tratamiento. 
#Retirar los datos que son muy extremos (decidan ustedes qué consideran muy extremos)
#(Ayuda: no hay que retirar solamente el valor de GPx_activity, hay que retirar toda la fila del valor atípico)
#Por ejemplo, supongamos que la observación 100 tiene un GPx_activity atípico, entonces:
#nestlingsSinAtipicos <- nestlings[-100, ] #Es decir, sacamos toda la fila.
#A partir de acá, seguir trabajando con el nuevo dataframe con los datos atípicos removidos.

#Problema 5: Visualizar en un mismo gráfico de boxplot el sample_weight para cada tratamiento. 
#Retirar los datos que son muy extremos (decidan ustedes qué consideran muy extremos)
#Ayuda, para decirle a un boxplot que grafique una variable 'y' pero separando por la variable 'x', pueden usar
#formulas: boxplot(y ~ x).
#A partir de acá, seguir trabajando con el nuevo dataframe con los datos atípicos removidos.

#Problema 6: Para poder determinar si existe una diferencia significativa en el peso entre estos los grupos, control y tratamiento, realizar un T test.
#¿A qué conclusión se llega?
#Realizar previamente todos los tests necesarios para garantizar que es válido aplicar un t test.
#Ayuda: para el test de homogeneidad de varianza, bartlett.test, es necesario previamente
#agrupar las observaciones en una lista. Si control es una variable con los pesos de los polluelos
#de control y tratamiento es una variable con los pesos de los polluelos tratados, generar una lista de la siguiente
#manera: total <- list(control, tratamiento) y utilizar esa lista en el test.

#Problema 7: Realizar un scatter plot entre GPx activity y sample weight ¿Puede observar algún tipo de relación entre estas dos variables?

#Problema 8: Realizar un ajuste lineal entre GPx activity y sample weight y graficarlo.
#Ayuda: funcion lm

#Problema 9: Cargar el dataset vinos.RData.
#El mismo consiste en los resultados del analisis quimico de vinos cultivados en la misma region de Italia pero en tres cultivares diferentes.
#Realizar un PCA y graficarlo. ¿Cuánta variabilidad explican las primeras 2 componentes? ¿Será una buena representación del dataset un gráfico 2d con estas primeras dos componentes?

#Problema 10: #Cual es el vino más cercano al 121? Y al 137?
#Clusterizar con kmeans. Elegir un k con el criterio del codo.
#Realizar un silohuette y decidir si todos los datos quedaron agrupados en grupos compactos.
#Graficar el PCA coloreando por clusters.