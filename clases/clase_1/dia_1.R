
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------Estadistica con R-------------------------------------------#
#--------------------------------------------D√≠a 1: Elementos b√°sicos de R-------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#En el dia de hoy vamos a comenzar viendo los elementos basicos de la sintaxis de R
#Entre las muchas cosas que podemos con R, lo podemos usar como si fuera una calculadora.

2+10
315/7
89*3
sqrt(99225)
21^pi
1 == 2 
1 == 1
1 != 2

#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------Tipos de Datos----------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#


#En R todo el tiempo vamos a estar trabajando con datos, por lo cual una de las primeras cosas que debemos hacer es
#crear los distintos "objetos" que van a contener nuestros datos. Por ejemplo podemos crear un objeto que se llame 
#"cursos" y que contenga el n√∫mero de cursos que se van a dar en la Escuelita de Bioinform√°tica este a√±o.

cursos <- 3
print(cursos)
#En R hay varios tipos de objetos: numeric, integer, character, logical, ... Para saber de que clase es un objetos 
#podemos usar la funcion class.

?class
class(cursos)

#con los objetos en R se pueden realizar una gran cantidad de operaciones. Por ejemplo, imaginen que en la segunda 
#parte del ano damos 2 cursos mas, ahora podriaamos crear un segundo objeto cursos2.

cursos2 <- 2

#Si lo que queremos es a partir de los dos objetos anteriores saber cuantos cursos de Bioinformatica se dieron en el
#ano podemos simplemente sumar los objetos "cursos" y "cursos2"

cursos + cursos2
cursos_totales <- cursos + cursos2 

#Por otro lado, podemos convertir los objetos en R de una clase a otra, aunque con algunas excepciones....

as.integer(cursos_totales)
as.character(cursos_totales)
as.logical(cursos_totales)

#CÛmo representar la ausencia de un dato? NA y NaN
NA
NA == NA #øNA es igual a NA?

0/0
Inf - Inf



#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------Estructuras de Datos----------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#


#En R podremos encontrar objetos que contengan m√°s de un dato como puede ser una tabla, una matriz o un vector.

cursos <- c("Linux", "Python", "R")
horas  <- c(8, 12, 20)

#De la misma manera que podemos realizar calculos con numeros tambien lo podemos hacer con vectores

horas + 2
2*horas
horas +c(1, 2, 3, 4)

#Los vectores solo aceptan que todos los elementos que sean guardados en los mismos sean de la misma clase.

cs <- c("Linux", "Python", "R", 2) #¬øQue ocurrio con el 2?

#En cambio, las listas nos permiten contener elementos de diferentes clases en el mismo objeto.

cs <- list("Linux", "Python", "R", 2)
class(cs[1])
class(cs[[1]]) #øDe que clase es el primer elemento de la lista?
class(cs[[4]]) #øDe que clase es el cuarto elemento de la lista?

#En R tambien encontramos estructuras de datos que son de dos dimensionen que nos van a permitir trabajar con
#tablas (a las que vamos a llamar data.frames) y matrices.

#A partir de los dos vectores que creamos anteriormente vamos a generar un data.frame

carga_horaria <- data.frame(cursos, horas)

class(carga_horaria)
str(carga_horaria)       #la funcion str nos permite analizar la estructura de los datos
dim(carga_horaria)       #dim devuelve las dimensiones del data.frame
colnames(carga_horaria)  #colnames nos devuelve los nombres de las columnas, de forma similar existe rownames

#Tambien podemos agregar mas columnas o filas a un dataframe
?cbind
?rbind

profes <- c("eli", "javi", "andres")

carga_horaria <- cbind(carga_horaria, profes) #fijense que estoy pisando la variable vieja agregando una nueva columna
                                              #de forma similar puedo agregar filas con rbind 


#Las matrices de forma similar a los vectores, solo van a permitir contener un solo tipo de dato
?matrix
?seq

v      <- seq(1, 9)

m1     <- matrix(data = v, nrow = 3, ncol = 3, byrow = TRUE)
m2     <- matrix(data = v, nrow = 3, ncol = 3, byrow = F)


#--------------------------------------------------------------------------------------------------------------#
#----------------------------------------------Subsetting------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#Vamos a trabajar con un dataset que se encuentra dentro del paquete 'datasets' que se llama PlantGrowth, que 
#contiene los datos de peso seco de plantas crecidas bajo distintas condiciones.

plantas <- datasets::PlantGrowth #al dataset lo nombro como "plantas"
head(plantas)         #nos muestra las primeras filas del dataset
colnames(plantas)
dim(plantas)
str(plantas)

class(plantas$group)  #factor es una clase de escructura de datos similar a un vector que se reserva para asignar 
                      #variables cualitativas
levels(plantas$group) #levels son los distintos valores que puede tomar los elementos dentro del objeto factor, en 
                      #este caso ser√≠an los distintos tratamiento a los que se sometieron las plantas.

#CÛmo quedarnos con todos los datos de una columna?
plantas$weight       #imprimir en pantalla la primera columna del dataset
plantas[, 1]         #otra forma de hacer lo mismo
plantas[, "weight"]  #y otra mas...

#Y si nos queremos quedar con el dato de una celda?
plantas[15, 1]  #imprimo en pantalla el valor que toma el peso (primera columna) en la planta n√∫mero 15
                #el primer valor entre corchetes indica a qu√© fila quiero acceder, y el segundo valor a qu√© columna


#Como haria si quiero que me muestre en pantalla solamente las filas que corresponden al tratamiento 2?

plantas$group == "trt2"
which(plantas$group == "trt2")

plantas[which(plantas$group == "trt2"), ] #con which le estoy preguntando cuales son las filas en las que en la columna
                                          #"group" el valor sea igual a "trt2"
plantas[plantas$group == "trt2", ]

#Y si quiero que me muestre las plantas que pesan mas que 5.5?

plantas[which(plantas$weight > 5.5), ]

#ahora vamos a hacer lo mismo pero con dos condiciones: voy a querer imprimir en pantalla las plantas que pertenecen
#al tratamiento 2 y que pesan mas de 5.5

plantas[which(plantas$weight > 5.5 & plantas$group == "trt2"), ]

#TambiÈn me puedo quedar solamente con los valores extremos
plantas[plantas$weight > 5.5 | plantas$weight < 4.5, ]

#Puedo guardar la tabla subseteada como un nuevo objeto
plantas2 <- plantas[which(plantas$weight > 5.5 & plantas$group == "trt2"), ]



#Ahora vamos a ver como se subsetea una LISTA. En las listas, al poder contener objetos muy variados, hay que tener 
#ciertas consideraciones a la hora de subsetear

tecnicas_BioMol <- list("Southern", "Western" , c("Northern", "RNASeq", "RTPCR"))

length(tecnicas_BioMol)     #cuantos elementos tiene la lista? por que?

tecnicas_BioMol[1]          #quiero imprimir el primer elemento de la lista...
class(tecnicas_BioMol[1])   #pero...

tecnicas_BioMol[[1]]        #cual es la diferencias con el paso anterior?
class(tecnicas_BioMol[[1]])

tecnicas_BioMol[[3]]        #que ocurre cuando tengo un vector dentro de una lista?
tecnicas_BioMol[[3]][2]     #para llegar al elemento que se encuentra en la 2da posicion en la lista


#tambien puedo nombrar a cada elemento de la lista y usarlos para subsetear
tecnicas_BioMol <- list(DNA = "Southern", Proteinas = "Western" , RNA = c("Northern", "RNASeq", "RTPCR"))

tecnicas_BioMol$RNA


#Ahora que ya sabemos quÈ tipos de datos nos podemos encontrar en R y comÛ acceder a la informacion 
#que contienen, en la clase que viene vamos a comenzar a ver cÛmo analizarlos.