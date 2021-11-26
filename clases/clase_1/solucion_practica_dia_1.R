#################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biologíca Computacional
#                 Fundación Instituto Leloir
#                        Marzo 2021
#            Práctica Elementos básicos de R
#################################################################
#1) Asignale a la variable x el valor 2 y a la variable y el valor 10. Ahora asignale a la variable w el valor x+y y a la variable z el valor x == y (observá que tiene dos signos = y no sólo uno, ¿Por qué?). ¿De qué clase son estas dos últimas variable?. Ayuda: la asignación de una variable se realiza con el operador “<-”, así por ejemplo para asignar el valor “Hola Mundo” a la variable Saludo debo tipear Saludo<-”Hola Mundo”. Además, tipeá en la consola ?class
x <- 2
print(x)
y <- 10
print(y)
w <- x + y
print(w)
z <- x == y
print(z)

#2) Creá una variable que se llame “GFP” y asignale el valor 509. ¿Qué clase de dato es? Ahora convierte la variable “GFP” en “character”. Ayuda:  tipeá en la consola ?as.character
GFP <- 509
class(GFP)
GFP <- as.character(GFP)
class(GFP)

#3) Asignale a la variable GFP el valor “verde” e intenta convertirla en la clase numeric ¿Qué ocurre?
GFP <- "verde"  
as.numeric(GFP)

#4) Crea un vector que se llame “r1” que contenga 5 números aleatorios elegidos por vos entre el 0 y el 100 Ayuda: ?c.
r1 <- c(25, 52, 23, 76, 45)

#5) Con la misma operación que en el ejercicio anterior crea un nuevo vector r2, ahora realiza las siguientes operaciones: r1+r2, r1*r2, r1/r2. ¿Cómo realiza R estas operaciones?
r2 <-  c(3, 86, 24, 46, 54)
r1 + r2
r1 * r2
r1 / r2

#6) Intenten ordenar de mayor a menor y de menor a mayor el vector r1. ¿De qué largo es el vector r1?¿Cómo podría agregarle un elemento al vector r1? Ayuda: ?sort,  ?length
sort(r1)
length(r1)
nuevo_elemento <- 37
r1 <- c(r1, nuevo_elemento)
length(r1)
r1
#Otra forma, sabiendo que r1 mide 6 ahora, le agregamos uno subseteando un nuevo elemento 7
r1[7] <- 56
r1

#7) Creá dos vectores. Uno llamado fp que contenga los siguientes datos: "Sirius", "CFP", "GFP", "Citrine", y otro llamado nm que contenga los siguientes valores: 424, 476, 509, 528. A partir de estos dos vectores crea una matriz m_fp y un dataframe que se llame df_fp. Comprueba de qué clase es cada objeto. ¿Qué tienen de diferente? Ayuda: ?cbind y ?data.frame
fp <- c("Sirius", "CFP", "GFP", "Citrine")
nm <- c(424, 476, 509, 528)
m_fp <- cbind(fp, nm)
class(m_fp)
m_fp

df_fp <- data.frame(fp, nm)
class(df_fp)
df_fp

#8) Construí un vector con todos los números del 1 al 9. Ayuda: comienzo:fin
1:9

#9) Construí un vector con todos los números impares del 1 al 9. Ayuda: ?seq
seq(from = 1, to = 9, by = 2)

#10) Crea una matriz m de 3x2 (3 filas y 2 columnas) que contenga en todas las posiciones el valor 0. Ayuda: ?matrix
m <- matrix(0, nrow = 3, ncol = 2)
m

#11) Crea una matriz m de 3x3 que contenga en la posición [1, 1] el número 1, en la [1, 2] el número 2 y así hasta el número 9 en la posición [3, 3]. Los elementos de una matriz son [fila, columna]. Ayuda: ?matrix y ejercicio 8). 
m <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE)
m

#12) A la matriz del punto anterior cambiale el valor de la primera fila y columna por “Esto es un string” con el siguiente código: m[1, 1] <- “Esto es un string” (¿Podrías explicar cómo funciona esto?). Imprimir el resultado en pantalla, ¿Qué ocurrió con el resto de los valores de la matriz? ¿Por qué?
m[1, 1] <- "Esto es un string"  
m

#13) A partir de un vector “Co2” con los siguientes elementos: “Bajo”, “Medio”, “Medio”, "Bajo", "Bajo" y “Alto”; crea un objeto de tipo factor. ¿Qué niveles (o levels) tiene? Ahora cambia el primer valor del objeto por “Medio” de la siguiente forma Co2[1] <- “Medio” ¿Cambiaron los niveles del objeto Co2? Ahora si quiero cambiar el primer valor por “Muy bajo”, ¿Qué ocurre? ¿Por qué? Ayuda: ?factor, ?levels
Co2 <- factor(c("Bajo", "Medio", "Medio", "Bajo", "Bajo", "Alto"))
Co2
levels(Co2)
Co2[1] <- "Medio"
Co2
Co2[1] <- "Muy bajo"

#Subsetting

#14) Generen dos listas a partir de las siguientes sintaxis: nombres1 <- list(c("Juana", "Pedro", "Camila")) y nombres2 <- list("Juana", "Pedro", "Camila"). ¿Cuántos elementos tienen cada una de ellas? Para cada caso intenten imprimir en pantalla solamente el nombre Camila
nombres1 <- list(c("Juana", "Pedro", "Camila")) 
nombres1
nombres2 <- list("Juana", "Pedro", "Camila")
nombres2

#A partir del paquete dataset generen un data.frame “air” que contiene datos climáticos de la ciudad de Nueva York, usando el siguiente comando: air <- datasets::airquality.
#Cargamos el dataset
air <- datasets::airquality

#15) Imprime en pantalla solamente las primeras filas de air y luego las últimas. Ayuda: ?head, ?tail
head(air)
tail(air)

#16) Imprime en pantalla todos los valores correspondientes con la Temperatura registrada y luego solamente el valor de Temperatura que se registró el tercer día.
air$Temp #Todas
air$Temp[3] #Solo el 3er día

#17) Seleccioná todas las filas de air del mes de mayo.
mayo <- air[air$Month == 5, ]
mayo

#18) ¿En qué día hubo menor radiación solar? ¿De cuánto fue esa radiación? Ayuda: ?which.min y ?min. Al correr el comando min, observamos que el resultado devuelto es NA. ¿Qué significa? ¿Por qué es el mínimo?. Probar nuevamente con min pero pasando el parámetro na.rm = TRUE, es decir, min(vector, na.rm=TRUE). ¿Qué obtenemos ahora?
min(air$Solar.R)
min(air$Solar.R, na.rm = T)
air[which.min(air$Solar.R), ]

#19) ¿Cuál fue la temperatura el 27 de agosto? Ayuda: ?which, & (este símbolo se llama ampersand y es equivalente a “y”. Permite concatenar dos condiciones, por ejemplo, quiero que el día sea el 27 y el mes sea agosto. Solo aquellos registros que cumplan ambas condiciones van a ser incluidos.)
air[which(air$Month == 8 & air$Day == 27), ]

#20) ¿Cuál fue la temperatura los meses de agosto y septiembre? Ayuda: ?which, | (este símbolo se llama pipe o tubería y es equivalente a “o”. Permite concatenar dos condiciones, por ejemplo, quiero que el mes sea agosto o el mes sea septiembre. Solo aquellos registros que cumplan alguna de las dos condiciones van a ser incluidos.)
air[air$Month == 8 | air$Month == 9, ] #¿Por qué pude usar tanto which como no usarlo?

#21) Seleccioná todas las filas de air del mes de mayo cuya radiación solar sea mayor a 150.
mayo[mayo$Solar.R > 150, ]

#22) La temperatura en el data.frame está expresada en grados Fahrenheit, generá una nueva columna que se llame grados_c con las temperaturas en grados Celsius. Ayuda: °C = (°F - 32)/1.8
#Podemos hacerlo con una función o hacerlo directamente
convertir <- function(f){
  return((f - 32)/1.8)
}
temp_c <- convertir(air$Temp)
air <- data.frame(air, temp_c)

#Lo hacemos directamente (la columna ya existe)
air$temp_c <- (air$Temp - 32)/1.8

#23) Genera un nuevo data.frame llamado “calor” que contenga la información de los días calurosos, por ejemplo, en los que hizo más de 30°C. Luego, utilizando la función table contá cuantos días calurosos hubo en cada mes. ¿Qué mes es el que tuvo mayor cantidad de días calurosos?. Ayuda: La función table permite contar casos. 
calor <- air[air$temp_c > 30, ]
head(calor)
table(calor$Month)
#Agosto (el mes 8) es el mes que tuvo mayor cantidad de días calurosos


