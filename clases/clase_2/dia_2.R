#--------------------------------------------------------------------------------------------------------------#
#----------------------------------Estructuras de Control------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#vamos a usar un ejemplo: queremos saber cuanto vale la suma de los primeros 100 números naturales
#para eso tenemos que tener una variable que vaya acumulando el resultado de la suma en cada iteración (n),y otra 
#variable que vaya recorriendo los números desde 1 hasta 100 (i)

n <- 0
for (i in 1:100){
  n=n+i
}

print(n)

#Hay alguna forma de hacer lo mismo pero sin utilizar un for?
sum(1:100)

#queremos saber si un numero es par o impar, entonces debemos usar la estructura if - else

numero = 7
if (numero%%2 == 0){
  print("El numero es par")
}else{
  print("El numero es impar")
}

#esta es otra forma pero imprimiendo de otra manera los resultados (mas fachero)
?paste

numero = 571
if (numero%%2 == 0){
  print(paste("El numero", numero, "es par"))
}else{
  print(paste("El numero", numero, "es impar"))
}


#Ahora queremos guardar en una variable la suma de los números pares desde 1 a 100 y en otra variable la suma de 
#los numeros impares. En este caso vamos a necesitar un "if" que me permita saber si un número es par o impar
#la expresion %% me permite saber el resto de la division, por ej. 48%%2 va a devolver 0, que 48 es divisible 
#por 2

pares   = 0
impares = 0

for (i in 1:100){
  if (i%%2 == 0){
    pares = pares + i
  } else{
    impares = impares +i
  }
}

print(pares)
print(impares)

#Hay alguna forma de hacer lo mismo pero sin utilizar un for ni un if?

pares = sum(seq(2, 100, by = 2))
impares = sum(seq(1, 100, by = 2))

#--------------------------------------------------------------------------------------------------------------#
#----------------------------------------------Crear Funciones-------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#primer ejemplo sencillo para definir una funcion en R
sumar <- function(numero1, numero2){
  print(numero1+numero2)
}

sumar(2, -7)

sumar <- function(x, y){
  juancito <- x+y
  return(juancito)
}

total <- sumar(2, 3)

print(total)

total <- sumar(total, 10)


#otro ejemplo pero usando un for dentro de la funci?n...

sumar_hasta <- function(n, m){
  suma_i <- 0
  for(i in n:m){
    suma_i = suma_i + i
  }
  return(suma_i)
}

sumar_hasta(76, 100)
sumar_hasta(517)


#--------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------Uso de Paquetes--------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#Los paquetes en `R` son conjuntos de funciones o de datos que expanden las posibilidades que tenemos de analizar nuestros datos. 
#Para poder usarlos, en primer lugar tendremos que descargar a nuestra computadora el paquete, lo que puede hacerse con la funci?n `install.packages()`. 
#Una vez instalado, nuestro paquete quedar? guardado en nuestra Biblioteca de paquetes. En el momento en el que nosotres querramos utilizar las funciones que nos provee un determinado paquete, tendremos que cargarlo en memoria para poder tenerlo a disposici?n. Esto se realiza con la funci?n `library()`.
#En el siguiente ejemplo, instalamos y cargamos en memoria uno de los paquetes m?s utilizados para realizar bellos gr?ficos: `ggplot2`.

install.packages("ggplot2")
library(ggplot2)

#`Bioconductor` es un repositorio de paquetes relacionados con el an?lisis de datos gen?micos que contiene una gran cantidad de paquetes de gran ayuda 
#a la hora de analizar datos de biolog?a molecular en general.

#A continuaci?n, vamos a instalar `Bioconductor` y, luego, uno de los paquetes que nos ofrece llamado `Biostrings`, que es de gran ayuda a la hora de 
#analizar secuencias biol?gicas, como ADN, ARN y prote?nas.


install.packages("BiocManager")

BiocManager::install("Biostrings")

#--------------------------------------------------------------------------------------------------------------#
#-------------------------------------------Exportacion e Importacion------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#Vamos a exportar la tabla del experimento de crecimiento de plantas filtrada por tipo de tratamiento (trt2) y por 
#peso (plantas con pesos extremos)
plantas <- PlantGrowth
plantas_filtradas <- plantas[plantas$weight > 5.5 | plantas$weight < 4.5, ]

write.csv(x = plantas_filtradas, file = "plantas_filtradas.csv")

#Por otro lado, se pueden guardar objetos de R para poder usarlos m?s tarde en una nueva sesi?n. A continuaci?n guardamos en un archivo 
#con la extensi?n `.RData`, la tabla original de plantas y la filtrada:

save(plantas, plantas_filtradas, file = "plantitas.RData")

#vamos a borrar los objetos que acabamos de guardar para ver como los importamos de nuevo a nuestra sesión

rm(plantas, plantas_filtradas)  #este comando elimina de nuestra sesion estos dos objetos

load("plantitas.RData") #ahora los volvemos a importar desde el archivo plantas_filtradas.RData

#Para importar datos hacia una sesi?n de `R` tambi?n es muy sencillo y depender? del tipo de archivo que sea: .csv, .txt, .fasta, etc. 
#Veamos un ejemplo cargando la tabla que acabamos de exportar:

tabla_plantas <- read.csv("plantas_filtradas.csv")

dir()

#--------------------------------------------------------------------------------------------------------------#
#---------------------------------------Ejemplo Integrador-----------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#Ahora vamos a ver un ejemplo que va a integrar varias de las cosas que estuvimos viendo hasta ac?,
#utilizando las funciones que nos da el paquete Biostring y DECIPHER.

library(Biostrings)

#Creamos un string que representa una secuencia de ADN de una longitud de 50pb

seq1 <- "GAACCAAGACACTGTATGACCACGTTTTGCACGAATGCTTTGGATCTACG"
class(seq1)

#Comencemos a utilizar las funciones que nos da `Biostrings`. A la secuencia que creamos en el paso anterior la tenemos que convertir en un nuevo objeto 
#que pueda ser entendido por las funciones que usemos de ahora en m?s como una secuencia de ADN:

dna1 <- DNAString(seq1)
print(dna1)
class(dna1)

#Algunas de las cosas que podremos hacer con nuestra secuencia son: conocer la secuencia inversa complementaria (con la funci?n `reverseComplement()`), 
#saber qu? prote?na se puede codificar a partir de nuestra secuencia de ADN (con la funci?n `translate()`), o calcular la frecuencia de cada nucle?tido 
#(con `alphabetFrequency()`) o, incluso, subsecuencias en mi secuencia (con `letterFrequency()`)

reverseComplement(dna1)
translate(dna1)
alphabetFrequency(dna1)
letterFrequency(dna1, letters = "GC", as.prob = T)

#Vamos con un ejemplo un poco mas complejo...

###El objetivo es realizar un Alineamiento Multiple de Secuencia (MSA) de una proteina llamada EEF2 en distintas especies de hongos

#Entre los archivos para la clase de hoy tienen uno llamado fungi_EEF2.csv (csv = comma separated values)
#Vamos a importarlo a nuestra sesion de R...

fungi_EEF2 <- read.csv("fungi_EEF2.csv")

#Vamos a explorar los datos
str(fungi_EEF2)
head(fungi_EEF2)

#De esta tabla me quiero quedar con las secuencias proteicas que estan en la tercer columna.
EEF2_seqs  <- fungi_EEF2$Secuencia

#Para poder trabajar con estas secuencias tengo que "convertirlas" en secuencias de proteinas (de forma analoga al ejemplo anterior)
#Ahora en vez de tener una secuencia de DNA tengo un conjunto de secuencias proteicas, por lo que la funcion que tenemos que usar es distinta...
EEF2_seqs <- AAStringSet(EEF2_seqs)
class(EEF2_seqs)

#Podemos hacer algunos de los analisis que habiamos hecho anteriormente
alphabetFrequency(EEF2_seqs)


#Finalmente, vamos a poder hacer un alineamiento m?ltiple de estas secuencias. Para realizarlo vamos a tener que utilizar un nuevo paquete que se llama `DECIPHER`. 
#Por lo que, al igual que antes, vamos a tener que instalar el paquete y cargarlo en memoria.

library(DECIPHER)

#La funcion `AlignSeqs` es la que me va a permitir realizar el MSA 

msa <- AlignSeqs(EEF2_seqs)
print(msa)

#La misma funci?n nos permite cambiar un monton de par?metros que van a determinar el resultado que obtendremos. En el siguiente ejemplo, 
#hago que en el alineamiento sea "m?s f?cil" partir las secuencias:

msa2 <- AlignSeqs(EEF2_seqs, gapOpening = 0)
print(msa2)

#Ya tenemos nuestro MSA, ahora vamos a querer exportar los resultados. 
#Como lo hacemos? Tenemos varias opciones...

#1)Podemos guardar el MSA en un archivo .txt
write.table(msa, file = "EEF2_seqs_align.txt", quote = F, row.names = F, col.names = F)

#2)Podemos agregar las secuencias ya alineadas como una nueva columna en la tabla original y exportar todo junto

fungi_EEF2$align <- as.character(msa)#Noten que tengo que cambiar el tipo de variable que tenia el msa
write.csv(fungi_EEF2, file = "fungi_EEF2_align.csv")

#3)Tambien podriamos buscar de guardarlo en algun formato que sea mas amigable a 
#la hora de querer trabajar con ese MSA en otros programas, como el formato FASTA.
#Pero no tengo idea de como exportar datos en formato FASTA desde R, acaso Google tendra la respuesta?...


##BONUS TRACK para los amantes de excel...

#Hay muchisimas formas de importar y exportar datos en formato de planilla de excel.
#En general, se requiere de alg?n paquete que nos ayude.

#Entre los archivos de la clase de hoy tienen la misma tabla que usamos antes pero en una planilla de Excel, vamos a importarla...
#Vamos a usar el siguiente paquete:

install.packages("openxlsx")
library(openxlsx)

#Y ahora si estamos listos para importar nuestra tabla a R.
fungi_EEF2_excel <- read.xlsx("fungi_EEF2.xlsx")
head(fungi_EEF2_excel)

#Tambien podemos exportar la tabla que creamos con el resultado del MSA
write.xlsx(fungi_EEF2, file = "fungi_EEF2_align.xlsx")


#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

