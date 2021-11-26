#--------------------------------------------------------------------------------------------------------------#
#----------------------------------Estructuras de Control------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#vamos a usar un ejemplo: queremos saber cuanto vale la suma de los primeros 100 n√∫meros naturales
#para eso tenemos que tener una variable que vaya acumulando el resultado de la suma en cada iteraci√≥n (n),y otra 
#variable que vaya recorriendo los n√∫meros desde 1 hasta 100 (i)

n=0
for (i in 1:100){
  n=n+i
}
print(n)

#Hay alguna forma de hacer lo mismo pero sin utilizar un for?
sum(1:100)

#queremos saber si un numero es par o impar, entonces debemos usar la estructura if - else

numero = 10
if (numero%%2 == 0){
  print("El numero es par")
}else{
  print("El numero es impar")
}

#esta es otra forma pero imprimiendo de otra manera los resultados (m√°s fachero)
?paste

numero = 10
if (numero%%2 == 0){
  print(paste("El numero", numero, "es par"))
}else{
  print(paste("El numero", numero, "es impar"))
}


#Ahora queremos guardar en una variable la suma de los n√∫meros pares desde 1 a 100 y en otra variable la suma de 
#los n√∫meros impares. En este caso vamos a necesitar un "if" que me permita saber si un n√∫mero es par o impar
#la expresion %% me permite saber el resto de la division, por ej. 48%%2 va a devolver 0, que 48 es divisible 
#por 2

pares   = 0
impares = 0

for (i in 1:100){
  if (i%%2 == 0)
    pares = pares + i
  else{
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

#primer ejemplo sencillo para definir una funci√≥n en R
sumar <- function(x, y){
  print(x+y)
}

sumar(2, 3)

sumar <- function(x, y){
  suma <- x+y
  return(suma)
}

total <- sumar(2, 3)
print(total)

total <- sumar(total, 10)


#otro ejemplo pero usando un for dentro de la funciÛn...

sumar_hasta <- function(n){
  suma_i <- 0
  for(i in 1:n){
    suma_i = suma_i + i
  }
  return(suma_i)
}

sumar_hasta(100)
sumar_hasta(1010)


#--------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------Uso de Paquetes--------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#Los paquetes en `R` son conjuntos de funciones o de datos que expanden las posibilidades que tenemos de analizar nuestros datos. 
#Para poder usarlos, en primer lugar tendremos que descargar a nuestra computadora el paquete, lo que puede hacerse con la funciÛn `install.packages()`. 
#Una vez instalado, nuestro paquete quedar· guardado en nuestra Biblioteca de paquetes. En el momento en el que nosotres querramos utilizar las funciones que nos provee un determinado paquete, tendremos que cargarlo en memoria para poder tenerlo a disposiciÛn. Esto se realiza con la funciÛn `library()`.
#En el siguiente ejemplo, instalamos y cargamos en memoria uno de los paquetes m·s utilizados para realizar bellos gr·ficos: `ggplot2`.

install.packages("ggplot2")
library(ggplot2)

#`Bioconductor` es un repositorio de paquetes relacionados con el an·lisis de datos genÛmicos que contiene una gran cantidad de paquetes de gran ayuda 
#a la hora de analizar datos de biologÌa molecular en general.

#A continuaciÛn, vamos a instalar `Bioconductor` y, luego, uno de los paquetes que nos ofrece llamado `Biostrings`, que es de gran ayuda a la hora de 
#analizar secuencias biolÛgicas, como ADN, ARN y proteÌnas.


install.packages("BiocManager")

BiocManager::install("Biostrings")

#---------------------------------------Ejemplo Integrador-----------------------------------#
#Ahora vamos a ver un ejemplo que va a integrar varias de las cosas que estuvimos viendo hasta ac·,
#utilizando las funciones que nos da el paquete Biostring.

library(Biostrings)

#Creamos una secuencia de ADN al azar de una longitud de 30pb. Para esto utilizamos la funciÛn `sample` para elegir 30 nucleÛtidos al azar, con esto 
#tendremos un vector con 30 elementos al cual vamos a querer colapsar en una ˙nica secuencia, lo que haremos con la funciÛn `paste`:

nucleotidos_azar <- sample(c("A","G", "T", "C"), 30, replace = T)
print(nucleotidos_azar)
seq1 <- paste(nucleotidos_azar, collapse = "")
print(seq1)


#Comencemos a utilizar las funciones que nos da `Biostrings`. A la secuencia que creamos en el paso anterior la tenemos que convertir en un nuevo objeto 
#que pueda ser entendido por las funciones que usemos de ahora en m·s como una secuencia de ADN:

dna1 <- DNAString(seq1)
print(dna1)
class(dna1)

#Algunas de las cosas que podremos hacer con nuestra secuencia son: conocer la secuencia inversa complementaria (con la funciÛn `reverseComplement()`), 
#saber quÈ proteÌna se puede codificar a partir de nuestra secuencia de ADN (con la funciÛn `translate()`), o calcular la frecuencia de cada nucleÛtido 
#(con `alphabetFrequency()`) o, incluso, subsecuencias en mi secuencia (con `letterFrequency()`)

reverseComplement(dna1)
translate(dna1)
alphabetFrequency(dna1)
letterFrequency(dna1, letters = "GC", as.prob = T)

#TambiÈn podemos trabajar con conjuntos de secuencias. En el siguiente ejemplo, vamos a crear 5 secuencias al azar y las vamos a poner en un vector utilizando un bucle `for`. 
#El objetivo final ser· realizar un alineamiento m˙ltiple entre estas secuencias.

secuencias <- c() #creamos un vector vacÌo en el cual vamos a ir incluyendo las secuencias

for(i in 1:5){ #i va a ser el indice de nuestro vector con el que vamos a ir agregando las secuencias
  nucleotidos_nuevos <- sample(c("A", "G", "T", "C"), size = 30, replace = T) #elijo al azar los nucleÛtidos que van a formar la secuencia
  seq_nueva <- paste(nucleotidos_nuevos, collapse = "") #colapso en una ˙nica secuencia el vector de nucleÛtidos
  secuencias[i] <- seq_nueva #agrego la secuencia nueva al vector de secuencias
}

print(secuencias)

#Ahora para poder trabajar con estas secuencias vamos tener que convertirlas en secuencias de ADN como las entienden las funciones de `Biostrings`. Noten que no es la misma 
#funciÛn que usamos antes, eso es porque ahora tenemos m·s de una secuencia.

dna_secuencias <- DNAStringSet(secuencias)

#Podemos hacer el mismo an·lisis que hicimos al principio con seq1

reverseComplement(dna_secuencias)
translate(dna_secuencias)
alphabetFrequency(dna_secuencias)
letterFrequency(dna_secuencias, letters = "GC", as.prob = T)

#Finalmente, vamos a poder hacer un alineamiento m˙ltiple de estas secuencias. Para realizarlo vamos a tener que utilizar un nuevo paquete que se llama `DECIPHER`. 
#Por lo que, al igual que antes, vamos a tener que instalar el paquete y cargarlo en memoria.

BiocManager::install("DECIPHER")
library(DECIPHER)

#La funcion `AlignSeqs` es la que me va a permitir realizar el alineamiento m˙ltiple 

msa <- AlignSeqs(dna_secuencias)
print(msa)

#La misma funciÛn nos permite cambiar un monton de par·metros que van a determinar el resultado que obtendremos. En el siguiente ejemplo, 
#hago que en el alineamiento sea "m·s f·cil" partir las secuencias:

msa2 <- AlignSeqs(dna_secuencias, gapOpening = -5)
print(msa2)



#--------------------------------------------------------------------------------------------------------------#
#-------------------------------------------Exportacion e Importacion------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#Vamos a exportar la tabla del experimento de crecimiento de plantas filtrada por tipo de tratamiento (trt2) y por 
#peso (plantas con pesos extremos)
plantas <- PlantGrowth
plantas_filtadas <- plantas[plantas$weight > 5.5 | plantas$weight < 4.5, ]

write.csv(x = plantas_filtradas, file = "plantas_filtradas.csv")

#Por otro lado, se pueden guardar objetos de R para poder usarlos m·s tarde en una nueva sesiÛn. A continuaciÛn guardamos en un archivo 
#con la extensiÛn `.RData`, los dos alineamientos m˙ltiples que generamos anteriormente:

save(msa, msa2, file = "alineamientos.RData")

#vamos a borrar los objetos que acabamos de guardar para ver como los importamos de nuevo a nuestra sesi√≥n

rm(msa, msa2)  #este comando eliminar√° de nuestra sesi√≥n estos dos objetos

load("alineamientos.RData") #ahora los volvemos a importar desde el archivo plantas_filtradas.RData

#Para importar datos hacia una sesiÛn de `R` tambiÈn es muy sensillo y depender· del tipo de archivo que sea: .csv, .txt, .fasta, etc. 
#Veamos un ejemplo cargando la tabla que acabamos de exportar:

tabla_plantas <- read.csv("plantas_filtradas.csv")
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

