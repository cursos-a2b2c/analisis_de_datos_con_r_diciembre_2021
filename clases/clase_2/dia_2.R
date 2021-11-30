#--------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------Limpieza de Datos--------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#Antes de comenzar a realizar el análisis estadísticos de nuestros datos, siempre tenemos que asegurarnos que las
#tablas con las que trabajamos estan bien. Esto quiere decir que no haya ningun dato faltante, filas duplicadas ni datos que no tengan sentido

#Vamos a abrir la tabla camas-criticas.csv
camas <- read.csv("camas_criticas_ba.csv")

#Vamos a ver un poco como son nuestros datos
head(camas)
dim(camas)

#Lo primero que podemos ver es si hay algun dato faltante en la tabla, para eso vamos a buscar si hay NAs

#primera opcion...
is.na(camas)
is.na(camas$cantidad_camas_criticas)
any(is.na(camas$cantidad_camas_criticas))

#segunda opcion...
complete.cases(camas)

#vamos a eliminar las filas que contienen NAs
camas <- camas[complete.cases(camas), ]
nrow(camas)

#Ahora vamos a ver si hay filas duplicadas en nuestra tabla y, en caso de que las haya, las vamos a eliminar
duplicated(camas)
any(duplicated(camas))

camas <- camas[!duplicated(camas), ]
nrow(camas)
#Por ultimo, es importante chequear que los datos que estan en nuestra tabla tienen sentido...
#Por ejemplo, vamos a ver los valores que aparecen en la variable "cantidad_camas_criticas"

#summary devuelve un resumen de varias caracteristicas estadisticas de nuestros datos
summary(camas$cantidad_camas_criticas)

#El minimo valor que figura es -1, eso tiene sentido? Vamos a eliminar las filas que tengan un valor menor a 0

camas <- camas[camas$cantidad_camas_criticas < 0, ]

#Por ultimo, vamos a guardar la tabla luego de la limpieza que hicimos 

write.csv(camas2017, file = "camas_criticas_ba_ok.csv")



#--------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------Uso de Paquetes--------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#Los paquetes en `R` son conjuntos de funciones o de datos que expanden las posibilidades que tenemos de analizar nuestros datos. 
#Para poder usarlos, en primer lugar tendremos que descargar a nuestra computadora el paquete, lo que puede hacerse con la función `install.packages()`. 
#Una vez instalado, nuestro paquete quedará guardado en nuestra Biblioteca de paquetes. En el momento en el que nosotres querramos utilizar las funciones que nos provee un determinado paquete, tendremos que cargarlo en memoria para poder tenerlo a disposición. Esto se realiza con la función `library()`.
#En el siguiente ejemplo, instalamos y cargamos en memoria uno de los paquetes más utilizados para realizar bellos gráficos: `ggplot2`.

install.packages("ggplot2")
library(ggplot2)

#`Bioconductor` es un repositorio de paquetes relacionados con el análisis de datos genómicos que contiene una gran cantidad de paquetes de gran ayuda 
#a la hora de analizar datos de biología molecular en general.

#A continuación, vamos a instalar `Bioconductor` y, luego, uno de los paquetes que nos ofrece llamado `Biostrings`, que es de gran ayuda a la hora de 
#analizar secuencias biológicas, como ADN, ARN y proteínas.


install.packages("BiocManager")

BiocManager::install("Biostrings")

#--------------------------------------------------------------------------------------------------------------#
#-------------------------------------------Exportacion e Importacion------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#Vamos a subsetear la tabla camas para quedarnos solamente con los datos correspondientes al ano 2017
# y despues vamos a querer exportarla en distintos formatos.

camas2017 <- camas[camas$anio == 2017, ]

#Exportamos como un archivo delimitado por comas (csv)
write.csv(camas2017, file = "camas_criticas_ba_2017.csv", row.names = FALSE)

#Expotamos como una tabla separada por tabs
write.table(camas2017, file = "camas_criticas_ba_2017.txt", row.names = FALSE)

#Usando el paquete openxlsx tambien podemos exportarlo como un archivo excel
library(openxlsx)
write.xlsx(camas2017, file = "camas_criticas_ba_2017.xlsx", row.names = FALSE)


#Por otro lado, se pueden guardar objetos de R para poder usarlos más tarde en una nueva sesión. A continuación guardamos en un archivo 
#con la extensión `.RData`:

save(camas, camas2017, file = "camas_criticas_ba_ok.RData")

#Vamos a borrar los objetos que acabamos de guardar para ver como los importamos de nuevo a nuestra sesiÃ³n

rm(camas, camas2017)  #este comando elimina de nuestra sesion estos dos objetos

load("camas_criticas_ba_ok.RData") #ahora los volvemos a importar desde el archivo plantas_filtradas.RData

#Como ya vimos en el ejemplo de las camas criticas, facilmente podemos importar a nuestra sesion de R datos
camas <- read.csv("camas_criticas_ba_2017.csv")
camas <- read.table("camas_criticas_ba_2017.txt")
camas <- read.xlsx("camas_criticas_ba_2017.xlsx")

#Con la funcion dir() podemos ver listados todos los archivo presentes en la carpeta en la que estamos trabajando
dir()

#--------------------------------------------------------------------------------------------------------------#
#---------------------------------------Ejemplo Integrador-----------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#Ahora vamos a ver un ejemplo que va a integrar varias de las cosas que estuvimos viendo hasta acá,
#utilizando las funciones que nos da el paquete Biostring y DECIPHER.

library(Biostrings)

#Creamos un string que representa una secuencia de ADN de una longitud de 50pb

seq1 <- "GAACCAAGACACTGTATGACCACGTTTTGCACGAATGCTTTGGATCTACG"
class(seq1)

#Comencemos a utilizar las funciones que nos da `Biostrings`. A la secuencia que creamos en el paso anterior la tenemos que convertir en un nuevo objeto 
#que pueda ser entendido por las funciones que usemos de ahora en más como una secuencia de ADN:

dna1 <- DNAString(seq1)
print(dna1)
class(dna1)

#Algunas de las cosas que podremos hacer con nuestra secuencia son: conocer la secuencia inversa complementaria (con la función `reverseComplement()`), 
#saber qué proteína se puede codificar a partir de nuestra secuencia de ADN (con la función `translate()`), o calcular la frecuencia de cada nucleótido 
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


#Finalmente, vamos a poder hacer un alineamiento múltiple de estas secuencias. Para realizarlo vamos a tener que utilizar un nuevo paquete que se llama `DECIPHER`. 
#Por lo que, al igual que antes, vamos a tener que instalar el paquete y cargarlo en memoria.

library(DECIPHER)

#La funcion `AlignSeqs` es la que me va a permitir realizar el MSA 

msa <- AlignSeqs(EEF2_seqs)
print(msa)

#La misma función nos permite cambiar un monton de parámetros que van a determinar el resultado que obtendremos. En el siguiente ejemplo, 
#hago que en el alineamiento sea "más fácil" partir las secuencias:

msa2 <- AlignSeqs(EEF2_seqs, gapOpening = 0)
print(msa2)

#Ya tenemos nuestro MSA, ahora vamos a querer exportar los resultados. 
#Como lo hacemos? Tenemos varias opciones...

#1)Podemos guardar el MSA en un archivo .txt
write.table(msa, file = "EEF2_seqs_align.txt", quote = F, row.names = F, col.names = F)

#2)Podemos agregar las secuencias ya alineadas como una nueva columna en la tabla original y exportar todo junto

fungi_EEF2$align <- as.character(msa)#Noten que tengo que cambiar el tipo de variable que tenia el msa
write.xlsx(fungi_EEF2, file = "fungi_EEF2_align.xlsx")

#3)Tambien podriamos buscar de guardarlo en algun formato que sea mas amigable a 
#la hora de querer trabajar con ese MSA en otros programas, como el formato FASTA.
#Pero no tengo idea de como exportar datos en formato FASTA desde R, acaso Google tendra la respuesta?...


#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

