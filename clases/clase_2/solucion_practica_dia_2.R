#################################################################
#              Curso de análisis de datos con R
#Asociación Argentina de Bioinformática y Biologíca Computacional
#                 Fundación Instituto Leloir
#                        Marzo 2021
#         Práctica Elementos básicos de R continuación
#################################################################

#1) Imprimir en pantalla los números del 1 al 100 utilizando un for (¿cómo lo harían sin un for?).
for(i in 1:100){
  print(i)
}

#sin un for, simplemente print(1:100) o 1:100 o seq(from = 1, to = 100, by = 1)

#2) Sumar todos los números del 1 al 100 usando un for e imprimir los resultados parciales en pantalla. Ayuda: usar una variable suma que empieza en 0 afuera del for e ir llenándola dentro del for de esta manera: suma <- suma + i. A una variable que se usa de esa forma se la llama contador o acumulador.
suma <- 0
for (i in 1:100){
  suma <- suma + i
  print(suma)
}

#3) Elevar un número x una cantidad de veces n e imprimir el resultado. Por ejemplo, 3 elevado a la 4 es 3*3*3*3. Ayuda: se puede usar un acumulador como en el caso anterior.
total <- 1 #¿Por qué tengo que empezar en 1 en lugar de 0?
for(i in 1:4){
  total <- total * 3
}
print(total)

#4) Usando seq, generar un vector xs desde -pi a pi con saltos de 0.1. Luego, imprimir cada elemento x de xs junto con el sin(x) usando un for (¿cómo lo harían sin un for?). Tanto la variable pi como la función sin vienen precargados en R, alcanza con llamarlos. Ayuda:  ?paste para pegar cadenas de caracteres o cadenas y números, por ejemplo, print(paste(‘A’, 1)) imprime A 1.
xs <- seq(from = -pi, to = pi, by = 0.1)
for(x in xs){
  print(paste(x, sin(x)))
}

#Sin el for
paste(xs, sin(xs))

#5) Imprimir en pantalla los números pares entre el 1 y el 100 usando un for y un if. Ayuda: Para preguntar si un número es par, hay que realizar la operación “módulo 2”, que consiste en dividir por dos y ver si el resto de la división es 0. La operación “módulo” se realiza utilizando %%. Por ejemplo: 3 %% 2 devuelve 1, mientras que 4 %% 2 devuelve 0. Entonces, un número n es par si n %% 2 == 0.
for(i in 1:100){
  if(i %% 2 == 0){
    print(i)
  }
}

#6) Imprimir en pantalla los números del 1 al 100 seguidos de una letra “p” si son pares o de una “i” si son impares, utilizando un for y un if else.
for(i in 1:100){
  if(i %% 2 == 0){
    print(paste(i, "p"))
  }else{
    print(paste(i, "i"))
  }
}

#7) Imprimir en pantalla los números del -10 al 10 seguidos de una letra “p” si son pares (sin incuir al cero, aunque el cero cumple con la definición de par), de una “i” si son impares y de una “o” si es el cero, utilizando un for, un if, else if y else. 
for(i in -10:10){
  if(i == 0){
    print(paste(i, "o"))
  }else if (i %% 2 == 0){
    print(paste(i, "p"))
  }else{
    print(paste(i, "i"))
  }
}

#8) Intermedio: generar una matriz de 5x10. Colocar dentro de cada posición la suma de la fila y de la columna de esa posición. Por ejemplo, en la fila 3, columna 4, iría el número 7, al igual que en la fila 4, columna 3. Ayuda: Utilizá un for adentro de otro for. En uno te movés en las filas y en otro en las columnas.
m <- matrix(0, nrow = 5, ncol = 10) #Genero una matriz vacía o de 0s
for(i in 1:5){ #Me muevo en las filas
  for(j in 1:10){ #Parado en la fila i me muevo en las columnas
    m[i, j] <- i + j
  }
}
m #La imprimo para ver si lo hice bien

#9) Muy difícil: juego de adivinar el número. En este juego, la computadora elige un número al azar entre 1 y 100 y el jugador tiene que adivinarlo en menos de 20 oportunidades. Con cada intento de adivinarlo por parte del jugador, la computadora le indica si el número elegido es mayor o menor al ingresado.
numero_al_azar <- sample(1:100, 1) #Le pido a la compu que saque un numero al azar entre 1 y 100
for(intento in 1:20){ #Le doy al jugador un máximo de 20 intentos
  numero_usuario <- as.numeric(readline(paste("Ingresá un número (intento", intento, "): "))) #Le pido al jugador que ingrese un número
  if(numero_usuario == numero_al_azar){ #Gano! Tengo que salir del for, no quiero seguir preguntándole
    print("¡Crack, ganaste!")
    print(paste("Usaste", intento, "intentos"))
    break #Con break salimos del for antes de que terminen los 20 intentos
  }else if (numero_usuario > numero_al_azar){ #Eligió un número muy grande
    print("Elegiste un número muy grande")
  }else{ #Eligió un número muy chico
    print("Elegiste un número muy chico") 
  }
  if(intento == 20){ #Si intento vale 20 es porque llegó hasta el final, hay que decirle que perdió
    print("Uh, le pifiaste 20 veces, perdiste!!")
  }
}


#10) Importar el dataset “antropometria”. 
antropometria <- read.csv("antropometria.csv") #Acá tenés que completar con la ruta completa a donde tengas el archivo
#¿Cuántas filas y columnas contiene el data.frame? 
nrow(antropometria)
ncol(antropometria)
#¿Qué tipo de datos tiene cada columna? 
str(antropometria)
head(antropometria)  
tail(antropometria)
#¿Existen elementos con NA? ¿Cuántos?
completos <- complete.cases(antropometria)
completos #Imprimo el vector para ver qué tipo de datos me devuelve complete.cases. (spoiler, es un vector de TRUE y FALSE donde TRUE es una fila que no tiene NA, es decir, es un caso completo)
table(completos) #Hay 28 elementos con NA

#Generar una nueva tabla removiendo todas las filas con algún NA
antropometria_filtrado <- antropometria[completos, ]
#Veamos si hay algún na. Podemos hacerlo de varias maneras, con complete.cases como antes, o usando is.na y any. is.na se fija cada elemento del data frame
#y any se fija si alguno de esos es TRUE
is.na(antropometria_filtrado) #A diferencia de complete.cases que devuelve TRUE o FALSE por fila, is.na se fija celda por celda y devuelve por celda.
any(is.na(antropometria_filtrado)) #Any se fija si alguna de todas las celdas es TRUE. Si todas son FALSE, devuelve FALSE

#y exportarla a un nuevo csv llamado antropometria_filtrado.csv
write.csv(antropometria_filtrado, file = "antropometria_filtrado.csv", quote = F, row.names = F) #write.csv tiene muchas opciones, estas son las más comunes para mantener compatibilidad con otros programas, por ejemplo, excel

