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


#otro ejemplo pero usando un for dentro de la funci�n...

sumar_hasta <- function(n, m){
  suma_i <- 0
  for(i in n:m){
    suma_i = suma_i + i
  }
  return(suma_i)
}

sumar_hasta(76, 100)
sumar_hasta(517)

