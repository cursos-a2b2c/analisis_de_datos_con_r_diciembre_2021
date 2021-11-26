#--------------------------------------------------------------------------------------------------------------#
#----------------------------------ANOVA y Regresion Lineal----------------------------------------------------#
#--------------------------------------------------------------------------------------------------------------#

#ANOVA.

#Comencemos usando datos de juguete...
#Tenemos tres grupos de datos normalmente distribuidos, cada uno generado con su propia media.

n_grupos = 3
m = c(10, 7, 21)#Pueden ser grupos de distinto tamano.
medias = c(0, 4, 2)#Podemos definir las medias.
var = c(10, 10.1, 9.9)#Pueden tener varianzas distintas, pero no por mucho.

#Generamos los grupos.
grupos = list(X=rnorm(n = m[1], mean = medias[1], sd = sqrt(var[1])), 
              Y=rnorm(n = m[2], mean = medias[2], sd = sqrt(var[2])), 
              Z=rnorm(n = m[3], mean = medias[3], sd = sqrt(var[3])))

#Como se va a ver esto?
boxplot(grupos)

#Primero vamos a hacer el ANOVA "a mano", para que podamos entender cómo es que funciona y luego utilizaremos 
#la función `aov()` con el cual podemos hacer todo el análisis en una sola línea.

#Primero calculamos la varianza que tiene la media entre los grupos.
media_grupal = lapply(FUN=mean, grupos)#Calculo la media interna de cada grupo.
media_total = mean(unlist(grupos))#Calculo la media total sin distinguir grupos.
var_entre_grupos = sum(m * (unlist(media_grupal) - media_total) ** 2)#Calculo la varianza de las medias de los grupos respecto a la total, pesando por el tamaÃ±o de cada grupo.
entre_grupos = var_entre_grupos / (n_grupos - 1)#Divido eso por el numero de grupos menos uno.

#Ahora calculamos la varianza interna de cada uno de los grupos, sumandolas.
dif_media = unlist(grupos) - c(rep(media_grupal[[1]], m[1]), rep(media_grupal[[2]], m[2]), rep(media_grupal[[3]], m[3]))#Calculo la diferencia de cada muestra a la media de su grupo.
var_interna_grupos = sum(dif_media ** 2)
inter_grupos = var_interna_grupos / (sum(m) - n_grupos)#Divido eso por el numero total de observaciones menos el numero de grupos.

F_stat = entre_grupos / inter_grupos#El numero F resume toda esta informacion.
pf(F_stat, n_grupos - 1, sum(m) - n_grupos)#Calculamos la probabilidad de que sean distintas las medias.

#O, en vez de hacer todo eso, podemos simplemente calcular todo esto con aov().
datos = data.frame(grupo=c(rep('X', length(grupos[[1]])), rep('Y', length(grupos[[2]])), rep('Z', length(grupos[[3]]))), muestra=c(grupos$X, grupos$Y, grupos$Z))
grupo_aov = aov(muestra ~ grupo, data=datos)
summary(grupo_aov)#Esto dara Pr(>F), la probabilidad de la hipotesis nula (o sea, de que NO sean distintas las medias).


#Vamos a probar con algunos datos de verdad...
antropo_pob <- read.csv("antropologia_poblaciones.csv")

#Exploramos los datos
str(antropo_pob)
table(antropo_pob)

#Algunos gráficos para entender la relación que hay entre nuestras variables
plot(x = antropo_pob$height, y = antropo_pob$weight, xlab = "Height", ylab = "Weight")

#Separando por sexo
plot(x = antropo_pob$height[antropo_pob$sex == "F"], y = antropo_pob$weight[antropo_pob$sex == "F"], 
     xlab = "Height", ylab = "Weight", col = "green")
points(x = antropo_pob$height[antropo_pob$sex == "M"], y = antropo_pob$weight[antropo_pob$sex == "M"], col = "orange")
legend("topleft", fill = c("green", "orange"), legend = c("F", "M"))


#Relacion de las variables con la edad
plot(x = antropo_pob$age, y = antropo_pob$height, xlab = "Age", ylab = "Height")
abline(v = 20, col = "red")


#Queremos saber si existe alguna diferencia significativa de altura entre las dos poblaciones
#Nos vamos a quedar con los datos cuya edad sea mayor a 20 años

antropo_mayores <- antropo_pob[antropo_pob$age > 20, ]

boxplot(antropo_mayores$height ~ antropo_mayores$poblacion, xlab = "Poblacion", "Altura")

#Nuestra variable esta normalmente distribuida?
shapiro.test(antropo_mayores$height)

#Hagamos el ANOVA
anova_antro <- aov(antropo_mayores$height ~ antropo_mayores$poblacion * antropo_mayores$sex)

summary(anova_antro)


#REGRESION LINEAL.

#Generamos una relacion lineal dada.
a = 0.5#La pendiente.
b = 1.0#La ordenada al origen.

x = rnorm(1000, mean=0, sd=4)#Genero mil puntos en x.
y = a * x + b + rnorm(1000, mean=0, sd=1)#Calculo sus puntos correspondientes en y, con cierto ruido.

plot(x, y)#Grafico los puntos.

fit = lm(y ~ x)#Ajusto una regresion lineal.
sum.fit <- summary.lm(fit)#Visualizo un resumen del ajuste.

abline(fit[1], fit[0], col='red', lwd=2)#Dibujo una linea roja donde corresponde el ajuste.
text(x = 6, y = 0, label = paste("R2:", round(sum.fit$r.squared, 3)))


#Volvemos a nuestro ejemplo: queremos ver si hay una relación lineal entre la altura y la edad.
#Como habíamos visto, la relacion entre las dos variables cambia a los 20 años
plot(x = antropo_pob$age, y = antropo_pob$height, xlab = "Age", ylab = "Height")
abline(v = 20, col = "gray", lty = 2)



antropo_menores <- antropo_pob[antropo_pob$age < 20, ]
antropo_mayores <- antropo_pob[antropo_pob$age > 20, ]

fit_menores = lm(antropo_menores$height ~ antropo_menores$age)
summary(fit_menores)
abline(fit_menores[1], fit_menores[0], col='blue', lwd=2)


fit_mayores = lm(antropo_mayores$height ~ antropo_mayores$age)
summary(fit_mayores)
abline(fit_mayores[1], fit_mayores[0], col='red', lwd=2)


#Es la mejor opcion que teniamos? Que otros modelos podriamos haber realizado?