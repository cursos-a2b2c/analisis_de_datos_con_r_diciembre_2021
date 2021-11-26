iris
boxplot(iris[, 1:4])
boxplot(iris$Sepal.Length ~ iris$Species)
setosa <- iris[iris$Species == "setosa", ]
versicolor <- iris[iris$Species == "versicolor", ]
virginica <- iris[iris$Species == "virginica", ]
summary(iris$Sepal.Length)
summary(iris$Sepal.Width)
plot(setosa$Sepal.Length, setosa$Sepal.Width, main = "Iris", 
     xlab = "Sepal Length", ylab="Sepal Width", xlim=c(4, 8), ylim=c(1, 5),
     col = "black", pch = 1)
points(versicolor$Sepal.Length, versicolor$Sepal.Width, col = "black", pch = 2)
points(virginica$Sepal.Length, virginica$Sepal.Width, col = "black", pch = 3)
legend("topleft", legend = c("Setosa", "Versicolor", "Virginica"), col = "black", pch=c(1, 2, 3), bty = "n")

plot(iris$Sepal.Length, iris$Sepal.Width, main = "Iris", 
     xlab = "Sepal Length", ylab="Sepal Width", xlim=c(4, 8), ylim=c(1, 5),
     col = iris$Species)
legend("topleft", legend = c("Setosa", "Versicolor", "Virginica"), col = 1:3, pch=1, bty = "n")

abline(a = -3, b = 1, col = "blue")
abline(v = 6, col = "blue")
abline(h = 3, col = "blue")
pairs(iris)
head(iris[, 1:4])
head(t(iris[, 1:4]))
matplot(t(iris[, 1:4]), type="l", col = iris$Species)
matplot(t(iris[, 1:4]), type="l", col = "black")
summary(iris$Sepal.Length)
summary(iris$Sepal.Width)
hist(iris$Sepal.Length, xlim = c(1.9, 8), ylim = c(0, 1), freq = F)
hist(iris$Sepal.Width, add = TRUE, freq = F, col = "red")
