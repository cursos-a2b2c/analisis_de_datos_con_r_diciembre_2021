sueno <- datasets::sleep
shapiro.test(sueno$extra[sueno$group == 1])
shapiro.test(sueno$extra[sueno$group == 2])

bartlett.test(list(sueno$extra[sueno$group == 1], sueno$extra[sueno$group == 2]))

t.test(sueno$extra[sueno$group == 1], sueno$extra[sueno$group == 2])

cd4 <- boot::cd4
shapiro.test(cd4$baseline)
shapiro.test(cd4$oneyear)
bartlett.test(list(cd4$baseline, cd4$oneyear))
t.test(cd4$baseline, cd4$oneyear, paired = T)

datasets::InsectSprays

bartlett.test(list(iris$Sepal.Length[iris$Species == "setosa"], iris$Sepal.Length[iris$Species == "versicolor"]))
t.test(iris$Petal.Length[iris$Species == "setosa"], mu = 1.5, var.equal = F)
t.test(iris$Sepal.Length[iris$Species == "versicolor"], iris$Sepal.Length[iris$Species == "virginica"])

boxplot(Sepal.Length ~ Species, data = iris)
boxplot(Petal.Length ~ Species, data = iris)
boxplot(Sepal.Width ~ Species, data = iris)
t.test(iris$Sepal.Length[iris$Species == "versicolor"], iris$Sepal.Length[iris$Species == "virginica"])
boxplot(Petal.Width ~ Species, data = iris)

tita<-as.data.frame(Titanic)
table(titanic_train$Pclass, titanic_train$Survived)
chisq.test(titanic_train$Pclass, titanic_train$Survived)
table(titanic_train$Sex, titanic_train$Survived)
chisq.test(titanic_train$Sex, titanic_train$Survived)
chisq.test(titanic_train$Sex, titanic_train$Pclass)
