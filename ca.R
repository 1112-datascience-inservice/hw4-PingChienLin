library(MASS)
set.seed(5220)
iris2 <- scale(iris[, 1:4])
model <- kmeans(iris[, 1:4], centers = 3)

table(iris$Species, model$cluster)
camodel <- corresp(iris$Species, model$cluster, nf = 2)

print(camodel)
plot(camodel)

tmp <- data.frame(spec = iris$Species, cl = model$cluster)
camodel2 <- corresp(~spec + cl, data = tmp, nf = 2)
camodel2

library(ca)
cmodel3 <- ca::ca(table(iris$Species, model$cluster), nd = 4)
cmodel3
summary(cmodel3)
plot(cmodel3)

set.seed(1001)
iris2 <- scale(iris[, 1:4])
model <- kmeans(iris[, 1:4], centers = 5)


model <- kmeans(iris[, 1:4], centers = 3)
table(iris$Species, model$cluster)
cmodel5 <- ca(table(iris$Species, model$cluster), nd = 2)
cmodel5
summary(cmodel5)
plot(cmodel5)
plot(cmodel5, arrows = c(TRUE, FALSE))
