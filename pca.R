data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

g <- g + scale_x_continuous(limits = c(-1, 1))
print(g)
g <- g + scale_x_continuous(limits = c(-2, 2))
print(g)
# HW4 可以互動X,Y座標

# g1, X:PC1, Y:PC2
g1 <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species
               ,ellipse = TRUE
               #, circle = TRUE
               , varname.size = 4
         #,labels = c("PC1", "PC2", "PC3")
         ) 
g1 <- g1 + scale_color_discrete(name = '')
g1 <- g1 + geom_point(aes(x = ir.pca$x[,1], y = ir.pca$x[,2], color = ir.species))
g1 <- g1 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g1)


# g2, X:PC1, Y:PC3
g2 <- ggbiplot(ir.pca
               ,choices = c(1, 3)
               , obs.scale = 1, var.scale = 1, groups = ir.species
               ,ellipse = TRUE#, circle = TRUE
               , varname.size = 4
               #,labels = c("PC1", "PC2", "PC3")
) 
g2 <- g2 + scale_color_discrete(name = '')
#g2 <- g2 + geom_point(aes(x = ir.pca$x[,3], y = ir.pca$x[,1], color = ir.species))
g2 <- g2 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g2)

# g3, X:PC1, Y:PC4
g3 <- ggbiplot(ir.pca
               ,choices = c(1, 4)
               , obs.scale = 1, var.scale = 1, groups = ir.species
               ,ellipse = TRUE#, circle = TRUE
               , varname.size = 4
               #,labels = c("PC1", "PC2", "PC3")
) 
g3 <- g3 + scale_color_discrete(name = '') 
g3 <- g3 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g3)

# g4, X:PC2, Y:PC1
g4 <- ggbiplot(ir.pca
               ,choices = c(2, 1)
               , obs.scale = 1, var.scale = 1, groups = ir.species
               ,ellipse = TRUE#, circle = TRUE
               , varname.size = 4
               #,labels = c("PC1", "PC2", "PC3")
) 
g4 <- g4 + scale_color_discrete(name = '')
g4 <- g4 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g4)

# g5, X:PC2, Y:PC3
g5 <- ggbiplot(ir.pca
               ,choices = c(2, 3)
               , obs.scale = 1, var.scale = 1, groups = ir.species
               ,ellipse = TRUE#, circle = TRUE
               , varname.size = 4
               #,labels = c("PC1", "PC2", "PC3")
) 
g5 <- g5 + scale_color_discrete(name = '')
g5 <- g5 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g5)


# Corresponding Analysis
library(ca)
set.seed(5220)
model <- kmeans(log.ir, centers = 3)
table(ir.species, model$cluster)
ca_model <- ca(table(ir.species, model$cluster), nd = 3)
plot(ca_model, arrows = c(TRUE, FALSE))

library(ca)
set.seed(5220)
model <- kmeans(iris[, 1:4], centers = 3)
table(iris$Species, model$cluster)
ca_model <- ca(table(iris$Species, model$cluster), nd = 3)
plot(ca_model, arrows = c(TRUE, FALSE))

