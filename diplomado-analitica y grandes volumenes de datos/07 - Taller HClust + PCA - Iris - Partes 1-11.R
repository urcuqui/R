
###########################################################################


####################################################
# 1. Análisis exploratorio
library(ggplot2)
library(caret)

data(iris)
str(iris)
summary(iris) 

modelo_estandar <- preProcess(iris[,1:4], method = c("center", "scale"))
iris2 <- predict(modelo_estandar, iris[,1:4])
iris2$Species <- iris$Species
str(iris2)
summary(iris2) 

####################################################
# 2. Clusterización jerárquica

#matriz de distancias
distancias <- dist(iris2[,1:4], method = "euclidean")
distancias

#Vamos a crear tres plots de los dendrogramas en una sola fila para compararlos
par(mfrow=c(1,5))

modelo_single <- hclust(distancias, method="single")
plot(modelo_single, labels=rownames(iris2), main="single", hang=-1)

modelo_complete <- hclust(distancias, method="complete")
plot(modelo_complete, labels=rownames(iris2), main="complete", hang=-1)

modelo_average <- hclust(distancias, method="average")
plot(modelo_average, labels=rownames(iris2), main="average", hang=-1)

modelo_centroid <- hclust(distancias, method="centroid")
plot(modelo_centroid, labels=rownames(iris2), main="centroid", hang=-1)

modelo_ward <- hclust(distancias, method="ward.D2")
plot(modelo_ward, labels=rownames(iris2), main="ward", hang=-1)

par(mfrow=c(1,1))
  
####################################################
# 3. Con single linkage
clustSingle <- cutree(modelo_single, k=3)
plot(modelo_single, labels=rownames(iris2), main="single", hang=-1)
rect.hclust(modelo_single, k=3)
iris2$clustSingle <- as.factor(clustSingle)
iris2

# Veamos como es la composición de cada cluster segUn la especie
for(i in 1:3) {
  print(paste("cluster", i))
  print(iris2[clustSingle==i, c("Species", "clustSingle")])
}

levels(iris2$clustSingle) <- levels(iris2$Species)
matSingle <- confusionMatrix(iris2$Species, iris2$clustSingle)
paste(matSingle$overall[1], matSingle$overall[2])
#Accuracy: 66%, Kappa: 49%

####################################################
# 4. Con complete linkage
clustComplete <- cutree(modelo_complete, k=3)
plot(modelo_complete, labels=rownames(iris2), main="complete", hang=-1)
rect.hclust(modelo_complete, k=3)
iris2$clustComplete <- as.factor(clustComplete)

# Veamos como es la composición de cada cluster segUn la especie
for(i in 1:3) {
  print(paste("cluster", i))
  print(iris2[clustComplete==i, c("Species", "clustComplete")])
}

levels(iris2$clustComplete) <- levels(iris2$Species)
matComplete <- confusionMatrix(iris2$Species, iris2$clustComplete)
paste(matComplete$overall[1], matComplete$overall[2])
#Accuracy: 78.7%, Kappa: 68%

####################################################
# 5. Con average linkage
clustAverage <- cutree(modelo_average, k=3)
plot(modelo_average, labels=rownames(iris2), main="average", hang=-1)
rect.hclust(modelo_average, k=3)
iris2$clustAverage <- as.factor(clustAverage)

# Veamos como es la composición de cada cluster segUn la especie
for(i in 1:3) {
  print(paste("cluster", i))
  print(iris2[clustAverage==i, c("Species", "clustAverage")])
}

levels(iris2$clustAverage) <- levels(iris2$Species)
matAverage <- confusionMatrix(iris2$Species, iris2$clustAverage)
paste(matAverage$overall[1], matAverage$overall[2])
#Accuracy: 68.7%, Kappa: 53%

####################################################
# 6. Con centroid linkage
clustCentroid <- cutree(modelo_centroid, k=3)
plot(modelo_centroid, labels=rownames(iris2), main="centroid", hang=-1)
rect.hclust(modelo_centroid, k=3)
iris2$clustCentroid <- as.factor(clustCentroid)

# Veamos como es la composición de cada cluster segUn la especie
for(i in 1:3) {
  print(paste("cluster", i))
  print(iris2[clustCentroid==i, c("Species", "clustCentroid")])
}

levels(iris2$clustCentroid) <- levels(iris2$Species)
matCentroid <- confusionMatrix(iris2$Species, iris2$clustCentroid)
paste(matCentroid$overall[1], matCentroid$overall[2])
#Accuracy: 66%, Kappa: 49%

####################################################
# 7. Con Ward linkage
clustWard <- cutree(modelo_ward, k=3)
plot(modelo_ward, labels=rownames(iris2), main="ward", hang=-1)
rect.hclust(modelo_ward, k=3)
iris2$clustWard <- as.factor(clustWard)

# Veamos como es la composición de cada cluster segUn la especie
for(i in 1:3) {
  print(paste("cluster", i))
  print(iris2[clustWard==i, c("Species", "clustWard")])
}

levels(iris2$clustWard) <- levels(iris2$Species)
matWard <- confusionMatrix(iris2$Species, iris2$clustWard)
paste(matWard$overall[1], matWard$overall[2])
#Accuracy: 82.7%, Kappa: 74%

###############################################
# 8. Visualizar los resultados versus los datos reales
ggplot(iris2, aes(Petal.Length, Petal.Width, color = iris2$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustSingle) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Single")

ggplot(iris2, aes(Petal.Length, Petal.Width, color = iris2$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustComplete) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Complete")

ggplot(iris2, aes(Petal.Length, Petal.Width, color = iris2$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustAverage) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Average")

ggplot(iris2, aes(Petal.Length, Petal.Width, color = iris2$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustCentroid) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Centroid")

ggplot(iris2, aes(Petal.Length, Petal.Width, color = iris2$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustWard) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Ward")

###############################################
# 9. Repetir con PCA

pcomp <- prcomp(iris2[,1:4])
pcomp

#porcentaje de informacion
varianzasPC <- pcomp$sdev^2
porcentajeInfoPC <- varianzasPC / sum(varianzasPC)
porcentajeInfoPC

sum(porcentajeInfoPC[1:2]) #En los 2 primeros PCs
#95.8%
sum(porcentajeInfoPC[1:3]) #en los 3 primeros PCs
#99.5%

biplot(pcomp, c(1,2))
# el PC1 tiene toda la informaciOn de tamaño de los petalos, y el largo del sepalo
# el PC2 tiene el ancho del sepalo

# proyectamos los datos en los dos primeros componentes principales
iris3 <- predict(pcomp, iris2[1:4])[,1:2]
iris3 <- data.frame(iris3)
iris3 <- cbind(iris3, iris2[,5:10])

head(iris3)

# visualizamos los datos según los 2 primeros PCs
ggplot(iris3, aes(x=PC1, y=PC2, color = iris3$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustSingle) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Single")

ggplot(iris3, aes(x=PC1, y=PC2, color = iris3$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustComplete) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Complete")

ggplot(iris3, aes(x=PC1, y=PC2, color = iris3$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustAverage) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Average")

ggplot(iris3, aes(x=PC1, y=PC2, color = iris3$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustCentroid) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Centroid")

ggplot(iris3, aes(x=PC1, y=PC2, color = iris3$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustWard) + 
  scale_color_manual(values = c('black', 'red', 'green')) +ggtitle("Ward")

#####################################################
# 10. Clusterización con K-Means

set.seed(1115)
kmClustering <- kmeans(iris2[,1:4], 3, nstart=100, iter.max=150)
clustKMeans <- kmClustering$cluster
iris2$clustKMeans <- as.factor(clustKMeans)
iris3$clustKMeans <- iris2$clustKMeans

# visualizamos los datos segUn los 2 primeros PCs
ggplot(iris3, aes(x=PC1, y=PC2, color = iris3$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clustKMeans) + 
  scale_color_manual(values = c('black', 'red', 'green')) +
  ggtitle("K-Means")

# Veamos como es la composición de cada cluster segUn la especie
levels(iris2$clustKMeans) <- levels(iris2$Species)
matKmeans <- confusionMatrix(iris2$Species, iris2$clustKMeans)
paste(matKmeans$overall[1], matKmeans$overall[2])
#Accuracy: 83.3%, Kappa: 75%


#####################################################
# 11. Clusterización con DBSCAN

#install.packages("dbscan")
#install.packages("fpc")
#install.packages("factoextra")
library(dbscan)
library(fpc)
library(factoextra)

# Se necesita que los datos esten en forma de matriz
irisMat <- as.matrix(iris[, 1:4])

# Escoger un valor de MinPts tentativo (K = 4, a ojo)
dbscan::kNNdistplot(irisMat, k =  4)
# Encontrar el codo, para obtener el valor de EPS correspondiente
abline(h = 0.40, lty = 2)

# Crear el modelo DBSCAN
set.seed(1234)
modeloDbscan <- dbscan::dbscan(irisMat, 0.40, 4)
modeloDbscan$cluster #Los clusters a los que pertenecen las observaciones. 0 implica ruido

fviz_cluster(modeloDbscan, irisMat, geom = "point") #factoextra library

ggplot(iris3, aes(x=PC1, y=PC2, color = iris3$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) +
  scale_color_manual(values = c('black', 'red', 'green')) +
  ggtitle("K-Means")
