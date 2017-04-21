
#     Ejercicio No. 2 Dendrogram
#      Crímines en Estados Unidos

USArrests


# otro ejemplo
#  Emplearemos una base de datos de los crimenes en los Estados de Estados Unidos
#  en 1973
# 

head(USArrests)

class(USArrests$Murder)
class(USArrests$Assault)
class(USArrests$UrbanPop)
class(USArrests$Rape)


# esta base de datos incluye 


plot(USArrests$Murder, USArrests$Assault)

plot(USArrests)



##################################################################
# Y quE tal si queremos mejor agrupar los datos? #
##################################################################

##################################################################
#                    AnAlisis de cluster                         #
#                    

# Solo funciona con variables cuantitativas
# 



# las variables cuantitativas están medidas en diferentes unidades 
# es importante "estandarizarlas" (normalizar).
# Es decir, quitarle la media y dividirla por la DE
#

#Paso 1.  CAlculo de la media


medias <- apply(USArrests,2,mean)
medias  
SD = apply(USArrests,2,sd) 
SD
USArrests2 <- scale(USArrests, center = medias, scale = SD)
head(USArrests2)
class(USArrests2)
USArrests2<- as.data.frame(USArrests2)
class(USArrests2)
plot(USArrests2)

#Paso 2. Calculo de las distancias euclidianes
#
USArrests.dist = dist(USArrests2)
round(USArrests.dist, 2)

#Paso 3. CAlculo de los clusters herarquicos y graficar el dendograma
#
USArrests.hclust = hclust(USArrests.dist)
dev.off() # reset la divisiOn de la venta de grAficos
plot(USArrests.hclust)
plot(USArrests.hclust, hang = -1)
plot(USArrests.hclust,labels = USArrests$Car,main = 'Dendrograma inicial',hang = -1)

#  Paso 4. Probar otro método. SOlo tener en cuenta el promedio como criterio de aglomeración
#  

USArrests.hclust.a = hclust(USArrests.dist, method =  "average")

plot(USArrests.hclust.a,labels = USArrests$Car,main = 'Dendrograma metodo promedio')
plot(USArrests.hclust.a,labels = USArrests$Car,main = 'Dendrograma metodo promedio', hang = -1)


# Paso 5.Evaluar el numero de clusters 





# para determinar el n´úmero de clusters podemos emplear
#  Silhoutte Plots
library(cluster)

plot(silhouette(cutree(USArrests.hclust,3), USArrests.dist ))

# este gr´áfico muestra que tan bien se ajusta cada observaci´ón al cluster que ha sido asignado
# comparando que tan cerca se encuentra de los demas en su cluster
# numeros cerca a uno significa que la observaci´ón est´á bien ubicada en su cluster
# cerca a cero implicaria que es posible que el individuo fue mal clasificado en su cluster
# en general
# 0.71-1.0	A strong structure has been found
# 0.51-0.70	A reasonable structure has been found
# 0.26-0.50	The structure is weak and could be artificial
# < 0.25	No substantial structure has been found
# 
# EN este caso no parece una buena opci´ón tener los 3 clusters

plot(silhouette(cutree(USArrests.hclust,4), USArrests.dist ))
# no mejora con 4
plot(silhouette(cutree(USArrests.hclust,6), USArrests.dist )) 
plot(silhouette(cutree(USArrests.hclust,5), USArrests.dist ))   
plot(silhouette(cutree(USArrests.hclust,2), USArrests.dist ))
# pareciera mejor 2 clusters



# ¿Y si usamos el promedio?
plot(silhouette(cutree(USArrests.hclust.a, 2), USArrests.dist ))
plot(silhouette(cutree(USArrests.hclust.a, 3), USArrests.dist ))
plot(silhouette(cutree(USArrests.hclust.a, 4), USArrests.dist ))
plot(silhouette(cutree(USArrests.hclust.a,5), USArrests.dist ))   
plot(silhouette(cutree(USArrests.hclust.a,6), USArrests.dist )) 

# pareciera mejor 2 clusters (y el m´étodo average)



# Otra forma de selccionar el numero de clusters es emplear
# Scree plots
# 

wss <- (nrow(USArrests2) - 1)*sum(apply(USArrests2,2,var))
for (i in 2:37) wss[i] <- sum(kmeans(USArrests2, 
                                     centers = i)$withinss)
plot(1:37, wss, type = "b", xlab = "Number of Clusters",
     ylab = "Within groups sum of squares")  
# este gr´áfico permite determinar cual es el cambio en SS al interior del grupo cuando se agrega un cluster mas
# EN este caso pasar de uno a dos y de dos a tres clusters gerena un aumento grande. 
# De tres a cuatro y de ahi en adelante no existe un aumento grande. As´í que todo parede que 2 0 3 clusters es lo ideal.
#  miremos los resultados con dos 2 clusters
groups.2 = cutree(USArrests.hclust,2)
groups.2a = cutree(USArrests.hclust.a,2)
table(groups.2, groups.2a)

# noten que la diferencia se encuentra en un estado que se clasifica difernte entre las dos aproximaciones
# miremos los resultados

#Seguiremos usando dos clusters y el m´étodo de average
plot(USArrests$Murder, USArrests$Assault, data = USArrests, col = groups.2a)
plot(USArrests$Murder, USArrests$UrbanPop, data = USArrests, col = groups.2a)
plot(USArrests$Murder, USArrests$Rape, data = USArrests, col = groups.2a)

plot(USArrests$Rape, USArrests$Assault, data = USArrests, col = groups.2a)


# Calculemos las medias por cluster

aggregate(USArrests2, list(groups.2a), mean)
aggregate(USArrests, list(groups.2a), mean) # en la escala original

# ¿existe direfencia entre los grupos?
# Veamos, empleando un modelo ANOVA

# Primero se necesita construir la base de datos (agregar la variable de grupo)
groups.2a
USArrests
USArrests.Grupos <- cbind(USArrests, groups.2a)
head(USArrests.Grupos)
# ahora comparemos las medias (para cada una de las variables)
# Esa es la gran diferenica con lo anterior
      # Murder

      fit <- aov(Murder ~ groups.2a , data = USArrests.Grupos )
      summary(fit)
      print(fit)
      # con un 99% de confianza se puede rechazar la nula que las medias de muertes 
      # por cada mil habitantes sean iguales entre los dos grupos


      # Assault
      
      fit.A <- aov(Assault ~ groups.2a , data = USArrests.Grupos )
      summary(fit.A)
      print(fit.A)
      # con un 99% de confianza se puede rechazar la nula que las medias de los asaltos 
      # por cada mil habitantes sean iguales entre los dos grupos
      
      
      # UrbanPop
      
      fit.U <- aov(UrbanPop ~ groups.2a , data = USArrests.Grupos )
      summary(fit.U)
      print(fit.U)
      # No se puede rechazar la nula que las medias de los asaltos 
      # por cada mil habitantes sean iguales entre los dos grupos
      
      # Rape
      
      fit.R <- aov(Rape ~ groups.2a , data = USArrests.Grupos )
      summary(fit.R)
      print(fit.R)
      # con un 99% de confianza se puede rechazar la nula que las medias de los violaciones 
      # por cada mil habitantes sean iguales entre los dos grupos
      


# Dado que el promedio para UrbanPop no es estad´ísticamente diferenite para los grupos (clusters),
#       se puede afirmar que esa variable no aporta para caracterisar el cluster. 
###############################################################
###############################################################
# Ahora realizemos el mismo an´álisis pero sin la vaible UrbanPop 
# 

      
      head(USArrests2)
      # remover la tercera columna
      USArrests3 <- USArrests3[,-3]
      
      class(USArrests3)
      USArrests3<- as.data.frame(USArrests3)
      class(USArrests3)
      plot(USArrests3)
      
      #Paso 2. Calculo de las distancias euclidianes
      #
      USArrests.dist = dist(USArrests3)
      
      
      
      # Paso 5.Evaluar el numero de clusters 
      #  
      
      
      # para determinar el n´úmero de clusters podemos emplear
      #  Silhoutte Plots
      library(cluster)
      
      plot(silhouette(cutree(USArrests.hclust,3), USArrests.dist ))
      
      # este gr´áfico muestra que tan bien se ajusta cada observaci´ón al cluster que ha sido asignado
      # comparando que tan cerca se encuentra de los demas en su cluster
      # numeros cerca a uno significa que la observaci´ón est´á bien ubicada en su cluster
      # cerca a cero implicaria que es posible que el individuo fue mal clasificado en su cluster
      # en general
      # 0.71-1.0	A strong structure has been found
      # 0.51-0.70	A reasonable structure has been found
      # 0.26-0.50	The structure is weak and could be artificial
      # < 0.25	No substantial structure has been found
      # 
      # EN este caso no parece una buena opci´ón tener los 3 clusters
      
      plot(silhouette(cutree(USArrests.hclust,4), USArrests.dist ))
      # no mejora con 4
      plot(silhouette(cutree(USArrests.hclust,6), USArrests.dist )) 
      plot(silhouette(cutree(USArrests.hclust,5), USArrests.dist ))   
      plot(silhouette(cutree(USArrests.hclust,2), USArrests.dist ))
      # pareciera mejor 2 clusters
      
      
      
      # ¿Y si usamos el promedio?
      plot(silhouette(cutree(USArrests.hclust.a, 2), USArrests.dist ))
      plot(silhouette(cutree(USArrests.hclust.a, 3), USArrests.dist ))
      plot(silhouette(cutree(USArrests.hclust.a, 4), USArrests.dist ))
      plot(silhouette(cutree(USArrests.hclust.a,5), USArrests.dist ))   
      plot(silhouette(cutree(USArrests.hclust.a,6), USArrests.dist )) 
      
      # pareciera mejor 2 clusters (y el m´étodo average)
      
      
      
      # Otra forma de selccionar el numero de clusters es emplear
      # Scree plots
      # 
      
      wss <- (nrow(USArrests3) - 1)*sum(apply(USArrests3,2,var))
      for (i in 2:37) wss[i] <- sum(kmeans(USArrests3, 
                                           centers = i)$withinss)
      plot(1:37, wss, type = "b", xlab = "Number of Clusters",
           ylab = "Within groups sum of squares")  
      # este gr´áfico permite determinar cual es el cambio en SS al interior del grupo cuando se agrega un cluster mas
      # EN este caso pasar de uno a dos y de dos a tres clusters gerena un aumento grande. 
      # De tres a cuatro y de ahi en adelante no existe un aumento grande. As´í que todo parede que 2 0 3 clusters es lo ideal.
      #  miremos los resultados con dos 2 clusters
      groups.2 = cutree(USArrests.hclust,2)
      groups.2a = cutree(USArrests.hclust.a,2)
      table(groups.2, groups.2a)
      
      # noten que ahora coinciden los dos grupos
      
      #Seguiremos usando dos clusters y el m´étodo de average
      plot(USArrests$Murder, USArrests$Assault, data = USArrests, col = groups.2a)
      plot(USArrests$Murder, USArrests$UrbanPop, data = USArrests, col = groups.2a)
      plot(USArrests$Murder, USArrests$Rape, data = USArrests, col = groups.2a)
      
      plot(USArrests$Rape, USArrests$Assault, data = USArrests, col = groups.2a)
      
      
      # Calculemos las medias por cluster
      
      aggregate(USArrests3, list(groups.2a), mean)
      aggregate(USArrests, list(groups.2a), mean) # en la escala original
      
      # ¿existe direfencia entre los grupos?
      # Veamos, empleando un modelo ANOVA
      
      # Primero se necesita construir la base de datos (agregar la variable de grupo)
      
      USArrests.Grupos <- cbind(USArrests, groups.2a)
      head(USArrests.Grupos)
      # ahora comparemos las medias (para cada una de las variables)
      # Esa es la gran diferenica con lo anterior
      # Murder
      
      fit <- aov(Murder ~ groups.2a , data = USArrests.Grupos )
      summary(fit)
      print(fit)
      # con un 99% de confianza se puede rechazar la nula que las medias de muertes 
      # por cada mil habitantes sean iguales entre los dos grupos
      
      
      # Assault
      
      fit.A <- aov(Assault ~ groups.2a , data = USArrests.Grupos )
      summary(fit.A)
      print(fit.A)
      # con un 99% de confianza se puede rechazar la nula que las medias de los asaltos 
      # por cada mil habitantes sean iguales entre los dos grupos
      
      
      # Rape
      
      fit.R <- aov(Rape ~ groups.2a , data = USArrests.Grupos )
      summary(fit.R)
      print(fit.R)
      # con un 99% de confianza se puede rechazar la nula que las medias de los violaciones 
      # por cada mil habitantes sean iguales entre los dos grupos
      
      #En este caso todas las variables son relevantes.
      #Podemos emplear esta agrupaci´ón para trabjar.    
      
      ###############################################################
      ###############################################################

# ahora miremos a que grupo corresponde cada estado
# 
USArrests[groups.2a == 1,]
      USArrests[groups.2a == 2,]
# ¿C´ómo interpretar estos dos grupos?
# 

      aggregate(USArrests, list(groups.2a), mean) # en la escala original
