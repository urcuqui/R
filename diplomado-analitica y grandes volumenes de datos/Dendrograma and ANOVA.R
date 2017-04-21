
#     Ejercicio ANOVA y dendrogram


  cars = read.delim(file.choose(),stringsAsFactors = FALSE)

# otro ejemplo
#  Emplearemos una base de datos del comportamiento de carros
#  en USA entre 1978-1979
# 

head(cars)
class(cars$Country)
class(cars$Cylinders)



# esta base de datos incluye 




cars$Cylinders <- as.factor(cars$Cylinders)



plotmeans(cars$MPG ~ cars$Cylinders,xlab = "No de cilindros",
          ylab = "Millas por gal", main = "Media e IC del 95%")

plotmeans(cars$MPG ~ cars$Country , xlab = "PaIs",
          ylab = "Millas por gal", main = "Media e IC del 95%")

# modelo ANOVA de dos vias
fit <- aov(MPG ~ Cylinders + Country , data = cars)
summary(fit)
print(fit)

# Con un 95% los dos factores parecen importantes para explicar
# el consumo promedio de gal por milla
# 



# Y los supuestos? 
#
layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

# y cu醠es medias son diferentes?
# prueba no param閠rica
# de Tukey

TukeyHSD(fit)


##################################################################
# Y quE tal si queremos mejor agrupar los datos no solo por cyl? #
##################################################################

##################################################################
#                    AnAlisis de cluster                         #
#                    

# Solo funciona con variables cuantitativas
# 
attach(cars)
    plot(cars$MPG, cars$Cylinders, data = cars)
    plot(MPG, Weight, data = cars)
    plot(MPG, Drive_Ratio, data = cars)
    plot( Weight, Drive_Ratio, data = cars)


      head(cars)
    
# las dos primera no son cuantitativas
# y Cilindros la convertimos en factor
#  regresmola a numerica
      class(cars$Cylinders)
      
      cars$Cylinders <- as.numeric(cars$Cylinders)

      class(cars$Cylinders)      
    
  # las variables cuantitativas est醤 medidas en diferentes unidades 
  # es importante "estandarizarlas" (normalizar).
  # Es decir, quitarle la media y dividirla por la DE
  #

      #Paso 1. Remover variables cualitativas y CAlculo de la media
      
      cars2 <- cars[,-c(1,2)]
      medias <- apply(cars2,2,mean)
      medias  
      SD = apply(cars2,2,sd) 
      cars2 <- scale(cars2, center = medias, scale = SD)
      head(cars2)
      
      #Paso 2. Calculo de las distancias euclidianes
      #
      cars.dist = dist(cars2)
      round(cars.dist, 2)
      
      #Paso 3. CAlculo de los clusters herarquicos y graficar el dendograma
      #
      cars.hclust = hclust(cars.dist)
      dev.off() # reset la divisiOn de la venta de grAficos
      plot(cars.hclust)
      plot(cars.hclust,labels = cars$Car,main = 'Dendrograma inicial')
      
      #  Paso 4. Probar otro m閠odo. SOlo tener en cuenta el promedio como criterio de aglomeraci髇
      #  
      
      cars.hclust.a = hclust(cars.dist, method =  "average")
      
      plot(cars.hclust.a,labels = cars$Car,main = 'Dendrograma metodo promedio')
      
      # Paso 5. podemos recortar el numero de clusters y mirar la membresia a cada cluster
      #  por ejemplo a 3 clusters
      
      groups.3 = cutree(cars.hclust,3)
      groups.3a = cutree(cars.hclust.a,3)
      
      table(groups.3, groups.3a)
      #En este caso no existe diferencia entre los dos m閠odos
      
      # Se pueden calcular las medias por cluster
      
      aggregate(cars2, list(groups.3), mean)
      # valores normalizados
      # 
      # si el promedio para una variable no cambia mucho entre grupos (clusters) se puede afirmar que esa 
      # variable no aporta para caracterisar el cluster. En este caso, todas las variables son importantes
      #
      # Ahora regresemos a realizar el calculo en las unidades originales 
      # 
      # 
      aggregate(cars, list(groups.3), mean)
      
      # para ver que carro se encuentra en cada cluster podemos emplear
      # 
      cars$Car[groups.3 == 1]
    
      # o para todos a la vez
      # 
      sapply(unique(groups.3),function(g)cars$Car[groups.3 == g])
      
      # para determinar el n代mero de clusters podemos emplear
      #  Silhoutte Plots
      library(cluster)
      
      plot(silhouette(cutree(cars.hclust,3), cars.dist ))
      
      # este gr瘁fico muestra que tan bien se ajusta cada observaci大n al cluster que ha sido asignado
      # comparando que tan cerca se encuentra de los demas en su cluster
      # numeros cerca a uno significa que la observaci大n est瘁 bien ubicada en su cluster
      # cerca a cero implicaria que es posible que el individuo fue mal clasificado en su cluster
      # en general
      # 0.71-1.0	A strong structure has been found
      # 0.51-0.70	A reasonable structure has been found
      # 0.26-0.50	The structure is weak and could be artificial
      # < 0.25	No substantial structure has been found
      # 
      # EN este caso no parece una buena opci大n tener los 3 clusters
      
      
      plot(silhouette(cutree(cars.hclust,4), cars.dist ))
      # no mejora con 4
      
      plot(silhouette(cutree(cars.hclust,5), cars.dist ))   
      plot(silhouette(cutree(cars.hclust,2), cars.dist ))
      # pareciera mejor 2 clusters
      
      # Otra forma de selccionar el numero de clusters es emplear
      # Scree plots
      # 
      
      wss <- (nrow(cars2) - 1)*sum(apply(cars2,2,var))
      for (i in 2:37) wss[i] <- sum(kmeans(cars2, 
                                           centers = i)$withinss)
      plot(1:37, wss, type = "b", xlab = "Number of Clusters",
           ylab = "Within groups sum of squares")  
      # este gr瘁fico permite determinar cual es el cambio en SS al interior del grupo cuando se agrega un cluster mas
      # EN este caso pasar de uno a dos y de dos a tres clusters gerena un aumento grande. 
      # De tres a cuatro y de ahi en adelante no existe un aumento grande. As错 que todo parede que 3 clusters es lo ideal.
      # 
      
      # miremos los resultados
      plot(MPG, Cylinders, data = cars, col = groups.3)
      plot(MPG, Weight, data = cars, col = groups.3)
      plot(MPG, Drive_Ratio, data = cars, col = groups.3)
      plot( Weight, Drive_Ratio, data = cars, col = groups.3)
      
      
