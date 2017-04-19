
###########################################################
#                Teorema del Límite Central  (TLC)       #
###########################################################

# 1. Generar una muestra de tamaño n desde una distribución
#  uniforme
#  
# 2. Paso calcular la media y guardar el resultado
#  
# 3. Repetir 1 y 2 muchas veces
# 
# 4. ver las propiedades de las medias calculadas
# 

      TLC <-function(n, m){
        # n= tamaño de la muestra
        # m= número de repeticioines (veces que se calcula la media)
        # 
       y<- integer(0) # crea un objeto vacio
        for( i in 1:m )
        {
          muestra<- runif(n,0,1) # se crea muestra aleatoria con media poblacional de 0.5
          y[i]<-mean(muestra)
            }
       result <- list(medias=y)
       return(result)
       
       }
      
  # ahora correremos la funcción
  
      R1 <- TLC(100, 1000)
  # la media de los medias es

      mean(R1$medias)  
      round(mean(R1$medias),2)
      
  #grafiquemos los resultados
      layout(matrix(c(1,2), 2,1, byrow=TRUE))
      
      hist(R1$medias, xlab= "medias calculadas", ylab="No de veces", 
           main = "Dn muestral de las medias de muestras de n=100 de una dn uniforme", 50, 
           freq = FALSE)
      
      curve(dnorm(x, mean=mean(R1$medias), sd=sd(R1$medias)), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")
      
    # repliquemos el resultado con una muestra mas grande

      R2 <- TLC(200, 1000)
      # la media de los medias es
      
      mean(R2$medias)  
      round(mean(R2$medias),2)
      
      #grafiquemos los resultados
      
      
      hist(R2$medias, xlab= "medias calculadas", ylab="No de veces", 
           main = "Dn muestral de las medias de muestras de n=200 de una dn uniforme", 50, 
           freq = FALSE)
      
      curve(dnorm(x, mean=mean(R2$medias), sd=sd(R2$medias)), 
            col="darkblue", lwd=2, add=TRUE, yaxt="n")  
