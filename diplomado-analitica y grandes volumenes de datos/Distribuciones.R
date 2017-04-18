
      # Probabilidad de que en 4 repticiones se observen 2 "exitos". 
      #     En cada intento la probabilidad de un "ëxito" es de 0.5
        dbinom(2, size=4, prob=.5)
        
        # Probabilidad de que en 4 repticiones se observen 2 "exitos" o menos. 
        #     En cada intento la probabilidad de un "ëxito" es de 0.5
        pbinom(2, size=4, prob=.5)
        
        #noten que es lo mismo que acumular
        dbinom(2, size=4, prob=.5) + dbinom(1, size=4, prob=.5) + dbinom(0, size=4, prob=.5)
        

        # El número de "éxitos" tal que existe una probabilidad (p) de 0.6875 de
        #      obtener ese número de exitos o menos
        #     En cada intento la probabilidad de un "ëxito" es de 0.5
        qbinom(0.6875, size=4, prob=.5)
        
        
        #generación de número aleatorio
        rbinom(1, size=4, prob=.5)


  # Repliquemos los gráficos que tengo en la presentación

  # Si  n permanece igual pero p cambia, entonces la forma de la dn cambia
      x <- seq(0, 10, 1)
      
                layout(matrix(c(1,2,3,4,5,6), 2,3, byrow=TRUE))
                barplot(dbinom(x, size=10, prob=.05), xlim=c(0,15), xlab="x (No de Exitos)", 
                        ylab="P(X)=x", main= "n= 10 y pi = .05", col="blue") 
                barplot(dbinom(x, size=10, prob=.1), xlim=c(0,15), xlab="x (No de Exitos)", 
                        ylab="P(X)=x", main= "n= 10 y pi = .1", col="blue")  
                barplot(dbinom(x, size=10, prob=.2), xlim=c(0,15), xlab="x (No de Exitos)", 
                        ylab="P(X)=x", main= "n= 10 y pi = .2", col="blue")  
                barplot(dbinom(x, size=10, prob=.4), xlim=c(0,15), xlab="x (No de Exitos)", 
                        ylab="P(X)=x", main= "n= 10 y pi = .4", col="blue")  
                barplot(dbinom(x, size=10, prob=.5), xlim=c(0,15), xlab="x (No de Exitos)", 
                        ylab="P(X)=x", main= "n= 10 y pi = .5", col="blue") 
                barplot(dbinom(x, size=10, prob=.9), xlim=c(0,15), xlab="x (No de Exitos)", 
                        ylab="P(X)=x", main= "n= 10 y pi = .9", col="blue") 
      
  # Si p permanece igual pero  n cambia, entonces la forma de la dn es más simétrica    
      x <- seq(0, 80, 1)
            barplot(dbinom(x, size=10, prob=.3), xlim=c(0,50), xlab="x (No de Exitos)", 
                    ylab="P(X)=x", main= "n= 10 y pi = .3", col="blue")  
            barplot(dbinom(x, size=20, prob=.3), xlim=c(0,50), xlab="x (No de Exitos)", 
                    ylab="P(X)=x", main= "n= 20 y pi = .3", col="blue")   
            barplot(dbinom(x, size=30, prob=.3), xlim=c(0,50), xlab="x (No de Exitos)",
                    ylab="P(X)=x", main= "n= 30 y pi = .3", col="blue")   
            barplot(dbinom(x, size=40, prob=.3), xlim=c(0,50), xlab="x (No de Exitos)", 
                    ylab="P(X)=x", main= "n= 40 y pi = .3", col="blue")  
            barplot(dbinom(x, size=50, prob=.3), xlim=c(0,50), xlab="x (No de Exitos)", 
                    ylab="P(X)=x", main= "n= 50 y pi = .3", col="blue")
            barplot(dbinom(x, size=100, prob=.3), xlim=c(0,50), xlab="x (No de Exitos)", 
                    ylab="P(X)=x", main= "n= 100 y pi = .3", col="blue")   


      

            ###########################################################
            #                Distribución normal                      #
            ###########################################################

            #  Muy parecido
  
      
      
              # Probabilidad de obtener un valor menor a 0 (media=0 y Desviación Estándar=1)
              pnorm(0, mean = 0, sd = 1)
              
              #noten que es lo mismo que acumular
             
                
              # El número que garantiza que exite una probabilidad de 0.5 de 
              #  ser menor o igual a el (media=0 y Desviación Estándar=1)
              qnorm(0.5, mean = 0, sd = 1)
              
              
              #generación de número aleatorio
              rnorm(1, mean = 0, sd = 1)    
        
              
              #Hagamos un gráfico de toda la distribución
                  x <- seq(-10, 10, 0.0001)
                  layout(matrix(c(1,2), 2,1, byrow=TRUE))
                  plot(x, type="n", xaxs="i", yaxs="i", xlim=c(-10, 10), ylim=c(0, 0.5),
                       bty="l", xaxt="n", xlab="x", yaxt="n", ylab="f(x)", bg="black",
                       main="Dn Normal con cambio en media igual D.E.")
                  lines(x, dnorm(x,0,1)) 
                  
                  # ahora agregemos una dn con media diferente
                  
                  lines(x, dnorm(x,2,1), col="red") 
                  
                  lines(x, dnorm(x,-3,1), col="blue") 
                  
                  #ahora veamos el efecto de la DE
                  
                  plot(x, type="n", xaxs="i", yaxs="i", xlim=c(-10, 10), ylim=c(0, 0.5),
                       bty="l", xaxt="n", xlab="x", yaxt="n", ylab="f(x)", bg="black", 
                       main="Dn Normal con cambio en D.E. igual media")
                  lines(x, dnorm(x,0,1)) 
                  
                  lines(x, dnorm(x,0,2), col="red") 
                  
                  lines(x, dnorm(x,0,0.8), col="blue") 

