
#     Ejercicio estadísticas descriptivas 
#                        y 
#             comparación de medias


#     Generación de datos 



  
            #Leer los datos
            data<-read.csv(file.choose())
            
            # chequear si los datos cargaron bien
            
            head(data)
            dim(data)
            class(data)
             
            # instalar y cargar los paquetes necesarios       
           # install.packages("psych")
  
            # Estadísticas Descriptivas
            
            library(psych)
            describe(data[,2:3])
            
            # las medias son muy parecidas, pero ...
            # otra forma, pero da menos información
            # 
            summary(data)
            
            # Veamos los gráficos de los datos.
  
           layout(matrix(c(1,2), 2,1, byrow=TRUE))
           hist(data$zona1)
           hist(data$zona2)
 
              # Cuidado con los ejes
             #  Ahora hagamoslos iguales
             hist(data$zona1, xlim = c(0,3500), ylim = c(0,200), breaks=25)
             hist(data$zona2, xlim = c(0,3500), ylim = c(0,200), breaks=25)
             
             # las dos distribuciones son muy diferentes
  
             
             # no obstante la pregunta sería 
             #  ¿ son iguales las medias?
          
            # Por ahora creemos intervalos de confianza
            # Para la media
      
             t.test(data$zona1)       
     
             t.test(data$zona1, conf.level = 0.99)$conf.int
             # ahora con un 99%
             
             # Para la otra zona
             #
             
             t.test(data$zona2)       
             
             t.test(data$zona2, conf.level = 0.99)$conf.int
     
     #install.packages("Rmisc")
     library(Rmisc)
     CI(data$zona1)
     CI(data$zona2)
                           
        ###############
       # Comapración de medias 
       #  ANOVA #
       ###############
        
  # necesitamos una base de datos organizada diferente
  # 
  
    head(data)
     
     zona <-rep(c("zona1", "zona2"), each = 1000)
     ventas <- c(data$zona1, data$zona2)
     data1<-as.data.frame(cbind(zona, ventas))
     data1[980:1010,]
     class(data1)
     # chequeo del tipo de variables
      
     class(data1$zona) 
     # está bien definida
     class(data1$ventas)
     # no está bien definida  
     #
     data1$ventas <- as.numeric(data1$ventas)
     class(data1$ventas)
       
     # modelo anova (una sola via)
     fit <- aov(ventas ~ zona, data=data1)
     
     fit
     # Tabla ANOVA
     summary(fit)
     print(fit)
      # no se puede rechazar que las medias sean iguales
      # Pero ... miremos los supuestos  
     
     layout(matrix(c(1,2,3,4),2,2)) # optional layout 
     plot(fit)
    # no parece cumplir los supuestos
    # ########################
    # 
    
     # una opción es emplear una preuba que no tiene
     # supuestos. Se conoce como la prueba no paramétrica
     # de Tukey (tambien conocida como el método de Tukey (o Tukey's honest
     #  significance test, Tukey's HSD (honest significant difference) test
     #  ,or the Tukey???Kramer method)
     TukeyHSD(fit)
     
     #También podemos graficar los resultados
     
     library(gplots)
     plotmeans(ventas ~ zona, data=data1)
     
     
     
     
     
