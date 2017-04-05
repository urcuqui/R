
###########################################################
###########################################################
###########################################################
#                   Diplomado
#     Analítica y Grandes Volúmenes de Datos
#               Universidad Icesi
###########################################################
#          Estadística para la analítica                 
###########################################################
#           Julio César Alonso
#           Profesor Titular
#           jcalonso@icesi.edu.co
###########################################################
###########################################################

###########################################################
# Ejercicio en clase
###########################################################
# Este ejercicio tiene como objetivo ilustrar los siguientes pasos 
# que corresponden al análisis exploratorio de los datos:
#    ???	Definición del tipo de variables
#    ???	Gráficos exploratorios


      ####################################
      # Primer Paso: Lectura de datos    #
      ####################################

      data<-read.csv(file.choose())


      #####################################################
      # Segundo Paso: chequear si los datos cargaron bien #
      #####################################################
      
     
        head(data)
        
        data <- data[,-1]
        
        head(data)
        dim(data)
        class(data)
        attributes(data)
        
        
        #####################################################
        # Tercer Paso: definici'on de las variables         #
        #####################################################
        

        class(data$ID)
        # esta variable debería ser una variable nominal 
        
        data$ID <-as.factor(data$ID)
        
        class(data$ID)
        
        # repetir lo mismo con las otras variables
        
        class(data$Ventas)
        # ok no es necesario cambiarla
        
        class(data$Zona)  
        # ok no es necesario cambiarla
        data$Zona
        
        class(data$Vendedor)  
        
        # noten que esto no tiene sentido aquí tenemos vendedores y el orden no importa
        
        data$Vendedor <-as.factor(data$Vendedor)
        
        class(data$Vendedor)
        data$Vendedor
        
        class(data$Devol)
        # ok no es necesario cambiarla
        
        class(data$Satis)
        # Noten que en este caso tenemos una variable cualitativa,     
        data$Satis <-as.factor(data$Satis)
        class(data$Satis)
        data$Satis
        
        class(data$Canal)
        # ok no es necesario cambiarla
        data$Canal
        
        class(data$Turno)
        # Noten que en este caso tenemos una variable ordinal
        data$Turno <- factor(data$Turno, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
        data$Turno

        #####################################################
        # Cuarto Paso: Grafiquemos los datos                #
        #####################################################
        
   
        # Un truco
        attach(data)
        
        hist(Ventas)
        hist(Devol)
        plot(Ventas, Devol)
        boxplot(Ventas)
        boxplot(Ventas~Zona,data=data, main="Ventas por zona", 
                xlab="Ventas", ylab="Zona")
        boxplot(Ventas~Satis,data=data, main="Ventas por Satisfaccion", 
                xlab="Ventas", ylab="Satisfaccion")
        boxplot(Ventas~Canal,data=data, main="Ventas por Canal", 
                xlab="Ventas", ylab="Canal")
        boxplot(Devol~Turno,data=data, main="Ventas por turno", 
                xlab="Ventas", ylab="Turno")
        
        #existen otros gr'aficos que no dicen mucho
        plot(Satis,Turno)
        
        pairs(~Ventas+Devol+Zona+Satis + Vendedor + Canal + Turno,data=data, 
              main="Simple Scatterplot Matrix")


