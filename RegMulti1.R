

###########################################################
###########################################################
###########################################################
###########################################################
#     Ejemplo selección automática de modelos 
###########################################################

# Primero intentaremos con el paquete Leaps
# Este intenta buscar los mejores "j" modelo que emplean "k" número de variables
# comparando el R2 ajustado
##########################################################


      # instalar el paquete
      # install.packages("leaps")
      # 
      library(leaps)
  
      
      # empleemos unos datos que est´án en linea que corresponden 
      # LOW BIRTH WEIGHT DATA
      # El objetivo del estudio era determinar las variables asociadas a dar a luz 
      # un bebe con bajo peso al nacer (BWT) en gramos. Es decir BWT < 2500 grams.
      # Los datos se recolectaron a 189 mujeres, 59 de las cuales dieron a luz bebes con bajo bwt.  
      # Cuatro variables que se creían importantes fueron medidas:
      # - edad (AGE), 
      # - Peso de la madre al momento de su último periodo mestrual (LWT), 
      # - Raza (1 = White, 2 = Black, 3 = Other) (RACE),
      # - El número de visitas médicas de controland realizadas durante 
      #     el primer trimestre del emaprazo (FTV)
      # Adem´ás se cuenta con la siguiente información:
      # - ID =Identification Code   
      # - LOW=   Low Birth Weight (0 = Birth Weight >= 2500g,  1 = Birth Weight < 2500g)
      # - AGE = Age of the Mother in Years  
      # - SMOKE = Smoking Status During Pregnancy (1 = Yes, 0 = No) 
      # - PTL = History of Premature Labor (0 = None  1 = One, etc.)
      # - HT = History of Hypertension (1 = Yes, 0 = No) 
      # - UI = Presence of Uterine Irritability (1 = Yes, 0 = No)      


    # Paso 1. Cargar los datos pero desde un sitio Web
      #install.packages("gdata")
      library(gdata)
      lbw <-readxl::read_excel(file.choose())
      #lbw <- read.xls("http://www.umass.edu/statdata/statdata/data/lowbwt.xls")
      head(lbw)
      
      # un truco para cambiar el nombre de las variables que están en mayusculas a min´sculas
      names(lbw) <- tolower(names(lbw))
      head(lbw)
      
      
      lbw <- within(lbw, {
        ## Cambiando los labels de race
        race.cat <- factor(race, levels = 1:3, labels = c("White","Black","Other"))
        
        ## ftv (frequency of visit) Cambiando los labels 
        ftv.cat <- cut(ftv, breaks = c(-Inf, 0, 2, Inf), labels = c("None","Normal","Many"))
        ftv.cat <- relevel(ftv.cat, ref = "Normal")
        
        ## ptl
        preterm <- factor(ptl >= 1, levels = c(F,T), labels = c("0","1+"))
      
        })
      
      head(lbw)
      
      # chequear la definicón de las variables
      class(lbw$age)
      class(lbw$age)    
      
      ##########################################################
      # la regresión empleando solo las cuatro variables de interés sería
      ##########################################################
      
      
      R1 <- lm(bwt ~ age + lwt + race.cat +  ftv.cat, data = lbw)
      
      # resultados
      # 
      R1
      summary(R1)
      layout(matrix(c(1,2,3,4),2,2)) 
      plot(R1)
      layout(matrix(c(1),1,1)) 
             
             ##########################################################
             #      Y si incluimos todas las variables?
             ##########################################################      
      
             R2 <- lm(bwt ~ age + lwt + race.cat +  ftv.cat + smoke + preterm + ht + ui , data = lbw)
             
             # resultados
             # 
             R2
             summary(R2)
             layout(matrix(c(1,2,3,4),2,2)) 
             plot(R1)
             layout(matrix(c(1),1,1)) 
    
   ##############################################################################          
   # Paso 2. Cargar el paquete y seleccionar el mejor modelo con k variables            
   ##############################################################################              
   #install.packages("leaps")           
   
      regsubsets.out <-   regsubsets(bwt ~ age + lwt + race.cat + smoke + preterm + ht + ui + ftv.cat,
                   data = lbw,
                   nbest = 1,       # 1 best model for each number of predictors
                   nvmax = NULL,    # NULL for no limit on number of variables
                   force.in = NULL, force.out = NULL,
                   method = "exhaustive")
      regsubsets.out
      
      
      
      #resultados
      summary.out <- summary( regsubsets.out )
      as.data.frame( summary.out$outmat )
      attributes( regsubsets.out )
      attributes(summary.out)
      summary.out$adjr2
      summary.out$which
      
            
      layout(matrix(c(1),1,1)) 
      plot(regsubsets.out, scale = "adjr2", main = "Adjusted R^2")
      plot(regsubsets.out, scale = "r2", main = "R^2")
      

     
      summary.out$which[ which.max(summary.out$adjr2),]
 
      
      ##############################################################################          
      # Paso 3. Esstimar elmejor modelo            
      ##############################################################################              
      
      
      best.model <- lm(bwt ~ lwt + race.cat + smoke + preterm + ht + ui, data = lbw)
      summary(best.model)
      
##########################################################  
##########################################################  
##########################################################  
# Segundo intentaremos con un método pra remover variables que no sean significativas
# step()
# Se puede iniciar con un modelo sin variables y adicionar
# o sepuede empezar con todas las variables y quitar (backward)
##########################################################    
##########################################################    
########################################################## 
########################################################## 

      # forward
      min.model <-       lm(bwt ~ 1, data = lbw)
      
      biggest <- formula(lm(bwt~.,lbw))

      
      biggest
      
      fwd.model = step(min.model, direction='forward', scope=biggest)
      fwd.model
      summary(fwd.model)
      
      # backward
      minimal <- formula(lm(bwt~ 1,lbw))
      max.model <-       lm(bwt ~ ., data = lbw)
      
      
      back.model = step(max.model, direction='backward', scope=minimal)
      back.model
      summary(back.model)
      
           
      
      
