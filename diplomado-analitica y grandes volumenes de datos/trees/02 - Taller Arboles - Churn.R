###########################################################################

####################################################
# 1. Analisis exploratorio (bAsico)

#install.packages("caret")
#install.packages("ggplot2")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)

# Cargar y explorar el dataset de defaults de churn de clientes
# QuE ven de particular en los datos? .............
churn <- read.table("03 - churn.csv", sep=";", header = TRUE)
summary(churn)
str(churn)
table(churn$LEAVE)
prop.table(table(churn$LEAVE))

# Vamos a ver que primeras ideas nos quedan despuEs de visualizar las distribuciones 
# de agunos de los atributos
featurePlot(x=churn[,c("HOUSE", "INCOME", "OVERAGE", "LEFTOVER")], y=churn$LEAVE, plot="pairs")
ggplot(churn, aes(x=HOUSE, y=OVERAGE, color=LEAVE)) + geom_point(size=2, alpha=.5)
ggplot(churn, aes(x=HOUSE, y=INCOME, color=LEAVE)) + geom_point(size=2, alpha=.5)
ggplot(churn, aes(x=INCOME, y=OVERAGE, color=LEAVE)) + geom_point(size=2, alpha=.5)
ggplot(churn, aes(x=LEFTOVER, y=OVERAGE, color=LEAVE)) + geom_point(size=2, alpha=.5)

####################################################
# 2. Separar el dataset en training y test set usando caret
set.seed(1234)
inTrain <- createDataPartition(
  y = churn$LEAVE, # Estratificar la particion segun la clase a aprender
  p = .75,         # Porcentaje de los datos que se quedarán en el training set
  list = FALSE)    # Que los resultados esten en una matriz y no en una lista

str(inTrain)
training <- churn[ inTrain,] # training set
testing  <- churn[-inTrain,] # test set, no lo utilizaremos sino hasta el final del proceso
dim(training) #15000 instancias de aprendizaje
dim(testing)  #5000 instancias de testing

####################################################
# 3. Aprender árbol sin post poda

# Vamos a utilizar el modelo de aprendizaje supervisado "rpart", que es una implementaciOn
# del algoritmo de Arboles de decisiOn CART.
# Por defecto el mEtodo train de caret utiliza tEcnicas de resampleo para determinar el 
# valor Optimo de los parAmetros del algoritmo (mEtodo) utilizado para el aprendizaje.
# En el caso de rpart, train intenta encontrar el mejor valor de cp, el parAmetro de complejidad
# que regula el overfitting del Arbol aprendido, controlando el nUmero de hojas encontradas 
# a partir de la post poda de ramas. Un valor de 0 implica que no se tiene en cuenta la complejidad
# del Arbol, y que no se poda.
# Vamos explIcitamente a especificar los parAmetros que desactivan la post poda, especificando una
# pre poda solamente basada en una profundidad mAxima de 3 para poder visualizar fácilmente
# el modelo aprendido

set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
grid <- data.frame(cp=c(0)) #Fijamos en 0 el parAmetro de complejidad para eliminar la post poda
modeloBasico <- train(LEAVE~., data = training, method = "rpart",
                tuneGrid=grid,
                control = rpart.control(
                  minsplit = 2, #min instancias para intentar split de un nodo, por defecto es 20
                  minbucket = 1, #min instancias en un nodo hoja terminal
                  maxdepth = 3 #max depth, por defecto es 30
                ))

modeloBasico
modeloBasico$bestTune   # el parAmetro de complejidad cp Optimo encontrado por caret, en este caso lo fijamos en 0
modeloBasico$finalModel # explica el tipo de modelo aprendido

# VisualizaciOn sencilla de la estructura y del detalle de los nodos
plot(modeloBasico$finalModel)           
text(modeloBasico$finalModel, cex=0.7) #cex controla el tamaNo de la letra

# visualizaciones mas sofisticadas, ojo al significado de los colore
rpart.plot(modeloBasico$finalModel)     
fancyRpartPlot(modeloBasico$finalModel)

# ExplicaciOn del detalle de las particiones
modeloBasico$finalModel$splits

# Se realizan predicciones con el mEtodo predict
predicciones <- predict(modeloBasico, newdata=testing) #type "raw" por defecto
predicciones
prediccionesProb <- predict(modeloBasico, newdata=testing, type="prob")
prediccionesProb

# Veamos como nos va con la matriz de confusiOn, esto incluye Sensitivity, 
# Specificity, + prediction value y - prediction value
confusionMatrix(predicciones, testing$LEAVE)

#Comparemos los niveles de accuracy del training set y los del test set
modeloBasico

#tambiEn podemos extraer otras mEtricas como Precision, Recall y F1
confusionMatrix(predicciones, testing$LEAVE, mode = "prec_recall")
confusionMatrix(predicciones, testing$LEAVE, mode = "everything")

#############################################################
# Overfitting: Como seria el árbol si desactivamos todas la post poda y dejamos una
# pre poda muy permisiva (parAmetros por defecto de pre poda)

grid <- data.frame(cp=c(0)) #Fijamos en 0 el parAmetro de complejidad para eliminar la post poda
modeloSinPodas <- train(LEAVE~., data = training, method = "rpart",
                      tuneGrid=grid)
plot(modeloSinPodas$finalModel)           
text(modeloSinPodas$finalModel, cex=0.7) #cex controla el tamaNo de la letra
rpart.plot(modeloSinPodas$finalModel)     

predicciones <- predict(modeloSinPodas, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE, mode = "prec_recall")

# COmo nos fuE con el overfitting?


####################################################
# 4. EstandarizaciOn de los datos

# Acordemonos de lo diferentes que eran los Ordenes de magnitud de las variables predictivas
# Vamos a modificar los datos a ver si podemos mejorar un poco la situaciOn, o si, como lo 
# dice la teoria, la estandarizaciOn de los datos no tiene efecto en los Arboles de decisiOn
set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modeloStd <- train(LEAVE~., data = training, method = "rpart", #tuneLength=10,
                   preProcess = c("center","scale"),
                   tuneGrid=grid,
                   control = rpart.control(
                     minsplit = 2, #min instancias para intentar split de un nodo, por defecto es 20
                     minbucket = 1, #min instancias en un nodo hoja terminal
                     maxdepth = 3 #max depth, por defecto es 30
                     ))

modeloStd
rpart.plot(modeloStd$finalModel)     

# Se realizan predicciones con el mEtodo predict
predicciones <- predict(modeloStd, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE)

# QuE tanto nos aporta la normalizaciOn?
# ...


####################################################
# 5. Aprender modelo de manera mAs inteligente con caret: PODA

# Por defecto el mEtodo train utiliza mEtodos de resampleo para determinar el 
# valor Optimo del parAmetro de complejidad
# Como la BD esta bien balanceada, no nos aporta nada utilizar  Kappa como métrica de 
# clasificación, por lo que utilizaremos accuracy
set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modelo <- train(LEAVE~., data = training, method = "rpart")
modelo
modelo$bestTune   # el CP Optimo encontrado con la configuraciOn utilizada (la que viene por defecto)
modelo$finalModel # Explica el tipo de modelo aprendido
plot(modelo)      # Evolucion de la mEtrica de evaluaciOn segUn los valores de CP

# Se realizan predicciones con el mEtodo predict
predicciones <- predict(modelo, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE)

####################################################
# 6. Tuning de la bUsqueda de los parAmetros del algoritmo
#    De pronto los 3 intentos por defecto con valores distintos de cp son muy pocos

# Entrenar un modelo de árbol de decisión con 10 configuraciones de cp diferentes
set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
modelo <- train(LEAVE~., data = training, method = "rpart", tuneLength=10)
modelo
plot(modelo)

# Se realizan predicciones con el mEtodo predict
predicciones <- predict(modelo, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE)

####################################################
# 7. UtilizaciOn de CV con repeticiones para identificar el mejor valor de CP

# Vamos a blindarnos con respecto a la escogencia del cp, utilizando 
# un protocolo de cross validation con 5 folds, repetido 5 veces.
# También, como ya tenemos una idea más clara mAs o menos clara de cuAles son los valores de cp
# Optimos (cercanos a 0), vamos a definir especificamente los valores que queremos que se evaluen.
# Esto tambiEn nos permite estar mas seguros de la estimaciOn de la mEtrica
# de evaluaciOn utilizada (accuracy)
set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
trControlRepCv <- trainControl(method="repeatedcv", number=5, repeats=5)
grid <- data.frame(cp=c(0.002,0.004,0.005,0.006,0.007,0.008,0.009,0.012))
modeloRepCv <- train(LEAVE~., data = churn, method = "rpart",
                        tuneGrid=grid,
                        trControl=trControlRepCv)
modeloRepCv
plot(modeloRepCv)
# Esta estimaciOn del kappa y accuracy es mAs confiable, así como la obtención del parAmetro cp

# Se realizan predicciones con el mEtodo predict
predicciones <- predict(modeloRepCv, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE)

##############################################################################
# 8. Vamos ahora a abandonar el uso de los Arboles de decisiOn sencillos.
#    Vamos a aplicar ahora el mEtodo bagging con mUltiples Arboles de decisiOn.

#install.packages("ipred")
library(ipred)
set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
#Este metodo no tiene parAmetros a estimar
modeloBagg <- train(LEAVE~., data = churn, method = "treebag")

#Esto se demora .....
#...... en mim mAquina se demora 4:40

modeloBagg

predicciones <- predict(modeloBagg, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE)

# Que les parece? ....

# Increible


##############################################################################
# 9. Seguimos ahora con el boosting

#install.packages("ada")
library(ada)

set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo
# Especificamos los parametros internos del algoritmo, no queremos que los optimice, ya que
# esto tomarIa mucho tiempo
#  iter = 30, # numero de iteraciones (modelos), por defecto es 50
#  nu = 1, # learning rate (shrinkage parameter)
#  maxdepth = 3 #max depth, por defecto es 30
grid <- data.frame(maxdepth=5, nu=1, iter=50) 
trControlCv <- trainControl(method="cv", number=5)
modeloBoost <- train(LEAVE~., data = churn, method = "ada", 
                     trControl = trControlCv,
                     tuneGrid = grid)

#Esto se demora .....
#...... en mi mAquina se demora 3:30

modeloBoost

predicciones <- predict(modeloBoost, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE)

# Para que no se demorara limitamos demasiado los parametros del algoritmo

##############################################################################
# 10. Finalmente intentamos con random forest

#install.packages("randomForest")
library(randomForest)

set.seed(54321) # el valor de la semilla, en sI, no es importante, sOlo que se utilice siempre el mismo

#grid <- data.frame(mtry=3) 
trControlCv <- trainControl(method="cv", number=5)
modeloRf <- train(LEAVE~., data = churn, method = "rf", 
                     trControl = trControlCv)

#Esto se demora .....
#...... en mi mAquina se demora 6:20

modeloRf

predicciones <- predict(modeloRf, newdata=testing) #type "raw" por defecto
confusionMatrix(predicciones, testing$LEAVE)

# Maravilloso
