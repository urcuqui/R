###########################################################################

###########################################################################



####################################################
# 1. Análisis exploratorio (básico)


#install.packages("kernlab")
#install.packages("e1071")
#install.packages("klaR")
library(kernlab) #incluye el dataset spam
library(caret)   #utilizado para administrar el proceso de ML
library(e1071)   #implementación sencilla de NB sin caret
library(klaR)    #implementacion de NB utilizada por caret

data(spam)
summary(spam)
str(spam)
dim(spam)
names(spam)

apply(spam, 2, class)

table(spam$type)
prop.table(table(spam$type))

spam$your

####################################################
# 2. Separar el dataset en training y test set usando caret

set.seed(1234)
inTrain <- createDataPartition(y=spam$type, p = .7, list = FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]

####################################################
# 3. Entrenar un modelo Naïve Bayes con el mEtodo naiveBayes
#    de la librerIa e1071

set.seed(3456)
modelo_e1071 <- naiveBayes(training[,-58], training$type)
summary(modelo_e1071)
modelo_e1071
class(modelo_e1071)

#Valores de probabilidad a priori
modelo_e1071$apriori
#Tablas con probabilidades condicionales (si predictores categóricos) o promedios y desviación estándar (si numéricas)
modelo_e1071$tables
names(modelo_e1071$tables)
modelo_e1071$tables$technology

predicciones <- predict(modelo_e1071, testing[,-58], type="raw")
predicciones
predicciones <- predict(modelo_e1071, testing[,-58])
predicciones
confusionMatrix(predicciones, testing$type)
#accuracy: 72.81%, kappa: 48.39%

####################################################
# 4. Entrenar un modelo Naïve Bayes directamente con el package klaR

set.seed(3456)
modelo_klar <- NaiveBayes(training[,-58], training$type, usekernal = FALSE, fL = 0)
modelo_klar

class(modelo_klar)
#Matriz de entrada con los datos
modelo_klar$x[1:8, 1:12]
#Valores de probabilidad a priori
modelo_klar$apriori
#Tablas con probabilidades condicionales (si predictores categóricos) o promedios y desviación estándar (si numéricas)
modelo_klar$tables
modelo_klar$varnames

predicciones <- predict(modelo_klar, testing[,-58], type="raw")
predicciones
confusionMatrix(predicciones$class, testing$type)
#accuracy: 72.81%, kappa: 48.39%

#Aquí todas las implementaciones básicas de NB tienen que dar lo mismo

####################################################
# 5. Entrenar un modelo Naïve Bayes en caret (method = "nb"). Internamente se
#    utiliza el metodo del package klaR

# Esto toma 3:15 minutos en mi equipo
set.seed(3456)
modelo_caret <- train(training[,-58], training$type, method = "nb")
modelo_caret
class(modelo_caret$finalModel)
modelo_caret$finalModel
modelo_caret$finalModel$varnames
modelo_caret$results
modelo_caret$bestTune

#El modelo tiene 3 parámetros a especificar:
# - fL: indica el valor de corrección de Laplace (fijo por defecto en 0 para Caret)
# - adjust: utilizado para el suavizamiento de la función no parámetrica de la distribución de 
#   probabilidad (fija por defecto en un valor de 1 para Caret)
# - usekernel: indica si se quiere utilizar una distribución de densidad normal o no parámetrica
#   (Caret intenta con los dos tipos de distribuciones: Normal y no parámetrica)
plot(modelo_caret)

predicciones <- predict(modelo_caret, testing[,-58])
confusionMatrix(predicciones, testing$type)
#accuracy: 72.81%, kappa: 48.39%


####################################################
# 6. Comparar con un modelo "knn" entrenado con caret
set.seed(3456)
modelo_knn <- train(training[,-58], training$type, method = "knn")
modelo_knn


predicciones <- predict(modelo_knn, testing[,-58])
predicciones
confusionMatrix(predicciones, testing$type)
#accuracy: 78.32%, kappa: 54.36%

########################################################

