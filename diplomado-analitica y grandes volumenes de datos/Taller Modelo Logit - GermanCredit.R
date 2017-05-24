

#install.packages("caret")
library(caret)
library(e1071)


data(GermanCredit)

summary(GermanCredit)
str(GermanCredit)

featurePlot(x=GermanCredit[,c("Duration","CreditHistory.NoCredit.AllPaid","Purpose.UsedCar","CheckingAccountStatus.none",
                              "Housing.Own")], y=GermanCredit$Class, plot="pairs")

####################################################
# 2. Separar el dataset en training y test set usando caret

set.seed(1234)
Train <- createDataPartition(GermanCredit$Class, p=0.8, list=FALSE)
training <- GermanCredit[ Train, ]
testing <- GermanCredit[ -Train, ]

####################################################
# 3. Entrenar un modelo por regresión lineal y predecir

training$Class2<-as.numeric(training$Class)
training$Class2[training$Class2==2]<-0
modelo_mpl<-lm(Class2~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training)
summary(modelo_mpl)

# Se realizan predicciones con el metodo predict
predicciones <- predict(object=modelo_mpl,testing[,-10]) #type "raw" por defecto
predicciones

predicciones_mpl<-ifelse(predicciones>0.5,"Good","Bad")

# Analizar una nueva instancia
nuevaInstancia <- data.frame(Duration=50,CreditHistory.NoCredit.AllPaid==1,Purpose.UsedCar==1,CheckingAccountStatus.none==0,Housing.Own==0)
predict(modelo_mpl, newdata = nuevaInstancia)

####################################################
# 7. Calcular la matriz confusión
confusionMatrix(data=predicciones_mpl, testing$Class)

####################################################
# 4. Entrenar un modelo por regresión logística y predecir

mod_logit <- train(Class ~ Duration+CreditHistory.NoCredit.AllPaid+Purpose.UsedCar+CheckingAccountStatus.none+Housing.Own,  data=training, method="glm", family="binomial")

Class<-NULL
# Se realizan predicciones con el mEtodo predict
predicciones <- predict(mod_logit, testing[,-10]) #type "raw" por defecto
predicciones
prediccionesProb <- predict(mod_logit, testing[,-10], type="prob")
prediccionesProb

####################################################
# 5. Analizar una nueva instancia
nuevaInstancia <- data.frame(Duration=50,CreditHistory.NoCredit.AllPaid==1,Purpose.UsedCar==1,CheckingAccountStatus.none==0,Housing.Own==0)
predict(mod_logit, newdata = nuevaInstancia)

####################################################
# 6. Interpretar coeficientes
exp(coef(mod_logit$finalModel))

####################################################
# 7. Calcular la matriz confusión
confusionMatrix(data=predicciones, testing$Class)
