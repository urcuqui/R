train <- read.csv(file.choose())
test <- read.csv(file.choose())

library(ggplot2)
library(lattice)
library(caret)
library(e1071)
library(knitr)


#train <- train[,-c(3,) ]

scatterPlot <- ggplot(mpg, aes(hwy, cty, color = mpg$class, size = mpg$displ,
                               alpha = 0.1))

table(train$Survived)

plot(as.factor(train$Survived))

# Pero de ahora en adelante usaremos la libreria ggplot2
ggplot(train, aes(as.factor(Survived), fill = as.factor(Survived))) + geom_bar() +
  labs(title = "Cantidad de sobrevivientes", x = "Sobrevivientes", y = "Personas",
       fill = "Muere o vive")

#Los mismos datos como proporciones
prop.table(table(train$Survived))

# Se crea una columna predicci<U+00F3>n
train$Prediction <- 0
# Confusion matrix. Recibe lo predicho y lo compara con los datos reales.
confusionMatrix(train$Prediction, train$Survived)

# Creacion de columna survived y la lleno con 0 para los 418 filas
test$Survived <- rep(0, 418)

resultados <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
# Creo el archivo con los resultados.
write.csv(resultados, file = "todosMueren.csv", row.names = FALSE)
# El archivo 'todosMueren.csv' se envia a Kaggle

ggplot(train, aes(Sex, fill = Survived)) + geom_bar() + labs(title = "Cantidad de sobrevivientes por sexo",
                                                             x = "Sexo", y = "Personas")
ggplot(train, aes(as.factor(Survived), fill = as.factor(Survived))) + geom_bar() +
  facet_grid(. ~ Sex) + labs(title = "Cantidad de sobrevivientes", x = "Sobrevivientes por sexo",
                             y = "Personas", fill = "Muere o vive")

summary(train$Sex)
# Muestra la proporcion con respecto al total de los datos
prop.table(table(train$Sex, train$Survived))
# Muestra la proporcion agregada por filas
prop.table(table(train$Sex, train$Survived), 1)


counts <- table(train$Survived, train$Sex)

# Calcula las proporciones de supervivencia a mano. Obtenemos los mismos
# datos que en el paso anterior.
counts[2]/(counts[1] + counts[2])
counts[4]/(counts[3] + counts[4])


# Hacemos un una nueva prediccion: Todos mueren:
train$Prediction <- 0
# Pero las muejres viven:
train$Prediction[train$Sex == "female"] <- 1
confusionMatrix(train$Prediction, train$Survived)

# Generamos el archivo para Kaggle
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
resultados <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(resultados, file = "mujeres.csv", row.names = FALSE)

head(train[is.na(train$Age), ])
nrow(train[is.na(train$Age), ])

ggplot(train, aes(Age)) + geom_histogram(bins = 20) + labs(title = "Edad de pasajeros",
                                                           x = "Edad", y = "Personas")
# Diferencia de edades por sexo
ggplot(train, aes(Age, fill = Sex)) + geom_histogram(bins = 20) + facet_grid(. ~
                                                                               Sex) + labs(title = "Edad de sobrevivientes", x = "Edad", y = "Personas",
                                                                                           fill = "Sexo")                                                                                         fill = "Sexo")

ggplot(train, aes(Age)) + geom_density() + geom_vline(xintercept = 18, color = "red") +
  labs(title = "Distribucion de pasajeros por edad", x = "Edad", y = "Densidad")

train$Child <- 0
# Todo pasajero < 18 es ni<U+00F1>o
train$Child[train$Age < 18] <- 1
train$Prediction <- 0
# todo nino vive
train$Prediction[train$Child == 1] <- 1
confusionMatrix(train$Prediction, train$Survived)


# Cuantos sobreviven?
aggregate(Survived ~ Child + Sex, data = train, FUN = sum)


# En gr<U+00E1>fica:
ggplot(train, aes(as.factor(Child), fill = as.factor(Survived))) + geom_bar() +
  facet_grid(Survived ~ Sex) + labs(title = "Comparacion de superviviencia entre ninos y adultos por sexo",
                                    x = "Nino o adulto", y = "Personas", fill = "Muere o vive")

# Que proporciones?
aggregate(Survived ~ Child + Sex, data = train, FUN = function(x) {
  sum(x)/length(x)
})


# PRECION DEL BILLETE
summary(train$Fare)

# table(train$Fare)
ggplot(train, aes(Fare)) + geom_histogram(binwidth = 10) + geom_vline(xintercept = 15,
                                                                      color = "red") + labs(title = "Distribucion de cantidades pagadas por pasajero",
                                                                                            x = "Valor", y = "Cantidad de pagos")
#Se crean bins para rangos de precios. Se deterimnan los rangos para
# distribuir la mayor cantidad de valores bajo la curva
train$Fare2 <- "30+"
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- "20-30"
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- "10-20"
train$Fare2[train$Fare < 10] <- "10 o menos"
ggplot(train, aes(Fare2)) + geom_bar() + labs(title = "Distribucion ajustada de cantidades pagadas",
                                              x = "Valor", y = "Cantidad de Pagos", fill = "Muere o vive")


table(train$Fare2)

# Clase
Pclass_survival <- table(train$Survived, train$Pclass)
ggplot(train, aes(Pclass)) + geom_bar(aes(fill = as.factor(Survived)), position = "dodge") +
  labs(title = "Superviviencia por clase", x = "Clase", y = "Personas", fill = "Muere o vive")

Pclass_survival[2]/(Pclass_survival[1] + Pclass_survival[2])


aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN = function(x) {
  sum(x)/length(x)
})

# Ajusto mi prediccion con base en lo anterior
train$Prediction <- 0
train$Prediction[train$Sex == "female"] <- 1
# Algunas mujeres mueren: Las que viajan en tercera clase y pagaron $20 o
# mas por su pasaje
train$Prediction[train$Sex == "female" & train$Pclass == 3 & train$Fare >= 20] <- 0
confusionMatrix(train$Prediction, train$Survived)

# Preparar el archivo para enviar a www.kaggle.com
test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
test$Survived[test$Sex == "female" & test$Pclass == 3 & test$Fare >= 20] <- 0
resultados <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(resultados, file = "mujeresDeClase.csv", row.names = FALSE)
