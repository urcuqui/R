mean(iris$Petal.Width)
load("anscombe.csv")
anscombe$Set <- as.factor(anscombe$Set)
str(anscombe)
uno <- subset(anscombe, Set=="1")
dos <- subset(anscombe, Set=="2")
tres <- subset(anscombe, Set=="3")
cuatro <- subset(anscombe, Set=="4")

XProm <- mean(uno$X)
YProm <- mean (uno$Y)
Xvar <- var(uno$X)
Yvar <- var(uno$Y)
Corr <- cor(uno$X, uno$Y)
# Se guardan en un dataset
SummaryStats <- data.frame(XProm, YProm, Xvar, Yvar, Corr)
SummaryStats


calc_Stats <- function (subS,df){
  xM <- mean(subS$X)
  yM <- mean (subS$Y)
  xV <- var(subS$X)
  yV <- var(subS$Y)
  cSet <- cor(subS$Y, subS$X)
  suma <- data.frame(xM, yM, xV, yV, cSet)
  colnames(suma) <- colnames(df)
  df <- rbind(df,suma)
  return(df)
}

# Usamos la funcion con cada subconjunto
SummaryStats <- calc_Stats(dos,SummaryStats)
SummaryStats <- calc_Stats(tres,SummaryStats)
SummaryStats <- calc_Stats(cuatro,SummaryStats)

SummaryStats

library(ggplot2)
# Canvas sobre el que vamos a dibujar
plotAns <- ggplot(anscombe,aes(X,Y, color = Set))
# Diagrama de lineas
plotAns <- plotAns + geom_line()
# Regresion lineal
plotAns <- plotAns + geom_smooth(method=lm, se=FALSE)
# promedio X
plotAns <- plotAns + geom_vline (aes ( xintercept = SummaryStats[1,1]))
# promedio Y
plotAns <- plotAns + geom_hline (aes ( yintercept = SummaryStats[1,2]))
# facetas
#plotAns <- plotAns + facet_grid(. ~ Set)
plotAns



library(ggplot2)
# Canvas sobre el que vamos a dibujar
plotAns <- ggplot(anscombe,aes(X,Y))
# Diagrama de dispersion
plotAns <- plotAns + geom_point()
# Regresion lineal
plotAns <- plotAns + geom_smooth(method=lm, se=FALSE)
# promedio X
plotAns <- plotAns + geom_vline (aes ( xintercept = SummaryStats[1,1]))
# promedio Y
plotAns <- plotAns + geom_hline (aes ( yintercept = SummaryStats[1,2]))
# facetas
plotAns <- plotAns + facet_grid(. ~ Set)
plotAns




