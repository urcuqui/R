library(ggplot2)
library(viridis)
str(mpg)

mpg2008 <- subset(mpg,mpg$year == 2008)
mpg2008 <- mpg2008[complete.cases(mpg2008),]
head(mpg2008,3)


# Creamos columna con secuencia
mpg2008$ID <- seq(1,nrow(mpg2008))
# Concatena nombre de modelo [1] con numero de sequencia [12]
mpg2008$ID <- do.call(paste0,mpg2008[c(1,12)])
# Asigna los nombres
rownames(mpg2008) <- mpg2008$ID
head(mpg2008,3)


# Normalizar columnas numericas
mpg2008N <- mpg2008
mpg2008N[,c(3,5,8,9)] <- scale(mpg2008[,c(3,5,8,9)])
head(mpg2008N,3)

d <- dist(mpg2008N, upper = TRUE)
str(d)

mtrx <- as.matrix(d)
heatmap(mtrx, keep.dendro = FALSE, symm = TRUE, revC = TRUE, col = heat.colors(100))

#Viridis
heatmap(mtrx, keep.dendro = FALSE ,symm = TRUE, revC = TRUE, col = viridis(100)) 

#Magma
heatmap(mtrx, keep.dendro = FALSE ,symm = TRUE, revC = TRUE, col = magma(100))

#Plasma
heatmap(mtrx, keep.dendro = FALSE ,symm = TRUE, revC = TRUE, col = plasma(100))

#Inferno
heatmap(mtrx, keep.dendro = FALSE ,symm = TRUE, revC = TRUE, col = inferno(100))

heatmap(mTwoSeater, revC = TRUE, na.rm = TRUE, symm = TRUE, col = heat.colors(50),
        column_title = "Two Seater")

heatmap(mCompact, revC = TRUE, na.rm = TRUE, symm = TRUE, col = heat.colors(50),
        column_title = "Compact")

heatmap(mMidsize, revC = TRUE, na.rm = TRUE, symm = TRUE, col = heat.colors(50),
        column_title = "Mid Size")

heatmap(mMinivan, revC = TRUE, na.rm = TRUE, symm = TRUE, col = heat.colors(50),
        column_title = "Mini Van")

heatmap(mPickup, revC = TRUE, na.rm = TRUE, symm = TRUE, col = heat.colors(50),
        column_title = "PickUp")

heatmap(mSubcompact, revC = TRUE, na.rm = TRUE, symm = TRUE, col = heat.colors(50),
        column_title = "Sub Compact")

heatmap(mSuv, revC = TRUE, na.rm = TRUE, symm = TRUE, col = heat.colors(50),
        column_title = "Sub Urban Vehicle")
