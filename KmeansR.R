library(ggplot2)
a <- read.delim(file='csv/movies.dat')
b <- transform(a, rtAllCriticsRating= as.numeric(rtAllCriticsRating),rtAllCriticsNumReviews= as.numeric(rtAllCriticsNumReviews), rtAllCriticsNumFresh = as.numeric(rtAllCriticsNumFresh), rtAllCriticsNumRotten = as.numeric(rtAllCriticsNumRotten), rtAllCriticsScore = as.numeric(rtAllCriticsScore), rtTopCriticsRating = as.numeric(rtTopCriticsRating), rtTopCriticsNumReviews= as.numeric(rtTopCriticsNumReviews), rtTopCriticsNumFresh = as.numeric(rtTopCriticsNumFresh),rtTopCriticsNumRotten=as.numeric(rtTopCriticsNumRotten), rtTopCriticsScore = as.numeric(rtTopCriticsScore), rtAudienceRating=as.numeric(rtAudienceRating), rtAudienceNumRatings = as.numeric(rtAudienceNumRatings), rtAudienceScore = as.numeric(rtAudienceScore), rtPictureURL= as.numeric(rtPictureURL))
mSet <- b[,8:21]
str(mSet)
normaliza =  function(df, rangos){
  as.data.frame(
    Map(function(columna, rango){
      # En este caso, los atributos constantes los dejamos sin modificar
      if(is.numeric(columna) & rango[2]>rango[1]) 
        (columna-rango[1])/(rango[2]-rango[1])
      else columna},
      df,
      rangos)
  )
}
View(b)
mSet$rtAllCriticsRating<-scale(mSet$rtAllCriticsRating)
mSet$rtAllCriticsNumReviews<-scale(mSet$rtAllCriticsNumReviews)
mSet$rtAllCriticsNumFresh<-scale(mSet$rtAllCriticsNumFresh)
mSet$rtAllCriticsNumRotten<-scale(mSet$rtAllCriticsNumRotten)
mSet$rtAllCriticsScore<-scale(mSet$rtAllCriticsScore)
mSet$rtTopCriticsRating<-scale(mSet$rtTopCriticsRating)
mSet$rtTopCriticsNumReviews<-scale(mSet$rtTopCriticsNumReviews)
mSet$rtTopCriticsNumFresh<-scale(mSet$rtTopCriticsNumFresh)
mSet$rtTopCriticsNumRotten<-scale(mSet$rtTopCriticsNumRotten)
mSet$rtTopCriticsScore<-scale(mSet$rtTopCriticsScore)
mSet$rtAudienceRating<-scale(mSet$rtAudienceRating)
mSet$rtAudienceNumRatings<-scale(mSet$rtAudienceNumRatings)
mSet$rtAudienceScore<-scale(mSet$rtAudienceScore)
mSet$rtPictureURL<-scale(mSet$rtPictureURL)
str(mSet)
kData <- kmeans(mSet, 5)
str(kData)
head(kData$cluster, 200)

mSet$cluster <- kData$cluster
mSet$year <- movies$year

#Falta quitar los valores atipicos

kPlot <- ggplot(mSet,aes(rtAllCriticsRating,rtAudienceRating, color = factor(cluster)) )
kPlot <- kPlot + geom_point(position = position_jitter())
kPlot


kPlot_two <- ggplot(mSet,aes(rtAllCriticsRating,rtAudienceRating), color = factor(mSet$cluster))
kPlot_two <- kPlot + geom_point(position = position_jitter())
kPlot_two <- kPlot + facet_grid(. ~ cluster)
kPlot_two

mCluster <- ggplot(mSet[mSet$cluster == 1, ],aes(rtAllCriticsRating,rtAudienceRating)) 
mCluster <- mCluster + geom_point(color = "red", position = position_jitter())
mCluster <- mCluster + geom_density2d(aes(alpha=..level.., fill=..level..),size = 1.5, bins=6) 
mCluster

install.packages(markdown)
