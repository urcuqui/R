ForeignAid <- read.csv('ForeignAid.csv')
ForeignAid$X <- NULL
columnas <- read.csv2('columnas.csv')
nombres <-lapply(columnas,as.character)
names(ForeignAid) <- nombres
str(ForeignAid)
library(reshape2)
ForeignAid2 <- melt (ForeignAid, id=c("country", "long", "lat", "program"))
str(ForeignAid2)
names(ForeignAid2) <- list("country","long","lat","program","year","investment")
library(ggplot2)
heatmap <- ggplot(ForeignAid2, aes(x=year, y=program, fill=investment))+ geom_tile()
ForeignAid3 <- ForeignAid2
ForeignAid3$invLog <- log10(ForeignAid2$investment)
heatmap2 <- ggplot(ForeignAid3, aes(x=year, y=program, fill=invLog))+ geom_tile()
NC <- subset(ForeignAid3, ForeignAid3$program == 'NC')
ggplot(NC, aes(x=year, y=country, fill=invLog)) + geom_tile()
NC2 <- na.omit(NC)
ggplot(NC2, aes(x=year,y=country, fill=invLog))+geom_tile()

#Mejorando las paletas de control
p <- ggplot(NC2, aes(x=year, y=country, fill=invLog))+geom_tile()
p + scale_fill_gradient2(low="darkorange",mid="grey96",high="darkolivegreen",midpoint = (NC2$invLog))

#Creando una paleta de colores divergente en el ColorSpace HCL

library(colorspace)
colores <- diverge_hcl(5)
p + scale_fill_gradientn(colours = c(colores))