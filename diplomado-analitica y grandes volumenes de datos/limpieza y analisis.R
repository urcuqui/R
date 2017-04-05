colnames(sb20121)


bd <- rbind(sb20121, sb20122)
bd <- cbind(sb20121, sb20122)
library(readxl)
quibdo <- read_excel("Sedes_quibdo.xlsx", col_names = TRUE)
colnames(quibdo)<-c("Secretaria", "C.departamento", "N.departamento", "C.municipio",
                    "N.municipio", "C.Establecimiento", "N.Establecimiento", "C.Sede",
                    "N.Sede", "Zona", "Direccion", "Telefono", "Estado.Sede", "Niveles",
                    "Modelos", "Grados")

quibdo_filter <- quibdo[,-c(1:3,6:7,11:12,14:16)]
quibdo_filter <- subset(quibdo, -c(1:3,6:7,11:12,14:16))
quibdo_filter2 <- quibdo_filter[quibdo_filter$Estado.Sede !="CIERRE DEFINITIVO" & 
                                   quibdo_filter$Estado.Sede!="CIERRE TROPICAL" & 
                                  quibdo_filter$Estado.Sede!="CIERRE TEMPORAL",]
levels(quibdo$Estado.Sede)
class(quibdo$Estado.Sede)
quibdo_filter3 <- quibdo_filter2
quibdo_filter3$Estado.Sede <- as.factor(quibdo_filter3$Estado.Sede)
levels(quibdo_filter3$Estado.Sed)

dim(quibdo_filter3)
dim(bd)

quibdo_estudiantes <- merge(quibdo_filter3,bd, by.x="C.Sede", by.y="COLE_COD_DANE_INSTITUCION", all.x = FALSE
                            , all.y = FALSE)
dim(quibdo_estudiantes)
quibdo_estudiantes$Estado.Sede <- NULL

#Frecuencia absoluta y relativa
table(quibdo_estudiantes$Zona)
prop.table(table(quibdo_estudiantes$Zona))

install.packages("plyr")
library(plyr)
quibdo_sedes <- ddply(quibdo_estudiantes, c("C.Sede"), summarise,
                      N = length(PUNT_MATEMATICAS),
                      meanmath = round(mean(PUNT_MATEMATICAS, na.rm = T),2),
                      meanleng = round(mean(PUNT_LENGUAJE, na.rm = T),2),
                      meaningles = round(mean(PUNT_INGLES, na.rm = T),2),
                      sdmath = round(sd(PUNT_MATEMATICAS, na.rm = T),2),
sdleng = round(sd(PUNT_LENGUAJE, na.rm = T),2),
sdingles = round(sd(PUNT_INGLES, na.rm = T),2),
estu_edad = round(mean(ESTU_EDAD, na.rm = T),2))
summary(quibdo_sedes)

####plot####
#histograma
hist(quibdo_sedes$meanmath, main="histograma promedio matecho", col.main="green", font.main=2,
     xlab = "Promedio de matecho", ylab="Frecuencia")
#bigotes
boxplot(quibdo_sedes$estu_edad, main="Caja de bigotes Edad", ylab="Edad")
#barplot
barplot(quibdo_sedes$N, main=" BarPlot", 
        Font.main=2, ylab="N")
source("diplomado-analitica y grandes volumenes de datos/support/outliers_script.R")

#analisis de outliers
outlierKD(quibdo_sedes, estu_edad)

#cuartiles
qnt <- quantile(quibdo_sedes$estu_edad, probs=c(.25,.75), na.rm=T)
qnt
H <- 1.5*IQR(quibdo_sedes$estu_edad, na.rm = T)
quibdo_sedes$atipicosedad <- ifelse(quibdo_sedes$estu_edad>qnt[2]+H | quibdo_sedes$estu_edad<qnt[1]-H,1,0)
quibdo_sedes$atipicosedad

cor(quibdo_sedes)

# analisis de tablas
tabla1<- table(quibdo_estudiantes$COLE_NATURALEZA, quibdo_estudiantes$COLE_CARACTER)
addmargins(tabla1)
prop.table(tabla1)


chisq.test(table(quibdo_estudiantes[,c(22,24)]))
fisher.test(table(quibdo_estudiantes[,c(13,16)]))

top10languaje <- quibdo_sedes[order(-quibdo_sedes[,4]),]
top10languaje <- top10languaje[c(1:10)]
top10languaje <- droplevels(top10languaje)


top10ingles <- quibdo_sedes[order(-quibdo_sedes[,5]),]
top10ingles <- top10ingles[c(1:10)]
top10ingles <- droplevels(top10ingles)

top10math <- quibdo_sedes[order(-quibdo_sedes[,3]),]
top10math <- top10math[c(1:10)]
top10math <- droplevels(top10math)

top10 <- merge(top10ingles[,c(1,5)],top10languaje[,c(1,4)], by="C.Sede")
top10 <- merge(top10,top10math[,c(1,3)], by="C.Sede")
top10
