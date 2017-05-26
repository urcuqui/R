
###########################################################################

library(ggplot2)

####################################################
# 1. Análisis exploratorio bAsico

# La base de datos a estudiar viene con los paquetes de base de R
?USArrests
head(USArrests)
estados <- row.names(USArrests)
summary(USArrests)

#QuE pueden decir de los datos, de los diferentes tipos de crimenes y de la poblaci?n?

apply(USArrests, 2, mean)
apply(USArrests, 2, var)

#Que atributo aporta la mayor cantidad de informaci?n?
#...

####################################################
# 2. Creación de los componentes principales

pcomp <- prcomp(USArrests)

# QuE pueden decir de los loadings de los componentes principales?
# Sirvió de algo? Por qué?

names(pcomp)
pcomp$center #promedios de las dimensiones originales
pcomp$scale  #desviacion estAndar de las dimensiones originales
pcomp$rotation #loadings

pcomp$x  #los datos originales representados en el nuevo espacio dimensional de los PCs
dim(USArrests)
dim(pcomp$x)
apply(pcomp$x, 2, mean)
apply(pcomp$x, 2, var)
sum(apply(pcomp$x, 2, var))

# Por defecto los datos se centran (ver ayuda del mEtodo prcomp), pero no se escalan
# Los datos deben ser completamente estandarizados

pcomp <- prcomp(USArrests, scale=TRUE)
pcomp

names(pcomp)
pcomp$center #promedios de las dimensiones originales
pcomp$scale  #desviación estándar de las dimensiones originales
pcomp$rotation #loadings

pcomp$x  #los datos originales representados en el nuevo espacio dimensional de los PCs
apply(pcomp$x, 2, mean)
apply(pcomp$x, 2, var)
sum(apply(pcomp$x, 2, var))

apply(pcomp$x, 2, function(x) {var(x)/4})

####################################################
# 3. Análisis del significado de los ejes de los PCs

#veamos como quedan los datos en el nuevo sistema dimensional
biplot(pcomp) 

#para que los vectores de los loadings sean mas f?ciles de interpretar, vamos a rotarlos 180 grados
pcomp$x=-pcomp$x
pcomp$rotation = -pcomp$rotation
biplot(pcomp) 

pcomp$rotation
# PC1: los loadings de Rape, Assault y Murder son bastante altos, se puede decir que se trata del 
#      eje que mide el nivel de criminalidad de cada ciudad
#      Estados como Florida, New Mexico, Nevada, California tienen altos niveles de criminalidad.
#      Estados como NorthDakota, New Hampshire, Vermont, tienen bajos niveles de criminalidad.
# PC2: los loadings de UrbanPop son muy altos, por lo que este eje representa el tamaño de las ciudades
#      Se puede interpretar libremente que a mayor poblaciOn, menor el efecto de los asesinatos
#      Estados como California, Hawaii, New Jersey tienen altos porcentajes de poblaciOn urbana
#      Estados como Mississippi, North Carolina, South Carolina, tienen bajos niveles de poblaciOn urbana.

ggplot(data=as.data.frame(pcomp$x), aes(x=PC1, y=PC2)) + geom_point(size=2, alpha=.5) +
  geom_text(aes(label=estados, hjust=-0.1))

####################################################
# 4. Efecto de la escala: quE hubiera pasado si no la tenemos en cuenta
pcompSinEscala <- prcomp(USArrests, scale=FALSE)
par(mfrow=c(1,2))
biplot(pcomp) 
biplot(pcompSinEscala) 

pcompSinEscala$rotation
apply(USArrests, 2, var)

#Cada variable original se lleva casi todo un PC, ordenadas por la varianza original

par(mfrow=c(1,1))

####################################################
# 5. Analisis de la cantidad de informaciOn de los PCs

varianzasPC <- pcomp$sdev^2
porcentajeInfoPC <- varianzasPC / sum(varianzasPC)
porcentajeInfoPC
#El primer eje representa el 62% de toda la información
#El segundo eje representa el 24.7% de toda la información
#Entre ellos dos tenemos el 86.7% de toda la información

dfPorcentajes = data.frame(PC=1:4, simple=porcentajeInfoPC, acumulado=cumsum(porcentajeInfoPC))
ggplot(data=dfPorcentajes, aes(x=PC, y=simple))+
  labs(title="Porcentaje de información de cada PC", x="PC", y="Porcentaje") + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_point(size=2)

#Truco para recomponer un dataframe fácilmente y poder visualizar varios tipos de datos
#install.packages("reshape2")
library(reshape2)
names(dfPorcentajes)
dfPorcentajes2 <- melt(dfPorcentajes, id.vars="PC", variable.name="Tipo", value.name="Porcentaje")
ggplot(data=dfPorcentajes2, aes(x=PC, y=Porcentaje, shape=Tipo, colour=Tipo))+
  labs(title="Porcentaje de información de cada PC", x="PC", y="Porcentaje") + 
  scale_y_continuous(limits = c(0, 1)) +
  geom_point(size=2)



