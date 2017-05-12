library(ggplot2)
library(knitr)
library(treemap)
str(mpg)
head(mpg)
mpg2008 <- subset(mpg,mpg$year == 2008)
mpg2008 <- mpg2008[complete.cases(mpg2008),]
head(mpg2008,3)
tree <- treemap(mpg2008, c("class", "manufacturer"), vSize = "hwy", vColor = "cty",
                fontsize.labels = 20, fontsize.title = 42)
treeHwy <- treemap(mpg2008, c("class", "manufacturer", "model"), vSize = "hwy",
                   vColor = "displ", palette = "Oranges", fontsize.labels = 20, fontsize.title = 42)
treeCty <- treemap(mpg2008, c("class", "manufacturer", "model"), vSize = "cty",
                   vColor = "displ", palette = "Oranges", fontsize.labels = 20, fontsize.title = 42)
treemap(mpg2008, c("cty", "manufacturer", "model"), vSize = "cty", vColor = "manufacturer",
        palette = "Spectral", fontsize.labels = 20, fontsize.title = 42)


#Organice el contenido de los treemap de forma que pueda responder si hay una correlación entre consumo en
#la ciudad, cilindraje, tipo de transmisión y número de cilindros. Explore el API de treeset y vea como puede
#asignar los colores de modo que sea de mayor provecho para reforzar su explicación.



treemap(mpg2008, c("trans", "cyl"), vSize="displ", vColor="cty", fontsize.labels = 20, fontsize.title = 42)
