knitr::opts_chunk$set(fig.width=15, fig.height=20, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)
library(ggplot2)
library(ggdendro)
library(protoclust)
str(mpg)


mpg2008 <- subset(mpg,mpg$year == 2008)
mpg2008 <- mpg2008[complete.cases(mpg2008),]
head(mpg2008,3)


mpg2008$ID <- seq(1,nrow(mpg2008))
mpg2008$ID <- do.call(paste0,mpg2008[c(1,12)])
rownames(mpg2008) <- mpg2008$ID
head(mpg2008,3)


mpg2008N <- mpg2008
mpg2008N[,c(3,5,8,9)] <- scale(mpg2008[,c(3,5,8,9)])
head(mpg2008N,3)

d <- dist(mpg2008N)
str(d)

hc2008 <- hclust(d)
str(hc2008)

p <- ggdendrogram (hc2008, rotate=TRUE, size=2)
p


dhc2008 <- as.dendrogram(hc2008)
# Obtenga la informaci<U+00F3>n, lineas son ortogonales
dhc_data2008 <- dendro_data(dhc2008, type = "rectangle")
# Estructura de dhc_data2008
str(dhc_data2008)


knitr::opts_chunk$set(fig.width = 15, fig.height = 10, fig.path = "Figs/", echo = TRUE,
                      warning = FALSE, message = FALSE)

p <- ggplot(segment(dhc_data2008)) + geom_segment(aes(x = x, y = y, xend = xend,
                                                      yend = yend, color = as.factor(xend)))
p <- p + coord_flip()
# quite el background
p <- p + theme_dendro()
p


# Function to color branches
colbranches <- function(n, col) {
  a <- attributes(n) # Find the attributes of current node
  # Color edges with requested color
  attr(n, "edgePar") <- c(a$edgePar, list(col = col, lwd = 2))
  n # Don't forget to return the node!
}


# Color the first sub-branch of the first branch in red, the second
# sub-branch in orange and the second branch in blue
dhc2008[[1]][[1]] = dendrapply(dhc2008[[1]][[1]], colbranches, "red")
dhc2008[[1]][[2]] = dendrapply(dhc2008[[1]][[2]], colbranches, "orange")
dhc2008[[2]][[2]][[1]][[2]][[2]] = dendrapply(dhc2008[[2]][[2]][[1]][[2]][[2]],
                                              colbranches, "blue")
# Plot
plot(dhc2008)


library(protoclust)
dist2 <- dist(mpg2008N)
protoC <- protoclust(dist2)

# k corresponde al numero de prototipos que se quieren obtener
grupos <- protocut(protoC, k=15)


# Crea el gráfico
plotwithprototypes(protoC, imerge = grupos$imerge, col=2, hang = 0) 
dhc2008[[1]][[1]] = dendrapply(dhc2008[[1]][[1]], colbranches, "red")

