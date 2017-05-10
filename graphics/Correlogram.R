library(ggplot2)
library(corrplot)
str(diamonds)

# actual cut values
head(diamonds$cut)

# coersed cut values
head(as.integer(diamonds$cut))

# actual color values. D is better than J, therefore we need to invert the scale
head(diamonds$color)

# corsed color values
head(as.integer(diamonds$color))


# Now duplicate dataset to preserve the original and change the factor for numbers
diamondsNew <- diamonds
diamondsNew$cut <- as.integer(diamondsNew$cut)
diamondsNew$color <- as.integer(diamondsNew$color)
diamondsNew$clarity <- as.integer(diamondsNew$clarity)


mcor <- cor(diamondsNew)
head(mcor, 2)

corrplot(mcor)

# ordered values
# The values are ordered using Angular Order of Eigenvectors (AOE)
corrplot(mcor, method = "shade", order= "AOE", addCoef.col = "azure4", shade.col = NA)
