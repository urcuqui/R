library(ggplot2)
library(knitr)
str(diamonds)
grafica <- ggplot(diamonds, aes(x=diamonds$carat, 
                                y=diamonds$price))
grafica
# asigne el tipo de geometría a usar
grafica <- grafica + geom_point()
# muestre el resultado
grafica

# cree nuevamente el objeto ggplot con el dataset a mostrar. Si no lo crea nuevamente va a adicionar una capa sobre la anterior
grafica <- ggplot(diamonds, aes(x=diamonds$carat, y=diamonds$price))
# Asigne el campo del dataset que quiere usar para asignar color
grafica <- grafica + geom_point(aes(color = diamonds$cut, alpha = 0.3))
# muestre el resultado ... con facetas
grafica + facet_grid(. ~diamonds$cut)


#Diagrama de barras (Bar)

# Graficamente
grafica <- ggplot(diamonds, aes(x=diamonds$clarity))
grafica <- grafica + geom_bar()
grafica

#Histograma
# Graficamente
grafica <- ggplot(diamonds, aes(x=diamonds$price))
# Cambie el valor del ancho de la barra.
grafica <- grafica + geom_histogram(binwidth = 10)
grafica

#Diagrama de Caja y Bigote (Box Plot)
# Cree un lienzo con doc coordenadas, en el eje X debe ser catogorica
grafica <- ggplot(diamonds, aes(x=color, y=price))
# Haga el boxplot
grafica <- grafica + geom_boxplot()
grafica

# utilice facetas para ver todas las opciones . Debe usar variables categoricas.
grafica <- grafica + facet_grid(cut~.)
grafica

#Curva de densidades (density)
# Calcule el promedio
promZ <- mean(diamonds$z)
# Cree el lienzo
grafica <- ggplot(diamonds, aes(x=diamonds$z, fill="red", color = "red", alpha = 0.3))
# agrege la curva de desnsidad con color
grafica <- grafica + geom_density()
# Linea vertical del precio
grafica <- grafica + geom_vline(xintercept=promZ)
# Muestre el resultado
grafica


grafica <- ggplot(diamonds, aes(price, fill = cut, color = cut))
grafica <- grafica + geom_density(alpha = 0.1)
grafica


grafica <- ggplot(diamonds, aes(diamonds$x,diamonds$y))
grafica <- grafica + geom_raster(aes(z=diamonds$z))
grafica
