#Libraries:
library(ggplot2)
library(ggmap)
# IMPORTANT: For problems with ggmap install the latest version from devtools


# Quick plot of the City of Cali / Google Maps provider
qmap("Cali", zoom = 10)


# get_googlemap() can access Google Static Maps API and customize the map attributes. See:
# https://developers.google.com/maps/documentation/static-maps/
qmap("Cali", zoom = 10, maptype = "hybrid")


# Stamen and Cloudmade are map providers with attractive styles: terrain, watercolor and toner
qmap("Cali", zoom = 10, source = "stamen", maptype = "toner")

#CloudMade Maps require the user to register to obtain an API key and then pass the API key into
#get_map with the api_key argument.
# The same map form OpenStreet Maps provider
# qmap("Cali", zoom = 10, source = "osm")
# Markers on a map

#Load a map
city <- get_map("Cali", zoom = 12)
# With that data and the long/lat of the center you can get the ggplot2 object used by ggmap to plot the str(city)

str(city)
# Cali coords
CaliCoords <- geocode("Cali")
MioStops <- read.delim(file.choose())
Cali <- ggmap(city)

Cali <- ggmap(city)
Cali <- Cali + geom_point(aes(x= DECIMALLONGITUDE, y = DECIMALLATITUDE),data = MioStops, color = "red", alpha = 0.3)
Cali

cityCenter <- get_map("Cali", source= "stamen",maptype="toner", zoom = 15)
CaliCenter <- ggmap(cityCenter)
CaliCenter <- CaliCenter + geom_point(aes(x= DECIMALLONGITUDE, y = DECIMALLATITUDE),data = MioStops, color CaliCenter


cityCenter <- get_map(location = c(-76.52038157, 3.38468334 ),source= "stamen",maptype="toner", zoom = 15)
CaliCenter <- ggmap(cityCenter)
CaliCenter <- CaliCenter 
CaliCenter
