library("sp")
library('rgdal')
library("rgeos")
library("raster")

rastSarea <- raster(ncol=20, nrow=40) ### this is a raster object
extent(rastSarea) <- c(0,20,0,40)
Sarea <- rasterToPolygons(rastSarea) ### this is a polygon object
rastSarea[] <- 1 
plot(rastSarea)
plot(Sarea)

#for creating individuals
x<- runif(30,min = 0, max =20)
y<- runif(30,min = 0, max = 40)
individuals<- points(x,y, pch = 16, col = "orange")

#for creating camera trap assay
rect(5,15,15,25)
points(5,15, pch = 1, col="black") # this is a very crude way, i think this can be done better in a loop
points(5,17, pch = 1, col="black")
points(5,19, pch = 1, col="black")
points(5,21, pch = 1, col="black")
points(5,23, pch = 1, col="black")
points(5,25, pch = 1, col="black")

points(7,15, pch = 1, col="black")
points(7,17, pch = 1, col="black")
points(7,19, pch = 1, col="black")
points(7,21, pch = 1, col="black")
points(7,23, pch = 1, col="black")
points(7,25, pch = 1, col="black")


points(9,15, pch = 1, col="black")
points(9,17, pch = 1, col="black")
points(9,19, pch = 1, col="black")
points(9,21, pch = 1, col="black")
points(9,23, pch = 1, col="black")
points(9,25, pch = 1, col="black")

points(11,15, pch = 1, col="black")
points(11,17, pch = 1, col="black")
points(11,19, pch = 1, col="black")
points(11,21, pch = 1, col="black")
points(11,23, pch = 1, col="black")
points(11,25, pch = 1, col="black")

points(13,15, pch = 1, col="black")
points(13,17, pch = 1, col="black")
points(13,19, pch = 1, col="black")
points(13,21, pch = 1, col="black")
points(13,23, pch = 1, col="black")
points(13,25, pch = 1, col="black")


points(15,15, pch = 1, col="black")
points(15,17, pch = 1, col="black")
points(15,19, pch = 1, col="black")
points(15,21, pch = 1, col="black")
points(15,23, pch = 1, col="black")
points(15,25, pch = 1, col="black")

