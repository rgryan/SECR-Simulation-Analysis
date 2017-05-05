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


xpoints<- c(5,5,5,5,5,5,7,7,7,7,7,7,9,9,9,9,9,9,11,11,11,11,11,11,13,13,13,13,13,13,15,15,15,15,15,15)
length(xpoints)
ypoints<- c(15,17,19,21,23,25,15,17,19,21,23,25,15,17,19,21,23,25,15,17,19,21,23,25,15,17,19,21,23,25,15,17,19,21,23,25)
length(ypoints)
camloc<- 1:36

camloc.mat<- matrix(nrow=36,ncol=2) #matrix storing camera locations
camloc.mat[,1]<-xpoints
camloc.mat[,2]<-ypoints

dist.mat<-rdist(camloc.mat,ind.mat) #matrix with distance between points

Go<- 0.2
sigma<- 2500
beta<- -1/(2*(sigma^2))

prob.mat<-range(Go*exp((dist.mat^2)*beta)) #probability matrix based on half normal function of distance values.

