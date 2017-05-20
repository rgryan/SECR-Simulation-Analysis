library("sp")
library('rgdal')
library("rgeos")
library("raster")
library("fields")

rastSarea <- raster(ncol=20, nrow=40) ### this is a raster object
extent(rastSarea) <- c(0,20,0,40)
Sarea <- rasterToPolygons(rastSarea) ### this is a polygon object
rastSarea[] <- 1 
plot(rastSarea)
plot(Sarea)

set.seed(1234)

#for creating 30 individuals
ind.mat<- matrix(nrow=30,ncol=2)
x<- runif(30,min = 0, max =20)
y<- runif(30,min = 0, max = 40)
individuals<- points(x,y, pch = 16, col = "orange")
id<-1:30
ind.mat[,1]<- x #matrix for individual locations
ind.mat[,2]<-y


#for creating camera trap assay, 36 camera locations
rect(5,15,15,25)## marks out areas
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
sigma<- 2.5
beta<- -1/(2*(sigma^2))

prob.mat<-Go*exp((dist.mat^2)*beta) #probability matrix based on half normal function of distance values.
range(Go*exp((dist.mat^2)*beta))



capthist.mat<- matrix(nrow = nrow(prob.mat), ncol = ncol(prob.mat)) 

  for (i in 1:nrow(prob.mat)){
    for(j in 1:ncol(prob.mat)) 
      capthist.mat[i,j]<-rbinom(1,1,prob.mat[i,j])
  
}

capthist.array<- array(dim=c(36,30,10))

for(k in 1:10){

for(i in 1:36){
    
for(j in 1:30){ capthist.array[i,j,k]<-rbinom(1,1,prob.mat[i,j])
}}}

capthist.array

?as.data.frame.array

df<- as.data.frame.table(capthist.array)

dam<-subset(df, Freq==1)
colnames(dam)<- c("Individuals", "Detector", "Occasion", "Capture")
head(dam)

library("plyr")
library("reshape2")
dam1<- melt(capthist.array)
colnames(dam1)<- c("Detector", "Individual", "Occasion", "Capture")
head(dam1)
dam2 <- subset(dam1, Capture == 1) #
nrow(dam1)
nrow(dam2)
head(dam2)
capt.data<- dam2 
capt.data<- capt.data[,c(2,1,3,4)]
capt.data<- capt.data[,-4]
head(capt.data) #capture histroy in the form required for SECR
nrow(capt.data)
capt.data$sessionId<- rep("S1",127)
summary(capt.data)
capt.data<- capt.data[,c(4,2,1,3)]
head(capt.data)
capt.data<-capt.data[,c(1,3,4,2)]
head(capt.data)


det.data<- as.data.frame(camloc.mat)
det.data$detector<- 1:36
colnames(det.data)<- c("x","y", "Detector")
det.data
det.data<-det.data[,c(3,1,2)]# detection data in the form of detector layout format
head(det.data)
det.data<- det.data[,-1]
head(det.data)

det.obj<- read.traps(data = det.data)
capt.obj<- make.capthist(capt.data, det.obj)
secr.fit(capt.obj)


#occasions<- 10

#for(i in 1:nrow(det.data)){
 # occ <- rep(1,occasions)
  #cat(occ,"\n",sep = "",file = "Activetraps.txt",append = T)
#}

#traphist <- read.table("Activetraps.txt",header = F,sep="")
#det.data <- cbind(det.data,traphist)
#write.table(det.data,"Locations.txt",sep="\t",row.names = F,col.names = F,quote = F)

#library('secr')
#colnames(det.data) <- c("x", "y")

#det.obj<- read.traps(data = det.data)

#write.table(det.data, file = "det.data.txt", sep = ",", col.names = colnames(det.data))

#det.data2 <- read.table(file = "det.data.txt",sep = ",", header = TRUE, stringsAsFactors = FALSE) 

#det.obj<- read.traps(data = "det.data.txt")

#?array
#det.data<- det.data[,-1]
#det.array<- array(data = det.data, dim = length(det.data))
#det.obj<- read.traps(data = det.array)
