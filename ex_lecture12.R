setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")

#############
## ISOMAP  ##
#############

##Load Swissroll data
require("rgl")
library("KODAMA")
n=1000
x=swissroll(n)
colpts=rainbow(n)
head(x)
dim(x)

##visualize data
open3d()
plot3d(x, col=colpts,box=FALSE,size=5)

##Apply ISOMAP
# source("https://bioconductor.org/biocLite.R")
# biocLite("RDRToolbox")
library("RDRToolbox")
sr_imap = Isomap(data=x, dims=2, k=10)##Takes some time (1-2 min on a desktop computer)
str(sr_imap)
plot(sr_imap$dim2,col=colpts,xlab="",ylab="")

sr_imap_plot = Isomap(data=x, dims=1:3, k=10,plotResiduals=TRUE)##Takes some time (1-2 min on a desktop computer)



############
## t-SNE  ##
############

###### Load digits data
digits=read.csv("digits.csv",head=FALSE)
dim(digits)
digits[1:5,1:10]

coldigits=rainbow(10)[digits[,1]+1]
x=as.matrix(digits[,-1])##convert to matrix for Isomap function

##tSNE
library(Rtsne)
sr_tsne <- Rtsne(x, dims=2, max_iter=1000)
##Alternatively (but slower, use rsne library)
# library(tsne)
# digits_tsne <- tsne(x, k=2, max_iter=1000)

plot(sr_tsne$Y[,1], sr_tsne$Y[,2], col=coldigits,xlab="",ylab="")
legend("topright",legend=0:9,col=rainbow(10),bty = "n",pch=1,pt.lwd=2)



#############
## DBSCAN  ##
#############

## Load data
x=read.csv(file="chameleon.csv", header = FALSE)
plot(x)

##k-means
##Determine number of clusters
kmax=20
wss <- rep(0, kmax)
wss[1] <- (dim(x)[1] - 1) * sum(apply(x, 2, var))
for (i in 2:kmax) wss[i] <- sum(kmeans(x, centers = i)$withinss)
plot(1:kmax, wss, type="l")
##5 or 2 seems ok

##Results for k=2
km <- kmeans(x, centers=2, nstart = 10)
plot(x, col=km$cluster)
##Results for k=5
km <- kmeans(x, centers=5, nstart = 10)
plot(x, col=km$cluster)

##Gaussian mixtures
library(mclust)
gm=Mclust(x,modelName = "VVV") ##takes some time (approx. 2 mins on a desktop computer)
plot(gm,what="classification",xlab="",ylab="",main="")

##Agglomerative clustering
library("MVA")
dis <- dist(x)
cc <- hclust(dis, method = "average") 

## Dendogram
plot(cc)
abline(h=100,col=2)
grps=cutree(cc, h=100)

plot(x, col=grps)


##DBSCAN
library(dbscan)
kNNdistplot(x, k = 4)##->eps=5
abline(h=5, col = "red", lty=2)
minPts=4
eps=5


dbs <- dbscan(x, eps = eps, minPts = minPts)
plot(x, col=(dbs$cluster+1))
##Number of clusters:
max(dbs$cluster)
## Too many clusters -> increase minPts

kNNdistplot(x, k = 20)##->eps=10
abline(h=10, col = "red", lty=2)
minPts=20
eps=10


dbs <- dbscan(x, eps = eps, minPts = minPts)
plot(x, col=(dbs$cluster+1))
##number of clusters
max(dbs$cluster)


