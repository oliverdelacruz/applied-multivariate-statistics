library(MASS)
library(cluster)
rm(list=ls())
cat("\014")  

# Load data
load(url("https://polybox.ethz.ch/index.php/s/qkeOhfnzmbfqq2p/download"))
cor.4.7
colnames(cor.4.7)<-rownames(cor.4.7)<-c("SkillDoct","Myself","DoctDoes",
                                          "MedAdvice","ExercFood","Careless","Responsible","ReliefDoct","Lucky")
# Principal component
pca <- eigen(cor.4.7)
var_explained <- cumsum(pca$values)/sum(pca$values)
plot(var_explained)

# Factor analysis
factor_analysis <- factanal(factors = 2,covmat = cor.4.7,n.obs = 123 )
pca$vector[,1:2]
factor_analysis_oblique <- factanal(factors = 2,covmat = cor.4.7,n.obs = 123, rotation = "promax"  )
factor_analysis_ortho <- factanal(factors = 2,covmat = cor.4.7,n.obs = 123, rotation =  "varimax" )

# Exerice 2
t.url <- "http://stat.ethz.ch/Teaching/Datasets/WBL/empl2.dat"
empl <- read.table(t.url, header=T)
labempl <- rownames(empl)
pairs(empl, panel=function(x,y) text(x,y, labels=labempl, xpd=T))

# Calculate euclidean distance
eu_dist <-  dist(empl)
ob.clust.single <- hclust(eu_dist, method= "single")
plot(ob.clust.single)
ob.clust.average <- hclust(eu_dist, method= "average")
plot(ob.clust.average)

# Classification in three groups using Single Linkage
r.3scl <- cutree(ob.clust.single, k=3)
split(labempl, r.3scl)
# MDS-Plot:
r.mds <- cmdscale(eu_dist)
plot(r.mds, type = "n", asp=1, main = "Single clustering, MDS coordinates")
text(r.mds, labempl, col = 1 + r.3scl)

# Exercise 3
d.bank.org <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/banknot.dat")
d.bank <- d.bank.org[,c("CODE","BOTTOM","DIAGONAL")]
obj_kmeans <- kmeans(d.bank[,2:3],2)
classification_error <- obj_kmeans$cluster == (d.bank[,1]+1)
t.bank <- dist(d.bank,method="euclidean")
kmean.sl <- silhouette(obj_kmeans$cluster,t.bank)
plot(kmean.sl)
obj_pam <- pam(d.bank[,2:3],2)
classification_error_pam <- obj_pam$clustering == (d.bank[,1]+1)
plot(obj_pam, which=2)
plot(obj_pam , which=1) 
plot(d.bank$BOTTOM,d.bank$DIAGONAL,col= as.integer(classification_error_pam )+2,pch = obj_pam$clustering +20)
obj_pam$medoids

set.seed(10)
t.kmeans <- NULL
for (i in 1:100){
  kmean.clustersizes <- kmeans(d.bank[,-1],centers=3)$size
  t.kmeans <- rbind(t.kmeans,sort(kmean.clustersizes))
}
clustersizes <- apply(t.kmeans, 1, paste, collapse=" ")
table(clustersizes)
## or
ones <- rep(1,100)
m.kmeans <- aggregate(ones,by=list(t.kmeans[,1],t.kmeans[,2],t.kmeans[,3]),  FUN=sum)
set.seed(10)
pam.kmeans <- NULL
for (i in 1:100){
  pam.clustersizes <- table(pam(d.bank[,2:3],3,medoids=sample(1:200,3))$clustering)
  pam.kmeans <- rbind(pam.kmeans,sort(pam.clustersizes))
}
clustersizes <- apply(pam.kmeans, 1, paste, collapse=" ")
table(clustersizes)
## or
ones <- rep(1,100)
m.kmeans <- aggregate(ones,by=list(t.kmeans[,1],t.kmeans[,2],t.kmeans[,3]),  FUN=sum)

# Exercise 4
load(url("https://polybox.ethz.ch/index.php/s/yuasx6cK4RmILCp/download"))
pca <- princomp(dat)
plot(pca$scores[,1],col =cl +1)

res <- lda(dat,cl)
plot(res)