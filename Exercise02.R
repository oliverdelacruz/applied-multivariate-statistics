#Exercise 2
#--------------------------------------------------------------------------------
# 1
library(mvoutlier)
d.ex1 <-  read.csv("http://stat.ethz.ch/Teaching/Datasets/typo.csv")
summary(d.ex1)
chisq.plot(d.ex1)
# 2
d.ex2 <- USArrests
summary(d.ex2)
class(d.ex2)
colnames(d.ex2)
d.ex2 <- d.ex2[,-3]
stars(d.ex2)
d.ex2["Rhode Island",]
m.ex2 <- as.matrix
apply(m.ex2,2,min)
which(d.ex2 ==min(d.ex2))
apply(m.ex2,2,max)
which(d.ex2 ==max(d.ex2))
d.ex2[34,]
range(d.ex2)
apply(m.ex2,2,range)
pca_corr <- princomp(m.ex2,cor=T)
pca_cov <- princomp(m.ex2,cor=F)
pca_corr$scores
pca_corr$loadings
plot(pca_corr$scores[,1],pca_corr$scores[,2],pch=20)
cor(pca_corr$scores[,1],pca_corr$scores[,2])
which(pca_corr$scores[,1]==max(pca_corr$scores[,1]))
which(pca_corr$scores[,2]==max(pca_corr$scores[,2]))
which(pca_corr$scores[,1]==min(pca_corr$scores[,1]))
which(pca_corr$scores[,2]==min(pca_corr$scores[,2]))
# Exercise 3
dataprotein<-read.table("https://www.polybox.ethz.ch/index.php/s/GZuyBkGS0p8LV2W/download",header=T)
row.names(dataprotein) <- dataprotein$Country
dataprotein <- dataprotein[,-1]
pca_protein <- princomp(dataprotein,cor=T)
pca_protein$loadings
screeplot(pca_protein)
# Exercise 4
centered_data <- dataprotein - apply(dataprotein,2,mean)
centered_data <-scale(dataprotein)
pcadata <- princomp(centered_data,cor=F)
apply(centered_data,2,mean)
var(centered_data)
cor(dataprotein)
# Exercise 5
d.iris<-read.table("ftp://stat.ethz.ch/Teaching/Datasets/WBL/iris2.dat", header=TRUE)
summary(d.iris)
data_iris <- d.iris[d.iris[,1]==1,2:3]
scale_data <- scale(data_iris,scale=F)
plot(scale_data)
covariance_data <- cov(scale_data)
eigen_decomp <- eigen(covariance_data)
eigen_decomp$vectors %*% t(eigen_decomp$vectors)
res <- scale_data %*% eigen_decomp$vectors
plot(res)
dim(res)

var(res)