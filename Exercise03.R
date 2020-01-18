# Series 03
rm(list=ls())
cat("\014")
# 1 - Exercise
load(url("http://stat.ethz.ch/Teaching/Datasets/WBL/countries.rda"))
# scale
xs <- scale(mat)
# distance
dist_mat <- dist(xs)
pol.mds <- cmdscale(dist_mat, k = 2, eig = TRUE)
# plot 
x <- pol.mds$points
plot(-x[,1], -x[,2])
text(-x[155,1], -x[155,2], col= "red", cex = 0.7)
# identify
identify(x[,1], x[,2])
pca_corr <- princomp(xs,cor=T)
plot(pca_corr$score[,1],pca_corr$score[,2])
plot(pca_corr$score[,2],pca_corr$score[,1])
# Exercise 2
t.url <- "http://stat.ethz.ch/Teaching/Datasets/WBL/ch-dist.dat"
chdist <- read.table(t.url, header=T)
library(MASS)
coord <- cmdscale(chdist)
plot(coord[,1], coord[,2],type="p")
text(coord[,1], coord[,2], labels = rownames(x), cex = 0.5)
f.dist1 <- c(as.matrix(coord))
f.dist2 <- c(as.matrix(dist(chdist)))
p.p1 <- plot(f.dist2~f.dist1)
res <- isoMDS(as.matrix(chdist))
res$points
# Exercise 3
load(url("https://polybox.ethz.ch/index.php/s/mtQnbb3T3QS1pqe/download"))
rownames(cor.4.6)=colnames(cor.4.6)=c("French","English","History","Arithmetric","Algebra","Geometry")
cor.4.6
fact_cor <- factanal(factors = 2, covmat=cor.4.6)
res <- varimax(fact_cor$loadings)
mat <- as.matrix(fact_cor$loadings[,1:2])
dim(mat)
dim(res$rotmat)
mat %*% res$rotmat
as.matrix(res$loadings[,1:2]) %*% as.matrix(res$loadings[,1:2])

     