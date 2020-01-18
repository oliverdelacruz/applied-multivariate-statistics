setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")
library(MVA)

#####################
## Classical MDS ####
#####################

## US cities pollution
dat <- read.csv(file = "USairpollution.csv", header = TRUE, row.names = 1)
dim(dat)
head(dat)
summary(dat)
boxplot(dat)
## scale
xs <- apply(dat, 2, function(x) (x - min(x))/(diff(range(x))))
head(xs)

## Fit Classical MDS (2 dimensions)
poldist <- dist(xs)
pol.mds <- cmdscale(poldist, k = 2, eig = TRUE)

## plot
x <- pol.mds$points
plot(x[,1], x[,2], type = "n")
text(x[,1], x[,2], labels = rownames(x), cex = 0.7)

## Fit again, but this time with more checks:
## check eigenvalues: Is inner product matrix pos. semidef.?
pol.mds <- cmdscale(poldist, k = 40, eig = TRUE)
ev <- pol.mds$eig

## Choose number of dimensions
cumsum(ev) / sum(ev)
cumsum(abs(ev)) / sum(abs(ev))
cumsum(ev^2) / sum(ev^2) 
## =>two or three dimensions should be fine

##Scree plot: elbow after 4 or 5?
plot(1:20,ev[1:20],ylab="eigenvalue",xlab="")

## Refit with chosen number of dimensions
pol.mds <- cmdscale(poldist, k = 2, eig = TRUE)
pol.mds

## Plot results
x <- pol.mds$points
plot(x[,1], x[,2], type = "n")
text(x[,1], x[,2], labels = rownames(x), cex = 0.7)

# Compare to PCA on original data:
pol.pca<- princomp(xs,scores=TRUE)
# plot scores of PC1 and PC2 (need to change sign):
text(pol.pca$score[,1],-pol.pca$score[,2],labels=row.names(pol.pca$score), cex = 0.7,col="red")

stars(xs, draw.segments = TRUE, key.loc = c(15,2))

#####################
## Nonmetric MDS ####
#####################

## Example: Voting
library(MASS)
load("voting.rda")
head(voting)

mdsRes <- isoMDS(voting)
mdsRes
str(mdsRes)
mycol <- c(1,1,2,2,1,1,1,2,2,2,2,1,1,2,2) ## republican or democrat
plot(mdsRes$points, type = "n", xlim = c(-15,10))
text(mdsRes$points, labels = rownames(mdsRes$points), col = mycol)



