
############################################
## PCA application (first PC as index)  ####
############################################

library(MVA)
?heptathlon

## Explore...
stars(heptathlon[,-8], draw.segments = TRUE, key.loc = c(-0.7,5))
## Rescale, so that "large value = good performance"
heptathlon$hurdles <- with(heptathlon, max(hurdles)-hurdles)
heptathlon$run200m <- with(heptathlon, max(run200m)-run200m)
heptathlon$run800m <- with(heptathlon, max(run800m)-run800m)
dat <- heptathlon[,-8]## exclude the score
stars(dat, draw.segments = TRUE, key.loc = c(-0.7,5))
library(mvoutlier)
chisq.plot(dat)
dat[25,] ## outlier: I decide to leave her out, but need to report this and the reasons I had
dat2 <- dat[-25,]
heptathlon2 <- heptathlon[-25,]

cor(dat2)
pairs(dat2) ## long jump - hurdles / javelin - run800m

heptathlon_pca <- princomp(dat2, cor = TRUE)
plot(heptathlon_pca)
summary(heptathlon_pca, loadings = TRUE)
heptathlon_pca$score[,1]



## Define a score for clear ranking: Use first PC
cor(heptathlon2$score, heptathlon_pca$score[,1])
plot(heptathlon2$score, heptathlon_pca$score[,1]) 
identify(heptathlon2$score, heptathlon_pca$score[,1], labels = rownames(heptathlon2))
heptathlon2[12,]
## Good agreement btw. Olympic scoring rule and first PC; 
## only Mrs. Dimitrova (BUL) should be annoyed ...
## Note: The sign of the PC coordinates is arbitrary



##################################
## PCA for image compression. ####
##################################

# As illustration, we use PCA for image compression.
# Of course this is not as good as JPEG, 
# but it still works reasonably well. 

library(png)
setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")

face <- readPNG("plots/face.png")
# load("face.Rda")
# writePNG(face, "plots/face.png")

dim(face)  ## 243 x 220 matrix representing the pixels
face[110:114,110:114] ## entries between 0 and 1 (greyscale)

## Apply PCA to the image
pca.res <- prcomp(face) # by default, center = TRUE if using prcomp
str(pca.res)
A <- pca.res$rotation # dim(A) = 220 x 220
Y <- pca.res$x # dim(Y) = 243 x 220
meanface <- pca.res$center  # dim(meanface) = 220 x 1


## Look at importance of components
plot(pca.res)
summary(pca.res)
# With 10 PCs we explain 96.3% of the variance. 
# How does the reconstructed image look like?

### Compress image using 10 PCs
nPCs <- 10
face.comp <- Y[,1:nPCs]

### Note: Original image was 243 x 220 pixels.
### Now only need to save: A[,1:nPCs] (A tilde from board): 220 x nPCs  pixels  
###                        face.comp  (Y tilde from board): 243 x nPCs pixels
###                        mean vector 220 x 1 pixels

## from the compressed face image and A[,1:nPCs] we can approximate the original image:
face.recovered <- face.comp %*% t(A[,1:nPCs])   # X tilde = (Y tilde) (A tilde)^T
face.recovered <- face.recovered + matrix(rep(meanface,243),byrow=T,nrow=243) # add mean again

## values should be between 0 and 1 (otherwise artifacts in picture)
## (e.g. if there is value 1.001, it is black (=0.001) instead of white (=1))
face.recovered <- pmax(pmin(face.recovered, 1), 0)
writePNG(face.recovered, "plots/face_recovered.png")

## Compare memory usage
size.comp <- object.size(A[,1:nPCs]) + object.size(face.comp) + object.size(meanface) ## memory usage of compressed image using nPCs princ. components
size.full <- object.size(face) ## memory usage of full image

## Could reduce memory usage by a factor of 
round(as.double(size.full / size.comp),2)

reduction <- NULL
PCs <- c(1,2,3,4,seq(from=5,to=40,by=5))
for (nPCs in PCs){
  face.comp <- Y[,1:nPCs]
  
  ## from the compressed face image and A[,1:nPCs] we can approximate the original image:
  face.recovered <- face.comp %*% t(A[,1:nPCs])
  face.recovered <- face.recovered + matrix(rep(meanface,243),byrow=T,nrow=243)
  
  ## values should be between 0 and 1 (otherwise artifacts in picture)
  ## (e.g. if there is value 1.001, it is black (=0.001) instead of white (=1))
  face.recovered <- pmax(pmin (face.recovered, 1), 0)
  writePNG(face.recovered, paste("plots/face_recovered_nPCs=",nPCs,".png",sep=""))
  
  ## Compare memory usage
  size.comp <- object.size(A[,1:nPCs]) + object.size(face.comp) + object.size(meanface) ## memory usage of compressed image using nPCs princ. components
  size.full <- object.size(face) ## memory usage of full image
  
  ## Could reduce memory usage by a factor of 
  reduction <- c(reduction,round(as.double(size.full / size.comp),2))
}
plot(PCs[5:13], reduction[5:13], xlim=c(0,40))
for (i in 0:5) abline(h=5*i, col="gray")



#####################################
## PCA example (USJudgeRatings)  ####
#####################################

?USJudgeRatings
rownames(USJudgeRatings)=abbreviate(row.names(USJudgeRatings))
pca.res <- prcomp(USJudgeRatings, center=TRUE, retx=TRUE)
pca.res
screeplot(pca.res)
plot(pca.res$x[,1:2], xlim=c(-6,9), ylim=c(-6,9), pch="")
text(pca.res$x[,1:2], labels=abbreviate(row.names(USJudgeRatings)), col="blue")
abline(v=0)
abline(h=0)

# Compare to star plot:
USJudge <- apply(USJudgeRatings, 2, function(x) x/max(x))
loc <- stars(USJudge, key.loc = c(13, 1.5), main = "Judge data", draw.segments = TRUE)
text(loc, abbreviate(row.names(USJudgeRatings)), 
     col = "blue", cex = 0.8, xpd = TRUE)

# Correlation between original variables and principal components
corr <- cor(USJudgeRatings, pca.res$x)
round(corr, 3)

# Correlation^2: amount of variation of each of the variables that
# is explained by the principal components:
round(corr^2, 2)

# Note the rows again sum to 1:
apply(corr^2, 1, sum)
