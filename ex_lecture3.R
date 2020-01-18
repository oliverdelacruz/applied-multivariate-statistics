###########
## PCA ####
###########

###############################
## PCA example (head size) ####
###############################
## save(headsize, file = "head.rda")
setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")
load("head.rda")
head_dat <- headsize[,c("head1", "head2")]##Head length (mm) of first and second son
head(head_dat)##show first colums
plot(head_dat)
cm <- colMeans(head_dat)
cm
## Covariance matrix
cov(head_dat)
## Total variance
sum(diag(cov(head_dat)))
## Correlation matrix
cor(head_dat)
## Calculate PCA
?princomp
head_pca <- princomp(x = head_dat)
summary(head_pca, loadings = TRUE)
plot(head_pca)


######################################
## 2nd PCA example (Corr vs Cov)  ####
######################################

load("blood.rda")
## Covariance matrix
blood_cov
sqrt(diag(blood_cov))
## PCA using covariance matrix
blood_pcacov <- princomp(covmat = blood_cov)
summary(blood_pcacov, loadings = TRUE)
plot(blood_pcacov)
## PCA using correlation matrix
blood_corr <- cov2cor(blood_cov)
blood_pcacor <- princomp(covmat = blood_corr)
summary(blood_pcacor, loadings = TRUE)

## How many PC's? ####
summary(blood_pcacor)
plot(blood_pcacor)


## Heads example again - this time using correlation matrix
cor(head_dat)
cm <- colMeans(head_dat)
cs <- apply(head_dat,2,sd)## Calculate standard deviation for each variable (head1 and head2)
head_scaled <- scale(head_dat, center = TRUE, scale = TRUE)
head(head_scaled)
## m1 = 186, m2 = 184, s1 = 9.8, s2 = 10.0
cor(head_scaled)
cov(head_scaled)

## PCA using the scaled data
head_pca2 <- princomp(head_scaled)
summary(head_pca2, loadings = TRUE)
## or equivalent using the original data with correlation matrix
head_pca3 <- princomp(head_dat, cor = TRUE)
summary(head_pca3, loadings = TRUE)

## using eigenvalue decomposition explicitly
corMat <- cor(head_scaled)
resEigen <- eigen(corMat)
resEigen$values
sqrt(resEigen$values)
resEigen$vectors
## Eigenvectors are loadings (direction) of PCs, eigenvalues are variances along PCs



#######################
## Compute scores  ####
#######################
str(head_pca2)
## 1. They are already computed for all input data
head(head_pca2$scores)
## 2. Compute yourself:
A=resEigen$vectors
Y=head_scaled%*%A
head(Y)

## 3. Alternatively, by hand:
## Compute scores for one old and one new observation
nd <- matrix(c(191, 179, 190, 180), 2,2, byrow = TRUE)
ndSc <- nd
ndSc[,1] <- (ndSc[,1] - cm[1])/cs[1] 
ndSc[,2] <- (ndSc[,2] - cm[2])/cs[2] 
colnames(nd) <-colnames(ndSc) <- c("head1", "head2")
## by hand
ndSc
0.71*0.54 - 0.71*0.48
-0.71*0.54 - 0.71*0.48
## 4. Using predict
predict(head_pca2, newdata = ndSc)
## Or if the correlation matrix was used with the original data, 
## we can use the unscaled (new) data
head_pca3 <- princomp(head_dat, cor = TRUE)
predict(head_pca3, newdata = nd)
