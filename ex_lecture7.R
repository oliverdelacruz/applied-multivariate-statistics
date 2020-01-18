#####################
## Pottery example ##
#####################

library(MVA)

?pottery
## Pottery from 3 regions: region 1: kiln1, region 2: k2 & k3  region 3: k4 & k5
n <- nrow(pottery)
potdata=pottery[, colnames(pottery) != "kiln"]
head(potdata)
##Standard deviations of each variable
sapply(potdata, sd)
## Scale data
pots <- scale(potdata, center = FALSE, scale = TRUE)

##############################
## Agglomerative clustering ##
##############################

## Distance matrix
dp <- dist(pots) ## Euclidean

## 1. Apply agglomerative clutstering using complete linkage
cc <- hclust(dp, method = "complete") ## 0.02 sec

## Investigate result
cc
plot(cc)

## Split into 3 groups
grps <- cutree(cc, k = 3)
grps

## Silhouette Plot from package "cluster"
library(cluster)
plot(silhouette(grps, dp))

## Groups represent regions
dfPots <- data.frame(cls = grps, kiln = pottery[,10])
table(dfPots)

## Visualize result in PC 1 & 2
summary(princomp(pots))
princomp(pots)$loadings
pr <- princomp(pots)$scores[,1:2]
plot(pr, pch = grps, col=grps)
legend("bottomright", legend = 1:3, pch = 1:3, col=1:3)

##Look at means of cluster centers
aggregate(potdata, by=list(cluster=grps), mean)
##Can also be done as follows:
# library(dplyr)
# dataf=data.frame(cbind(potdata,grps))
# meansCluster=dataf %>% group_by(grps) %>% summarise_each(funs(mean))

##Global mean
apply(potdata,2,mean)   


## 2. Using single linkage
cc2 <- hclust(dp, method = "single")
plot(cc2)

## Split into 3 groups and compare with region
grps2 <- cutree(cc2, k = 3)
grps2
dfPots2 <- data.frame(cls = grps2, kiln = pottery[,10])
table(dfPots2)

data.frame(cls1=grps, cls2 = grps2)
## In this example: same result
## In general, the result can depend on distance measure


## Alternative: Agnes in package "cluster"
library(cluster)
ccA <- agnes(dp, method = "complete")
par(mfrow = c(1,2))
plot(ccA, which.plots = 2) ## tree using agnes
plot(cc) ## tree using hclust
## they are identical up to rearrangement of branches


#############
## k-means ##
#############

## k-means with 3 centers
ckm <- kmeans(pots, centers = 3, nstart = 10) ## <0.01 sec
grpsKM <- ckm$cluster
grpsKM

## Choose number of clusters k with scree plot
wss <- rep(0, 6)
for (i in 1:6) wss[i] <- sum(kmeans(pots, centers = i)$withinss)
plot(1:6, wss, type = "b", xlab = "Number of groups", ylab = "Within groups sum of squares") ## 3 centers was a good choice
## Note: results can vary, because of random starting configurations in kmeans

## Silhouette Plot
plot(silhouette(grpsKM, dp))

## visualize in PC 1 & 2
pr <- princomp(pots)$scores[,1:2]
plot(pr, pch = grpsKM)


###########
## PAM ####
###########

library(cluster)
pamC <- pam(x = dp, k = 3) ## < 0.01 sec
str(pamC)

pamC$medoids
pamC$clustering

## Medoids
pottery[pamC$medoids,]

plot(pamC) ## Plots Shilouette directly

## Compare with k-means
table(grpsKM, pamC$clustering)

###############################
## Gaussian Mixture Models ####
###############################

library(mclust)
mc <- Mclust(pots) ## 0.16 sec
mc ## Fitted parameters for best model
str(mc)
plot(mc)

## Compare with k means
table(mc$classification, grpsKM)

## Silhouette plot
plot(silhouette(mc$classification, dp))

## plot on PC 1 & 2
pr <- princomp(pots)$scores[,1:2]
plot(pr, pch = mc$classification, col = grpsKM)

## Fit a specific model directly
mcRestr <- Mclust(pots, G=5, modelNames = "VEV") 
mcRestr
str(mcRestr)
