#############
## Trees ####
#############

#######################
## Regression Tree ####
#######################
library(MVA)
library(rpart)
?USairpollution
fit.tr <- rpart(SO2 ~ ., data = USairpollution)
fit.tr
plot(fit.tr, margin = 0.2, uniform = TRUE)
text(fit.tr, use.n = TRUE, cex = 0.7)

predict(fit.tr, newdata = USairpollution[1:3,]) ## predict values for first three samples

###########################
## Classifiaction Tree ####
###########################
## Default (grow tree and prune automatically)
ir.fit <- rpart(Species ~ ., data = iris)
ir.fit
plot(ir.fit, margin = 0.2, uniform = TRUE)
text(ir.fit, use.n = TRUE, cex = 0.7)

## Full tree and pruning manually
ir.fit <- rpart(Species ~ ., data = iris, minsplit = 1, cp = 0)
##'cp' corresponds to the alpha fomr the lecture, cp=alpha/(Goodness of fit of empty tree)
plot(ir.fit, margin = 0.2, uniform = TRUE)
text(ir.fit, use.n = TRUE, cex = 0.7)

plotcp(ir.fit)

ir.fit3 <- prune(ir.fit, cp = 0.094)
plot(ir.fit3, margin = 0.2, uniform = TRUE)
text(ir.fit3, use.n = TRUE, cex = 0.7)

## Predict observations
testExpl <- iris[c(1, 78, 148),]
predict(ir.fit3, newdata = testExpl, type = "class")
predict(ir.fit3, newdata = testExpl, type = "prob")
?predict.rpart


######################
## Random Forests ####
######################

library(randomForest)

## Regression with Random Forest ####
?USairpollution
fit.rf <- randomForest(SO2 ~ ., data = USairpollution)
fit.rf

## Classification with Random Forest ####
library(MASS)
?fgl
head(fgl)
summary(fgl)

## Fit RF with default settings
fit.rf <- randomForest(type ~ ., data = fgl)
fit.rf

## Plot Error vs. nmb. of trees
plot(fit.rf)
## Approx. 100 trees seems enough

## Refit with 100 trees
fit.rf <- randomForest(type ~ ., data = fgl, ntree = 100)

## Predict class labels on new observations
## (for simplicity, we take some old observations)
idx <- c(1,73,150,169,181,205)
datNew <- fgl[idx,]
predict(fit.rf, newdata = datNew) ## predicted labels
fgl$type[idx] ## true labels for comparision

#########################
## Compare with tree ####
#########################

## leave-one-out CV
set.seed(123)
nreps <- nrow(fgl)
resRF <- resT <- rep(NA, nreps)
for (i in 1:nreps) {
  cat("i=",i,"\n")
  dTrain <- fgl[-i,]
  dTest <- fgl[i,]
  rf.fit <- randomForest(type ~ ., data = dTrain, ntree = 100)
  t.fit <- rpart(type ~ ., data = dTrain)
  resRF[i] <- predict(rf.fit, newdata = dTest) != dTest$type
  resT[i] <- predict(t.fit, newdata = dTest, type = "class") != dTest$type
}
mean(resRF) ## Missclass.rate RF: 20%
mean(resT) ## Missclass.rate Tree: 28%

## Compare with LDA (also leave-one-out CV) ####
lda.fit <- lda(type ~ ., data = fgl, CV = TRUE)
mean(lda.fit$class != fgl$type) ## 35%

###########################
## Variable Importance ####
###########################

set.seed(32)
rf.fgl <- randomForest(type ~ ., data = fgl, importance = TRUE)
varImpPlot(rf.fgl, n.var = ncol(fgl)-1)
rf.fgl

rf.good <- randomForest(type ~ RI + Al + Mg, data = fgl)
rf.good

rf.bad <- randomForest(type ~ Fe + Si + K, data = fgl)
rf.bad

