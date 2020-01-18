###########
## LDA ####
###########
library(MASS)
?lda

###############################
## Example 1: Iris flowers ####
###############################
?iris

## Data
head(iris)

summary(iris)
cls <- as.numeric(iris[,5]) ## class labels recoded to numbers (for later plots)
trainIdx <- c(1:40, 51:90, 101:140) ## 40 samples of each class for training
testIdx <- c(41:50, 91:100, 141:150) ## 10 samples of each class for testing

## Fit LDA
lda.fit <- lda(Species ~ ., data = iris, subset = trainIdx)
lda.fit
ld.pred <- predict(lda.fit, dimen = 2)
ld <- ld.pred$x
ld.pred$class

## Plot samples on first two linear discriminants
plot(ld, asp = 1, col = cls[trainIdx], xlab = "LD 1", ylab = "LD 2")

## Predict labels for new observations
lda.pred <- predict(lda.fit, newdata = iris[testIdx,])## apply to test data
points(lda.pred$x, col = 4, pch = 20)
str(lda.pred)
lda.pred$class

## Check quality of prediction ####
df <- data.frame(est = lda.pred$class, truth = iris[testIdx,"Species"])
tab <- table(df) ## confusion matrix
tab
1 - sum(diag(tab)) / nrow(df) ## error rate only 0% -> expect very good predicions


## Use Cross Validation to assess quality of predictions
lda.cv <- lda(Species ~ ., data = iris, CV = TRUE)
lda.cv$class
res <- data.frame(est = lda.cv$class, truth = iris[, 5])
tab <- table(res) ## confusion matrix
tab
1 - sum(diag(tab)) / nrow(iris) ## error rate only 2% -> expect very good predicions


###############################################
## Example 2: Digit recognition on letters ####
###############################################

## install.packages("ElemStatLearn")
library(ElemStatLearn)
?zip.train
dim(zip.train)
zip.train[1:5,1:10]

## Save original data into data frame for convenience
zip.df <- as.data.frame(zip.train)
zip.df[[1]] <- as.factor(zip.df[[1]])
zipTest.df <- as.data.frame(zip.test)
zipTest.df[[1]] <- as.factor(zipTest.df[[1]])

## Compute LDA
lda.zip <- lda(V1 ~ ., data = zip.df)
lda.zip
## Make a plot using the first two LD's
lda.predOrig <- predict(lda.zip)
plot(lda.predOrig$x[,1:2], col = as.numeric(zip.df[[1]]))
## Make predictions for test samples
lda.pred <- predict(lda.zip, newdata = zipTest.df)
## Compute confusion matrix and error rate
res <- data.frame(est = lda.pred$class, truth = zipTest.df$V1)
table(res)
1 - sum(diag(table(res)))/nrow(zipTest.df) ## around 11% error rate


###########################
## Logistic regression ####
###########################

################################
## Example: SPAM detection #####
################################

?spam
dim(spam)
str(spam)
table(spam$spam)

## Select a test set
set.seed(45)
testIdx <- sort(sample(1:4601, 500))
table(spam$spam[testIdx])

## Fit logistic regression
spam.fit <- glm(spam ~ ., data = spam[-testIdx,], family = "binomial")

## Predict outcome in test set
spam.pred <- predict(spam.fit, newdata = spam[testIdx,], type = "response")
spamEst <- (spam.pred > 0.5)
spamTrue <- (spam$spam[testIdx] == "spam")

## Evaluate performance of prediction
df <- data.frame(est = spamEst, truth = spamTrue)
tab <- table(df) ## confusion matrix
tab
1 - sum(diag(tab))/length(testIdx) ## About 8% error rate

