setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")

#####################
## Factor Analysis ##
#####################

load("FAdata.rda")
## Doing Factor Analysis with R ####
## Intelligence tests
?ability.cov
ability ## correlation matrix

ability.FA1 <- factanal(factors = 1, covmat=ability, n.obs = n.ability)
ability.FA1
ability.FA2 <- factanal(factors = 2, covmat=ability, n.obs = n.ability)
ability.FA2

## Is correlation matrix represented well using only two factors?
est <- ability.FA2$loadings %*% t(ability.FA2$loadings) + diag(ability.FA2$uniquenesses)
round(est, 3)
round(ability, 3)
dif <- round(ability - est, 3)
dif




#############
# Drug use ##
#############

# These data were collected by Huba et al (1981).
# The data describe drug usage rates for 1634 students in 
#   7th-9th grade (about 12-15 years old) 
#   in 11 schools in the area of Los Angelos
# Each student completed a questionnaire about the number of times 
#   a particular substance was used
# Responses were on a five point scale:
#  1 = never tried
#  2 = only once
#  3 = a few times
#  4 = many times
#  5 = regularly

##Correlation matrix
druguse.cor <- druguse
round(druguse.cor,1)
n.druguse

# plot correlation matrix:
image(c(1:13), c(1:13), druguse.cor, col=gray((32:0)/32), 
      xlab="", ylab="", main="Image plot of correlation matrix")
row.names(druguse.cor)

# fit factor models with 4, 5, 6 factors:
res4 <- factanal(covmat = druguse.cor, factors=4, n.obs=1634)
res4 
res5 <- factanal(covmat = druguse.cor, factors=5, n.obs=1634)
res5 
res6 <- factanal(covmat = druguse.cor, factors=6, n.obs=1634)
res6 

# Compare fitted correlation matrix to true correlation matrix:
fitted <- res4$loadings %*% t(res4$loadings) + diag(res4$uniquenesses)
round(druguse.cor-fitted, 2)

# Compare results:
res4$loadings ##F1: social drugs, F2: hard drugs, F3: smoking, F4: amphetamine
res5$loadings ##F1: social drugs, F2: hard drugs, F3: amphetamine, F4: smoking, F5: inhalants
res6$loadings ##F1: social drugs, F2: hard drugs, F3: amphetamine, F4: hashish, F5: smoking ,F6: inhalants
# Note that the first 4 factors do not remain the same
# there are considerable changes in factors 3 and 4

# How could we interpret the factors in the 4-factor model?
# (note that varimax rotation is used by default)

# Answer:
# first factor: high on beer, wine, liquor => tendency for alcohol use (+ cigarettes)
# second factor: high on cocaine, tranquilizers, heroine => tendency for hard drug use
# third factor: tendency for marijuana (+ cigarettes) use
# fourth factor: tendency for amphetamine use

# We can also interpret the unrotated factor loadings:
res7 <- factanal(covmat = druguse.cor, factors=4, n.obs=1634, rotation="none")
res7$loadings

# Answer:
# first factor: overall tendency for drug use, with emphasis on amphetamines
# second factor: high on cigarettes, alcohol, liquor, marijuana 
#   => tendency for soft drug use
# third factor: high on alcohol, negative on marijuana 
#   => tendency for alcohol use (vs marijuana)
# Fourth factor: high on hard drugs => tendency for hard drug use

##If we use only 3 factors, the results change completetly
factanal(covmat = druguse.cor, factors=3, n.obs=1634)


#########################
# Compute factor scores #
#########################

library(bootstrap)
# Load exam score data:
data(scor)
help(scor)
head(scor)

# Thompson's scores
res2.t <- factanal(scor, factors=2, rotation="none", scores="regression")   
# Bartlett's scores
res2.b <- factanal(scor, factors=2, rotation="none", scores="Bartlett")   
round(cbind(res2.t$scores, res2.b$scores),3)

# Plot
plot(res2.b$scores,pch="")
text(res2.b$scores, labels=c(1:88))
text(res2.t$scores, labels=c(1:88), col="red")

# Interpret plot
apply(scor, 1, mean) 
scor[66,]
scor[81,] 
scor[2,]
scor[87,]
# First factor represents "being good at school"
# Second factor represents "ability in closed book vs open book exams"

res2.t$scores / res2.b$scores
# Scores differ by constant factor for each factor

# This is no longer the case if we rotate:
# thompson's scores
res2.t.r <- factanal(scor, factors=2, scores="regression")   
# bartlett's scores
res2.b.r <- factanal(scor, factors=2, scores="Bartlett")   

# Plot
plot(res2.b.r$scores,pch="")
text(res2.b.r$scores, labels=c(1:88))
text(res2.t.r$scores, labels=c(1:88), col="red")

res2.t.r$scores / res2.b.r$scores
