#########################
## Multivariate Tests  ##
#########################

## install.packages("ICSNP")
library(ICSNP)

#######################################
## One sample Hotelling's T^2 test ####
#######################################

?HotellingsT2
?pulmonary
data(pulmonary)
pulmonary

library(rgl)
##Calculate empirrical mean
smean=apply(pulmonary,2,mean)
dat <- rbind(c(0,0,0),smean, pulmonary)
##Plot data. Red dot: zero. Blue dot: sample mean.
plot3d(dat, col = c(2,4,rep(1,12)),size=2,type='s')

HotellingsT2(pulmonary)

########################################
## Two samples Hotelling's T^2 test ####
########################################

setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")
load("screws.rda")
library(rgl)
head(screws)
pairs(screws, col=(screws$Lightning+1),lwd=2,cex=2)

# Boxplots for comparing the two groups:
par(mfrow=c(1,3))
for(i in 1:3){
  boxplot(screws[screws$Lightning=="0",i], boxwex=0.25, at=1-0.2, 
          col="blue", main=names(screws)[i])
  boxplot(screws[screws$Lightning=="1",i], boxwex=0.25, at=1+0.2, 
          col="red", add=TRUE)
  legend("bottomleft", c("Before lightning", "After lighning"), fill = c("blue", "red"),bty='n')
}
##3d plot
meanbefore=apply(screws[screws$Lightning=="0",-4],2,mean)
meanafter=apply(screws[screws$Lightning=="1",-4],2,mean)
datscrews <- rbind(meanbefore,meanafter, screws[,-4])
## black dots: screws before lightning, red dots: screws after lightning
## blue dot: mean before lightning, purple dot: mean after lightning
plot3d(datscrews, col = c(4,6,(screws$Lightning+1)),size=2,type='s')

##Plot data. Red plot=zero. Blue dot=sample mean.
plot3d(dat, col = c(2,4,rep(1,12)),size=2,type='s')

screwsPre=screws[screws$Lightning==0,-4]
screwsPost=screws[screws$Lightning==1,-4]
HotellingsT2(screwsPre, screwsPost)
# Note that what is called the T2 statistic in the output
# is the rescaled statistic that has an F distr.

##############
## MANOVA ####
##############
head(iris)
summary(aov(Sepal.Length ~ Species, data = iris)) ## Univariate ANOVA

manova.fit <- manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = iris)
summary(manova.fit, test = "Wilks") ## overall result
summary.aov(manova.fit) ## can easily obtain ANOVA results for individual response variables

######################################
## Multivariate Linear Regression ####
######################################
## (not well supported in R)
load("body_meas.rda")

summary(lm(chest ~ age + height, data = body_meas)) ## univariate regression

mvlm <- lm(cbind(chest, upArm)~ age + height, data = body_meas)
anova(mvlm, test = "Wilks")
summary(mvlm)

######################
## Multiple Testing ##
######################

# source("https://bioconductor.org/biocLite.R")
# biocLite("multtest")
library("multtest")

?mt.rawp2adjp

pvals <- c(0.011, 0.02, 0.005, 0.13, 0.04) 

## No correction: Reject 4 Nullhyps. at 5% level

## FWER: Bonferroni 
## (control at 5% level; i.e., prob. of having one or more false pos. is at most 5%)
resB <- mt.rawp2adjp(pvals, proc = "Bonferroni")
resB
rejectB <- resB$index[resB$adjp[,2] < 0.05]
rejectB ## reject only one Nullhyp.

## FWER: Holm-Bonferroni
## (control at 5% level; i.e., prob. of having one or more false pos. is at most 5%)
resH <- mt.rawp2adjp(pvals, proc = "Holm")
resH
rejectH <- resH$index[resH$adjp[,2] < 0.05]
rejectH ## reject two Nullhyps.: Holm is more powerful than Bonferroni

## FDR: Benjamini-Hochberg 
## (control at 10% level; i.e., about 10% of the found are actually not there)
resBH <- mt.rawp2adjp(pvals, proc = "BH")
resBH
rejectBH <- resBH$index[resBH$adjp[,2] < 0.05]
rejectBH ## reject three Nullhyps.: Holm is more powerful than Bonferroni


#######################
## Case study: Which genes are associated with different types of Leukemia?
#######################
data(golub)
?golub

head(golub)
golub.cl

?mt.teststat
teststat <- mt.teststat(golub, golub.cl, test = "t")
str(teststat)

rawp0 <- 2 * (1 - pnorm(abs(teststat))) ## use normals distr. instead of t as simplification

## no correction
sum(rawp0 < 0.05)
## FDR < 10%
resBH <- mt.rawp2adjp(rawp0, proc = "BH")
sum(resBH$adjp[,"BH"] < 0.1)
## FWER < 5% using Holm
resBH <- mt.rawp2adjp(rawp0, proc = "Holm")
sum(resBH$adjp[,"Holm"] < 0.05)
## FWER < 5% using Bonferoni
resBH <- mt.rawp2adjp(rawp0, proc = "Bonferroni")
sum(resBH$adjp[,"Bonferroni"] < 0.05)
