library(nlme)

######################
## Weights example  ##
######################

setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")
load("weights.rda")
head(dat)

## Save data in special object for grouped data
## (not necessary but useful)
w <- groupedData(weight ~ week | pers, data = dat)

## Do a standard regression for each person and compare
## (only possible for groupedData objects)
wl <- lmList(weight ~ week | pers, data = w) 
intervals(wl)
plot(intervals(wl))

#############################
## Random intercept model  ##
#############################
fmw <- lme(weight ~ week, data = w, random = ~ 1 | pers)
summary(fmw)

#############################################  
## Random intercept  & random slope model  ##
#############################################
fmw <- lme(weight ~ week, data = w, random = ~ 1 + week | pers)
summary(fmw)

intervals(fmw) ## fixed parameters 
coef(fmw) ## parameters for individual persons

## Predict new observations
newd <- data.frame(pers = c(1,5), week = c(2.5, 11))
predict(fmw, newdata = newd)

library(lattice)

pfun <- function(x,y) {
  lx <- length(x)
  panel.xyplot(x,y[1:lx])
  panel.lines(x,y[1:lx+lx], lty = 1)
}
predw <- predict(fmw)
xyplot(cbind(weight, predw) ~ week | pers, data = w,
       layout = c(3,3), panel = pfun)

## Residual plots
plot(fmw)
qqnorm(resid(fmw))
qqline(resid(fmw))
ranef(fmw)
qqnorm(ranef(fmw)[,1])
qqline(ranef(fmw)[,1])
qqnorm(ranef(fmw)[,2])
qqline(ranef(fmw)[,2])

###################
## Hausman test  ##
###################

## The example below is used only for illustration of the Hausman test. 
## We actually know in this example that the covariates are not correlated with the unit effects 
## (since the covariates are constant over the units)

##Code as factor for use in fixed effect model
dat[,1]=factor(dat[,1])
##Fixed effects model
fit.fixed = lm(weight ~ week + pers, data = dat)
b.fixed = fit.fixed$coefficients[2]
v.fixed = vcov(fit.fixed)[2,2]
##Random effects model
fit.random <- lme(weight ~ week, data = w, random = ~ 1 | pers)
b.random = fit.random$coefficients$fixed[2]
v.random=fit.random$varFix[2,2]

##Hausman test statistics
Haus.chi = (b.fixed - b.random) * (v.fixed - v.random)^(-1) * (b.fixed - b.random)
Haus.chi

## Critical value:
qchisq(.95,1)

## p-value:
1 - pchisq(Haus.chi,1)

##Alternatively, a Hausman test can be done using the plm package
library(plm)
## Create copy of week variables ("technical hack" in order to use the package in this example)
dat[,4]=dat[,2]
names(dat)[4]="weekcopy"
##Fixed effects model
fe <- plm(weight ~ weekcopy, data = dat, model = "within")
##Random effects model
re <- plm(weight ~ weekcopy, data = dat, model = "random")
summary(fe)
summary(re)
##Hausman test
phtest(re, fe)


