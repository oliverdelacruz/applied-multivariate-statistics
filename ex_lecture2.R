library(vcd) ## package "Visualization for Categorical Data"

#####################################
# Visualization of categorical data #
#####################################

## Mosaicplot ####
## Mariage and Education
setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")
df <- read.csv2("marriage.csv", header = TRUE)
df
tab <- xtabs(Freq ~ ., data = df)
tab
structable(~ Edu + Mar, data = df)
mosaic(tab)

mosaic(tab, shade = TRUE)

## Hair/Eye Color
## table to df: as.data.frame
df <- as.data.frame(HairEyeColor)
## df to table: xtabs
tab <- xtabs(Freq ~ ., data = df)
tab
## structable & mosaic plot: split horiz./vert./horiz./vert./etc.
## don't use for more than four variables
## Convenient layout of table:
structable(~ Hair + Eye, data = df)
## or
structable(~ Hair + Eye + Sex, data = df)

## Mosaic plot without independence test
mosaic(~ Hair + Eye, data = df, shade = FALSE)
## Mosaic plot with independence test
mosaic(~ Hair + Eye, data = df, shade = TRUE)
## The hyp. "Haircolor and Eyecolor are independent" is clearly rejected (p-value < 2.22*10^-16).
## If you have black hair, it is "surprisingly" likely that you have brown eyes and surprisingly unlikely to have blue eyes.
## If you have blond hair, you are surprisingly unlikely to have brown eyes and surprisingly likely to have blue eyes.

## Cotabplot ####
## Make a mosaic plot for each sex:
cotabplot(~ Hair + Eye | Sex, data = df, shade = TRUE)
## The connection blue eyes - blond hair in women is much more striking than in men

## Admission to UC Berkeley in 1973: Gender Bias ? ####
UCBAdmissions
dfUCB <- as.data.frame(UCBAdmissions)
head(dfUCB)

## The columns "Freq" will be taken automatically into account
## If it was named e.g."fr", you would have to write the formula
## fr ~ Admit + Gender
## instead of
## ~ Admit + Gender
structable( ~ Admit + Gender, data = dfUCB)
mosaic( ~ Admit + Gender, data = dfUCB)
mosaic(~ Admit + Gender, data = dfUCB, shade = TRUE)
## Men are accepted significantly more often
## Is this proof for gender bias?

## Where do people apply?
structable(~ Dept + Gender, data = dfUCB)
mosaic(~ Dept + Gender, data = dfUCB, shade = TRUE)
## Independence is clearly rejected (p < 2.2*10-16)
## Men prefer Dept's A,B; women prefer Dept's C,E

## How selective are departments?
structable(~ Dept + Admit, data = dfUCB)
mosaic(~ Dept + Admit, data = dfUCB, shade = TRUE)
## Independence is clearly rejected (p < 2.2*10-16)
## Dept's A,B are not very competitive; Dept's E,F are very competitive

## Women apply more to competitive departments.

## Let's look at a mosaic plot PER DEPARTMENT:
cotabplot(~ Admit + Gender | Dept, data = dfUCB, shade = TRUE)


## There is no strong gender bias within the departments. If anything, women are favored in Dept A.


## There SEEMED to be a bias agains women, but this only arises to the fact, that women tend to 
## apply to more competitive departments. The departments themselves have no gender bias (apart perhaps Dept. A).


#####################
# Outlier detection #
#####################

#### Outliers in one component
set.seed(123)
dat <- matrix(rnorm(5*100),100,5)
summary(dat)
dat[23,4] <- dat[23,4] * 10
summary(dat)
pairs(dat)

which.min(dat[,4])
# That was easy.
# But outliers can be well hidden...

#### Multivariate outlier: A simple example

load(file = "1dOutlierExample.rda")

par(mfrow=c(1,3))
plot(dat, xlim = c(-5,5), ylim = c(-5,5))
barplot(dat[,1], main="x values")
barplot(dat[,2], main="y values")
# There are no clear univariate outliers

# Mahalanobis distance
par(mfrow = c(1,2))
d <- mahalanobis(dat, colMeans(dat), cov(dat))
barplot(d, main="Mahalanobis")
which.max(d)

# Create chi-squared QQ-plot:
qqplot(qchisq(ppoints(301), df = 2), d,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 2]))
# The outlier is clearly visible.

## more sophisticated method - use robust estimates: 
library(mvoutlier)
par(mfrow = c(1,2))
plot(dat, xlim = c(-5,5), ylim = c(-5,5))
chisq.plot(dat)

# What was the outlier?
par(mfrow = c(1,1))
plot(dat, xlim=c(-5,5), ylim=c(-5,5))
points(dat[301,1],dat[301,2],col="red",lwd=2)

#### More dimensions
load(file = "3dExample.rda")
pairs(dat)

# Create outlier
outFactor <- 2
dat <- rbind(dat, outFactor*c(-1,-1.2,0.7))
pairs(dat)
pairs(dat, col = c(rep(1,300), 2), pch = c(rep(1,300), 3), cex = c(rep(1,300), 2))
# In none of the plots, the point is an outlier

d <- mahalanobis(dat, colMeans(dat), cov(dat))
barplot(d, main="Mahalanobis")
which.max(d)

# Create chi-squared QQ-plot:
par(mfrow=c(1,1))
qqplot(qchisq(ppoints(301), df = 2), d,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 2]))
# The chi-squared QQ plot shows that it is a multivariate 
#   outlier. 

library(rgl)
plot3d(dat, col = c(rep(1,300), 2))
# This is confirmed by a 3d-plot that can be moved around.
