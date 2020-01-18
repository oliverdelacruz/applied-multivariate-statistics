#####################
# Quick review of R #
#####################

# You can use R as a calculator:
5+3

# Create a vector, using the command 'c()' (for concatenate)
x <- c(1,2,3,4)
x

# Sum all elements of x:
sum(x)

# Square all elements of x:
x^2

# Get the third element of x:
x[3]

# Add an extra element:
x <- c(x,10)
x

# Create a vector using the command 'seq()' (for sequence)
x <- seq(from=1, to=10, by=.1)
x

# Obtain the length of x:
length(x)

# Create a vector using the command 'rep()' (for repeat)
x <- rep(0,times=10)
x


###################
# Load a data set #
###################

## Read comma-separated-version file (csv)
## The first line is the header (header = TRUE)
## The first col contains the row names (row.names = 1)
getwd() 
## Watch out: The "/" has a different orientation than shown in e.g. Windows Explorer;
## change by hand !
setwd("/Users/whsigris/Dropbox/teaching/AMS/Data")
x <- read.csv(file = "USairpollution.csv", header = TRUE, row.names = 1)
##Alternatively
x=read.csv(file = "/Users/whsigris/Dropbox/teaching/AMS/Data/USairpollution.csv",
         header = TRUE, row.names = 1)
?read.csv ## if you need more info on the function "read.csv"
## If you use MS Excel, you might also want to check read.csv2:
## Excel uses a ; as separator for .csv

## For importing data sets, you can also use the "Import Dataset" button in the menu

library(MVA) ## package corresponding to the book we use
## This help page explains the data set
?USairpollution

########################
# Quickly explore data #
########################

## look at first couple of rows
head(x)
dim(x)
## look at structure of x
str(x)

# The data are stored in a matrix. 
# We can access the entries in the data set. For example, 
# for the entry in the second row and the third column:
x[2,3]

# Obtain the first row:
x[1,]

# Obtain the first column:
x[,1]





######################################
# Visualization and summary measures #
######################################

############
## 1-D data
############

## Histogramm of temp
hist(x$temp)
## Boxplot of temp
boxplot(x$temp)
## Boxplot for all variables
boxplot(x, main="Boxplot of airpolution data")


#############
## Dependency
#############

## Is there a (linear) association between SO2 and manu?
plot(x$manu, x$SO2) ## Scatterplot
cov(x$manu, x$SO2) ## Covariance: depends on units of measurement
cor(x$manu, x$SO2) ## Correlation: does not depend on that
cor.test(x$manu, x$SO2, method = "pearson") 


##################################
## Multivariate summary statistics
##################################

## Mean vector, covariance and correlation matrix 
cm <- colMeans(x)
cm
covm <- cov(x) ## Covariance matrix
corm <- cor(x) ## Correlation matrix
round(covm,digits=2)
round(corm,digits=3)


#####################################
## Visulaization of multivariate data
#####################################

# Bivariate scatter plot again
plot(x$precip, x$temp)

# add labels along the axes
plot(x$precip, x$temp, xlab="Precipitation", 
     ylab="Temperature", main="Temperature vs. Precipitation")

## What about all pairs of variables?
plot(x) ## Scatterplotmatrix. Same as pairs(x)
pairs(x)

## Bubble plot
plot(x$temp, x$SO2, pch = 20, xlab = "Temp", ylab = "SO2")
symbols(x$temp, x$SO2, circles = x$manu, add = TRUE)
##Nicer display:
symbols(x$temp, x$SO2, circles = x$manu, add = TRUE, 
        lwd=2, fg=rgb(0,0,1), bg="#0000FF40")

## Parallel coordinate plot
library(MASS)
parcoord(x)

## Stars plot
stars(x)
palette(rainbow(12, s = 0.6, v = 0.75)) ## for nicer colors
stars(x, key.loc = c(15,1.5), flip.labels = FALSE,draw.segment = TRUE )
palette("default") ## set colors back to default

##############
## Mixed data (continuous and discrete)
##############

##Cars data
?mtcars

## Use of colors:
plot(mtcars[,1:7])
cyl=mtcars$cyl
plot(mtcars[,1:7],main="Scatterplot matrix of car data",col=cyl)
## or just one scatter plot
plot(mtcars$hp, mtcars$mpg, col=cyl)
legend("topright", c("4 cyl", "6 cyl", "8 cyl"), 
       lty=1, col=c(4,6,8), lwd=2)
## Parallel coordinate plot
parcoord(mtcars[,1:7])
# colors can help:
parcoord(mtcars[,1:7], col=cyl)

## Conditioning plots
coplot(mpg ~ hp | as.factor(cyl), data = mtcars,panel = panel.smooth, rows=1)
