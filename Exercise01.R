# Exercise 01
bank <- read.table("http://stat.ethz.ch/Teaching/Datasets/banknot.dat",header=T, sep="")
readRDS("C:\\Users\\Oliver\\Desktop\ETH\StatsLab\quizAnonym.rds")
summary(bank)
str(bank)
pairs(bank[,-1],col=bank$CODE+1)
hist(bank[,-1]) 
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, ...)
}
pairs(bank[,-1],diag.panel=panel.hist, col= bank$CODE+2)
panel.hist(bank[,-1])
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(bank[,-1],diag.panel=panel.hist, col= bank$CODE+2,upper.panel = panel.cor)
data(swiss)
stars(as.matrix(swiss),draw.segments = T )
stars(as.matrix(swiss[,c(2,3,5,6)]), location=as.matrix(swiss[,c(4,1)]), axes=T,
draw.segments = T)
data <- read.csv("http://stat.ethz.ch/Teaching/Datasets/quakes.csv", header=T)
dim(data)
jitter()
str(data)
plot(data$depth, jitter(data$mag), col = rgb(1,0,0,0.3), pch = 20)
plot(data$stations, jitter(data$mag), col = rgb(1,0,0,0.3), pch = 20)
library(MASS)
parcoord(data[,-1], col = rgb(1,0,0,0.3))
deepVec <- cut(quakes$depth, breaks=c(0,250,450,700), labels=c("green","orange","red"))
class(deepVec[1])
deepVecString <- as.character(deepVec)
plot(data$lat,data$long,col=deepVecString,pch=20)
coplot(data$lat~data$long|deepVec, columns = 3)
dfTitanic <- as.data.frame(Titanic)
str(dfTitanic)

Sex <- xtabs(formula = Freq~ Sex, data = dfTitanic)
Age <- xtabs(formula = Freq~ Age, data = dfTitanic)
Surv <- xtabs(formula = Freq~ Survived, data = dfTitanic)

library(vcd)
mosaic( ~ Survived + Sex, data = dfTitanic)
cotabplot( ~ Survived + Sex| Class = "1st"  , data = dfTitanic, shade = TRUE)

dfTitanic
jitter(data$mag) - data$mag

