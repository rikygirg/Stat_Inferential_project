library(MASS)
library(GGally)
library(RColorBrewer)
library(car)

stars <- read.csv("new_stars.csv",header=TRUE)
stars <- stars[which(stars$Type!="Hyper Giants"),]
attach(stars)

ggpairs(stars,col=c(1:4),aes(col=Type))

mod = lm(Temperature ~ R+L+A_M)
summary(mod)

## Leverage points
p = mod$rank
n = dim(stars)[1]

lev = hatvalues(mod)

plot(mod$fitted.values,lev,pch=16)
abline(h = 2*p/n,lty=2,col='red')
