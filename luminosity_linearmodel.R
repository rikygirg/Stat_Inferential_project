library(MASS)
library(GGally)
library(RColorBrewer)
library(car)
library(plotly)
library(dplyr)


stars <- read.csv("new_stars.csv")
attach(stars)

M <- stars[which(stars$Type=="Main Sequence" | stars$Type=="Brown Dwarf"),]

logL = log(stars$L)
logR = log(stars$R)
logT = log(stars$Temperature)

mod <- lm(logL ~ logR+logT,data=stars)
summary(mod)

lev = hatvalues(mod)
p = mod$rank
n = dim(stars)[1]

mod_nelev <- lm(logL ~ logR+logT,subset = (lev<2*p/n))
summary(mod_nelev)

# residui standardizzati
res_std = mod$residuals/mod$sigma
watchout = which(abs(res_std)>2)


plot(mod$fitted.values,mod$residuals)
qqnorm(mod$residuals)
qqline(mod$residuals,col='red')

shapiro.test(mod$residuals)


logL_M = log(M$L)
logR_M = log(M$R)
logT_M = log(M$Temperature)
mod1 <- lm(logL_M ~ logR_M + logT_M,data=M)
summary(mod1)

lev = hatvalues(mod)
p = mod$rank
n = dim(savings)[1]

plot(mod$fitted.values,lev,pch=16,col='black',main="Leverage Points")
abline(h=2*p/n,lty=2,col='red')

watchout_points_lev = lev[ which( lev > 2 * p/n ) ]
watchout_ids_lev = seq_along( lev )[ which( lev > 2 * p/n ) ]
points(mod$fitted.values[ watchout_ids_lev], watchout_points_lev, col = 'red', pch = 16 )


nolev <- lm(logL ~ logR+logT, subset=(lev<2*p/n))
summary(nolev)
shapiro.test(nolev$residuals)

plot(mod$fitted.values,mod$residuals)

qqnorm(mod$residuals)
qqline(mod$residuals,col='red')
