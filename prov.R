library(MASS)
library(GGally)
library(RColorBrewer)
library(car)

stars <- read.csv("new_stars.csv")
attach(stars)

ggpairs(stars,col=c(1:4),aes(col=Type))

mod <- lm(R ~ Temperature+L+A_M)
summary(mod)

qqnorm(mod$residuals)
qqline(mod$residuals,col='red')

shapiro.test(mod$residuals)

b = boxcox(mod)
lambda = b$x[which.max(b$y)]

mod1 <- lm((R^lambda-1)/lambda ~ Temperature+L+A_M)
summary(mod1)
qqnorm(mod1$residuals)
qqline(mod1$residuals,col='red')
shapiro.test(mod1$residuals)

plot(mod1,which=1)


mod2 <- lm(Temperature ~ L+R+A_M+L*Type)
summary(mod2)
qqnorm(mod2$residuals)
qqline(mod2$residuals)
shapiro.test(mod2$residuals)

mod3 <- lm(Temperature ~ L+R+A_M*Type)
summary(mod3)
qqnorm(mod3$residuals)
qqline(mod3$residuals)
shapiro.test(mod3$residuals)

b = boxcox(mod3)
lambda = b$x[which.max(b$y)]
mod4 <- lm((Temperature^lambda-1)/lambda ~ L+R+A_M)
summary(mod4)
shapiro.test(mod4$residuals)
