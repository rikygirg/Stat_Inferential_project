library(MASS)
library(GGally)
library(RColorBrewer)
library(car)

stars <- read.csv("stars.csv")
attach(stars)

my_colors = brewer.pal(6, 'Set2')

par(mfrow=c(2,2))
boxplot(stars$Radius ~ stars$Type,col=my_colors)
boxplot(stars$L ~ stars$Type,col=my_colors)
boxplot(stars$R ~ stars$Type,col=my_colors)
boxplot(stars$A_M ~ stars$Type,col=my_colors)

par(mfrow=c(2,2))
boxplot(stars$Temperature ~ stars$Spectral_Class,col=my_colors)
boxplot(stars$L ~ stars$Spectral_Class,col=my_colors)
boxplot(stars$R ~ stars$Spectral_Class,col=my_colors)
boxplot(stars$A_M ~ stars$Spectral_Class,col=my_colors)

# ANOVA Temperature vs Spectral Class
fit = aov(Temperature ~ Spectral_Class)
summary(fit)

leveneTest(Temperature,as.factor(Spectral_Class))

# ipotesi ANOVA su A_M
n = length(stars$Type)
ng = table(stars$Type)
treat = c(0,1,2,3,4,5)
g = length(treat)

# Normalita' tra gruppi
Ps = c(
  shapiro.test(A_M[Type == treat[1]])$p,
  shapiro.test(A_M[Type == treat[2]])$p,
  shapiro.test(A_M[Type == treat[3]])$p,
  shapiro.test(A_M[Type == treat[4]])$p,
  shapiro.test(A_M[Type == treat[5]])$p,
  shapiro.test(A_M[Type == treat[6]])$p
)

Ps = tapply(A_M, Type, function( x ) ( shapiro.test( x )$p ) )
# normali per un cazzo

# Omoschedastici
Var = tapply(L,Type,var)

leveneTest(R,Type)
bartlett.test(R,Type)

# Omoschedastici manco per il cazzo

fit = aov(A_M ~ Type)
summary(fit)

# Proviamo con Box-Cox
b = boxcox(stars$R ~ A_M)
best_lambda_ind = which.max( b$y )
lambda = b$x[best_lambda_ind]

Rnew = (R^lambda-1)/lambda

mod = lm(R ~ A_M)
mod1 = lm(Rnew ~ A_M)
qqnorm(mod1$residuals)
abline(0,1,col='red')
shapiro.test(mod1$residuals)
shapiro.test(mod$residuals)
