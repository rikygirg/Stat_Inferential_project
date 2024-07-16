library(car)
library(faraway)
library(leaps)
library(MASS)
library(GGally)
library(rgl)
library(dplyr)
library(data.table)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(onewaytests)

source("diagnostic.R")

# Importiamo il dataset
stars <- read.csv("new_stars.csv")
attach(stars)

# Come e' fatto il dataset?
n = dim(stars)
head(stars)

ggpairs(stars,col=c(1:4),aes(colour = stars$Type))

# Vogliamo fittare un modello che metta in relazione la luminosita'
# usando solo features numeriche (R,T,A_M)

g <- lm(L ~ R+Temperature+A_M,data=stars)
summary(g)

# R^2 di partenza onesto R^2_adj = 0.5147

diagnostica(g)

# Non omoschedastico e residui non gaussiani. Proviamo 
# a studiare punti leva e residui

par(mfrow=c(1,3))
lev <- leverage_points(g,TRUE)
res_stu <- studentized_residuals(g,TRUE)
res_std <- standardized_residuals(g,TRUE)

p = g$rank

g_post_lev <- lm(L ~ R+Temperature+A_M,data=stars, subset = (lev < 2 * p / n))
summary(g_post_lev)
AIC(g_post_lev)

g_post_stu <- lm(L ~ R+Temperature+A_M,data=stars, subset = (abs(res_stu) < 2))
summary(g_post_stu)
AIC(g_post_stu)

g_post_std <- lm(L ~ R+Temperature+A_M,data=stars, subset = (abs(res_std) < 2))
summary(g_post_std)
AIC(g_post_std)

# Scegliamo modello senza residui studentizzati, R^2 migliore e AIC minore

diagnostica(g_post_stu)

# Ancora niente omoschedasticita' e normalita'

# Passiamo a scala logaritmica: A_M e' gia' in scala logaritmica

type_order = c("White Dwarf","Red Dwarf","Brown Dwarf","Main Sequence","Super Giants","Hyper Giants")

df <- data.frame(lum = log(stars$L), temp = log(stars$Temperature), radius = log(stars$R), AM = stars$A_M, class = stars$Type)
df$class <- factor(df$class,type_order)

# Inserire immagini rilevamenti

log_l <- log(stars$L) # in unita' solari: 8e-5 - 849420 L_sun = 3.828*10^26 Watts
log_r <- log(stars$R) # in unita' solari: 0.0084 - 1948.5 R_sun = 6.9551 x 10^8 m
log_t <- log(stars$Temperature) # kelvin: 1936 - 40000

mod <- lm(log_l ~ log_r + log_t + stars$A_M, data = stars)
summary(mod)

# R^2 notevolmente aumentato, vediamo normalita' e omoschedasticita'

plot(mod,which=1)
qqnorm(mod$residuals)
qqline(mod$residuals,col='red')
shapiro.test(mod$residuals)

#proviamo ad introdurre anche la variabile categorica

categorical <- lm(log_l ~ log_r + log_t + stars$A_M + stars$Type, data=stars)
summary(categorical)
shapiro.test(categorical$residuals)

# dividiamo il dataset nelle diverse classi di stelle. Questo ha sia delle motivazioni 
# fisiche: stelle appartanenti a classi diverse hanno comportamenti molto differenti;
# sia motivazioni statistiche: nella classe, i dati sono normali. 
detach(stars)

m_sequence <- df[which(df$class=="Main Sequence"),]
dwarf <- df[which(df$Type == "Brown Dwarf" | df$Type == "Red Dwarf" | df$Type == "White Dwarf"),]
giants <- df[which(df$Type == "Super Giants" | df$Type == "Hyper Giants"), ]

mod_m <- lm(lum ~ radius+temp+AM, data=m_sequence)
summary(mod_m)
shapiro.test(mod_m$residuals)

# R^2 molto alto (bene), Pvalue shapiro alto (molto bene)
# Procediamo con pulizia di punti leva, residui

par(mfrow=c(1,3))
lev <- leverage_points(mod_m,TRUE)
res_stu <- studentized_residuals(mod_m,TRUE)
res_std <- standardized_residuals(mod_m,TRUE)

p = g$rank
n = dim(m_sequence)

mod_m_post_lev <- lm(lum ~ radius+temp+AM,data=m_sequence, subset = (lev < 2 * p / n))
summary(mod_m_post_lev)
AIC(mod_m_post_lev)

mod_m_post_stu <- lm(lum ~ radius+temp+AM,data=m_sequence, subset = (abs(res_stu) < 2))
summary(mod_m_post_stu)
AIC(mod_m_post_stu)

mod_m_post_std <- lm(lum ~ radius+temp+AM,data=m_sequence, subset = (abs(res_std) < 2))
summary(mod_m_post_std)
AIC(mod_m_post_std)

# scegliamo studentized residuals perche' ha AIC minore

summary(mod_m_post_stu)
par(mfrow=c(1,2))
plot(mod_m_post_stu,which=1)
qqnorm(mod_m_post_stu$residuals)
qqline(mod_m_post_stu$residuals, col='red')
shapiro.test(mod_m_post_stu$residuals)

# Abbiamo ottima normalita' e sembra anche abbastanza omoschedastico
# AM ha un pvalue del t-test moltoooo alto, e' davvero rilevamente?
# Rimuoviamo AM dal modello e vediamo cosa succede

g2 <- lm(lum ~ radius+temp, data=m_sequence, subset = (abs(res_stu) < 2))
summary(g2)

# R^2 sostanzialmente invariato, ottimo

par(mfrow=c(1,2))
plot(g2,which=1)
qqnorm(g2$residuals)
qqline(g2$residuals, col='red')
shapiro.test(g2$residuals)

# I coefficienti della regressione lineare sono
# beta_0 = -42.9 vs -35.6
# beta_r = 1.449 vs 2
# beta_t = 4.94 vs 4

# Questa differenza puo' essere dovuta alla presenza di misclassified o
# particolari outlier. Indaghiamo la cosa

par(mfrow=c(1,2))
plot(m_sequence$temp,m_sequence$lum)
plot(m_sequence$radius,m_sequence$lum)

# torniamo in scala naturale, per vedere meglio quanto sono realmente outlier

ms <- stars[which(stars$Type == "Main Sequence"),]
par(mfrow=c(1,1))
plot(ms$Temperature,ms$L)

idx = which(ms$L > 1.5e5)
mintaka = ms[idx,]

points(mintaka$Temperature,mintaka$L,col='red',pch=16)
text(mintaka$Temperature[1],mintaka$L[1],labels = "Mintaka")

# La stella Mintaka (anche chiamta delta-orinis), appartiene alla
# cintura di orione. Gli astronomi classificano questa stella come
# super giants, a differenza del nostro dataset. Lo stesso vale 
# per le altre stelle in rosso nel grafico. 

# Proviamo ad escludere mintaka & friens dalla main sequence e
# ricalcoliamo i coefficienti del modello

keep <- m_sequence[which(ms$L<1.5e5),]
g_3 <- lm(keep$lum ~ keep$radius+keep$temp, data=keep)
summary(g_3)

sb <- function(radius,temp){
  sigma = 5.67*10^-8
  L0 = 3.828e26
  R0 = 6.9551e8
  
  beta0 = log(R0^2/L0)+log(4*pi)+log(sigma)
  
  return(R0^2/L0*4*pi*radius^2*sigma*temp^4)
}

ms = stars[which(stars$Type == "Main Sequence" & stars$L<1.5e5),]
d <- stars[which(stars$Type == "Brown Dwarf" | stars$Type == "White Dwarf" | stars$Type == "Red Dwarf"),]
s <- stars[which(stars$Type == "Super Giants" | stars$Type == "Hyper Giants"),]

plot(sb(ms$R,ms$Temperature),ms$L, main="Theoretical Model vs Empirical Data")
abline(0,1,col='red')

plot(sb(d$R,d$Temperature),d$L, main="Dwarfs: Theoretical Model vs Empirical Data")
abline(0,1,col='red')

plot(sb(s$R,s$Temperature),s$L, main="Giants: Theoretical Model vs Empirical Data",xlim=c(0,1e6))
abline(0,1,col='red', xlim=c(0,1e6))

# Per le stelle giganti e le stelle nane, la legge non vale. Non c'e'
# alcuna relazione lineare tra luminosita' ~ raggio+temperatura
