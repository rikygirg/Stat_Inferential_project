library(MASS)
library(GGally)
library(RColorBrewer)
library(car)
library(plotly)
library(dplyr)
library(caret)

source("diagnostic.R")

# Importiamo il dataset
stars <- read.csv("new_stars.csv")
attach(stars)

# Come e' fatto il dataset?
dim(stars)
head(stars)

ggpairs(stars,col=c(1:4),aes(colour = stars$Type))

# Togliamo colore e spectral class?

# Vogliamo fittare un modello che metta in relazione la luminosita'
# di una stella con gli altri parametri numerici. Per il momento escludiamo
# classe, variabile categorica. Facciamo il fit sui logaritmi

log_l <- log(stars$L)
log_r <- log(stars$R)
log_t <- log(stars$Temperature)

mod <- lm(log_l ~ log_r + log_t + stars$A_M, data = stars)
summary(mod)

plot(mod$fitted.values,mod$residuals)
qqnorm(mod$residuals)
qqline(mod$residuals,col='red')
shapiro.test(mod$residuals)

# dividiamo la cosa per classi
detach(stars)

m_sequence <- stars[which(stars$Type == "Main Sequence"), ]
attach(m_sequence)

dwarf <- stars[which(stars$Type == "Brown Dwarf" | stars$Type == "Red Dwarf" | stars$Type == "White Dwarf"), ]

giants <- stars[which(stars$Type == "Super Giants" | stars$Type == "Hyper Giants"), ]

log_l_m = log(m_sequence$L)
log_r_m = log(m_sequence$R)
log_t_m = log(m_sequence$Temperature)

mod_m <- lm(log_l_m ~ log_r_m + log_t_m, data = m_sequence)


shapiro.test(mod_m$residuals)
