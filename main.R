library(MASS)
library(GGally)
library(RColorBrewer)
library(car)
library(plotly)
library(dplyr)

source("diagnostic.R")


stars <- read.csv("new_stars.csv")
attach(stars)

m <- stars[which(stars$Type == "Main Sequence" | stars$Type == "Brown Dwarf"), ]

log_l <- log(stars$L)
log_r <- log(stars$R)
log_t <- log(stars$Temperature)

mod <- lm(log_l ~ log_r + log_t, data = stars)
summary(mod)

# DIAGNOSTICA
# punti leva

par(mfrow = c(1, 3))
leverage <- leverage_points(mod, TRUE)
res_stu <- studentized_residuals(mod, TRUE)
res_std <- standardized_residuals(mod, TRUE)

# non colora i puntini, questo e' da fixare
# residui standarizzati e studentizzati si equivalgono

# cooks distance?
# punti influenti?

# decidiamo quali modello scegliere, rimuoviamo un po' di merda

g_post_lev <- lm(log_l ~ log_r + log_t, data = stars, subset = (lev < 2 * p / n)) # nolint: line_length_linter.
summary(g_post_lev)
AIC(g_post_lev)

g_post_rs <- lm(log_l ~ log_r + log_t, data = stars, subset = (abs(stud) < 2))
summary(g_post_rs)
AIC(g_post_rs)

g_post_both <- lm(log_l ~ log_r + log_t, data = stars, subset = (abs(stud) < 2) | lev < 2 * p / n) # nolint : line_length_linter.
summary(g_post_both)
AIC(g_post_both)

# scegliamo il modello senza residui studentizzati perche' ha AIC minore

plot(g_post_rs, which = 1)
shapiro.test(g_post_rs$residuals)

plot(g_post_lev, which = 1)
shapiro.test(g_post_lev$residuals)
