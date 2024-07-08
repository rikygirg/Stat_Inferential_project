library(MASS)
library(GGally)
library(RColorBrewer)
library(car)
library(plotly)
library(dplyr)


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
lev <- hatvalues(mod)
p <- mod$rank
n <- dim(stars)[1]

watchout_points <- lev[which(lev > 2 * p / n)]
watchout_idx <- seq_along(lev)[which(lev > 2 * p / n)]

plot(mod$fitted.values, lev, main = "Leverages", pch = 16, col = "black")
abline(h = 2 * p / n, lty = 2, col = "red")
points(mod$fitted.values[watchout_idx], watchout_points, col = "red", pch = 16)

# residui standardizzati
gs <- summary(mod)
res_std <- mod$res / gs$sigma

watchout_idx_std <- which(abs(res_std) > 2)
watchout_rstd <- res_std[watchout_idx_std]

# residui studentizzati
stud <- rstandard(mod)

watchout_idx_rstu <- which(abs(stud) > 2)
watchout_rstu <- stud[watchout_idx_rstu]

# residui standarizzati e studentizzati si equivalgono

plot(mod$fitted.values, stud, main = "Studentized Residuals", pch = 16)
points(mod$fitted.values[watchout_idx_rstu], stud[watchout_idx_rstu], col = "pink", pch = 16)
abline(h = c(-2, 2), lty = 2, col = "orange")

# cooks distance?

par(mfrow = c(1, 3))
plot(mod$fitted.values, res_std, main = "Standardized Residuals", pch = 16)
points(mod$fitted.values[watchout_idx_rstu], stud[watchout_idx_std], col = "green", pch = 16)

plot(mod$fitted.values, stud, main = "Studentized Residuals", pch = 16)
points(mod$fitted.values[watchout_idx_rstu], stud[watchout_idx_rstu], col = "pink", pch = 16)
abline(h = c(-2, 2), lty = 2, col = "orange")

plot(mod$fitted.values, lev, main = "Leverages", pch = 16, col = "black")
abline(h = 2 * p / n, lty = 2, col = "red")
points(mod$fitted.values[watchout_idx], watchout_points, col = "red", pch = 16)

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

# box-cox transformation
b <- boxcox(g_post_rs)
lambda <- b$x[which.max(b$y)] # we can't cause the logarithm is not positive

# summary(mod_nelev)

# residui standardizzati
# res_std <- mod$residuals / summary(mod)$sigma
# watchout <- which(abs(res_std) > 2)
# res_watchout <- res_std[watchout]

# mod_std <- lm(logL ~ logR + logT, subset = (res_std < 2))
# summary(mod_std)

# shapiro.test(mod_std$residuals)


# plot(mod$fitted.values, mod$residuals)
# qqnorm(mod$residuals)
# qqline(mod$residuals, col = "red")

# shapiro.test(mod$residuals)
