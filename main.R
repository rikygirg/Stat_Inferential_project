library(MASS)
library(GGally)
library(RColorBrewer)
library(car)
library(plotly)
library(dplyr)

source("diagnostic.R")


analysis <- function(data, flag = FALSE) {
    log_l <- log(data$L)
    log_r <- log(data$R)
    log_t <- log(data$Temperature)

    mod <- lm(log_l ~ log_r + log_t + data$A_M, data = data)
    gs <- summary(mod)

    print(gs)

    r_squared <- gs$r.squared

    # Homoschedasticity????

    # Normality Check
    if (flag) {
        qqnorm(mod$residuals)
        qqline(mod$residuals)
    }
    sh <- shapiro.test(mod$residuals)$p.value

    print(paste("R^2: ", r_squared))
    print(paste("Shapiro P-Value: ", sh))
    return(mod)
}

stars <- read.csv("new_stars.csv")
attach(stars)

mod <- analysis(stars)
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
p <- leverage[1]
n <- leverage[2]
g_post_lev <- lm(log_l ~ log_r + log_t, data = stars, subset = (leverage[1] < 2 * p / n)) # nolint: line_length_linter.
summary(g_post_lev)
AIC(g_post_lev)

g_post_rs <- lm(log_l ~ log_r + log_t, data = stars, subset = (abs(stud) < 2))
summary(g_post_rs)
AIC(g_post_rs)

g_post_both <- lm(log_l ~ log_r + log_t, data = stars, subset = (abs(stud) < 2) | lev < 2 * p / n) # nolint : line_length_linter.
summary(g_post_both)
AIC(g_post_both)

best_removal(log_r, log_t, log_l, stars)

# scegliamo il modello senza residui studentizzati perche' ha AIC minore

plot(g_post_rs, which = 1)
shapiro.test(g_post_rs$residuals)

# normalita' ed omoschedastici fanno sempre schifo, proviamo a vedere se
# intraclasse qualcosa migliora

# raggruppiamo le stelle in tre classi: giants, main sequence, white dwarf
m_sequence <- stars[which(stars$Type == "Main Sequence"), ]
white_dwarf <- stars[which(stars$Type == "White Dwarf"), ]
red_dwarf <- stars[which(stars$Type == "Red Dwarf"), ]
brown_dwarf <- stars[which(stars$Type == "Brown Dwarf"), ]
super_giants <- stars[which(stars$Type == "Super Giants" | stars$Type == "Hyper Giants"), ]

# ripetiamo analisi di punti leva e residui per ogni classe

mod_m <- analysis(m_sequence)
analysis(white_dwarf)
analysis(brown_dwarf)
analysis(red_dwarf)
analysis(super_giants)
analysis(hyper_giants)

# scartiamo la covariate A_M, non serve T-test alto
log_l_m <- log(m_sequence$L)
log_r_m <- log(m_sequence$R)
log_t_m <- log(m_sequence$Temperature)

mod_m <- lm(log_l_m ~ log_t_m + log_r_m, data = m_sequence)
summary(mod_m)

# facciamo pulizia: residui, leverages etc

lev <- hatvalues(mod_m)
p <- mod_m$rank
n <- dim(m_sequence)[1]

print(paste(2 * p / n))

watchout_points <- lev[which(lev > 2 * p / n)]
watchout_idx <- seq_along(lev)[which(lev > 2 * p / n)]

plot(mod_m$fitted.values, lev, main = "Leverages", pch = 16, col = "black") # nolint
points(mod_m$fitted.values[watchout_idx], watchout_points, col = "red", pch = 16) # nolint
abline(h = 2 * p / n, lty = 2, col = "red")

res_stu <- studentized_residuals(mod_m, TRUE)
res_std <- standardized_residuals(mod_m, TRUE)

g_post_lev <- lm(log_l_m ~ log_t_m + log_r_m, data = m_sequence, subset = lev < 2 * p / n)
g_post_rs <- lm(log_l_m ~ log_r_m + log_t_m, data = m_sequence, subset = (abs(res_stu) < 2))

# Intervallo di confidenza sui parametri

# Assuming you want to test the hypothesis for the coefficient of log_t_m in g_post_lev
summary_g_post_lev <- summary(mod_m)

# Extract the estimate and standard error for log_t_m
beta_hat <- summary_g_post_lev$coefficients["log_t_m", "Estimate"]
se_beta_hat <- summary_g_post_lev$coefficients["log_t_m", "Std. Error"]

# Hypothesized value
beta_0 <- 4

# Calculate the Wald statistic
wald_statistic <- (beta_hat - beta_0)^2 / (se_beta_hat^2)

# Get the p-value from the chi-squared distribution with 1 degree of freedom
p_value <- 1 - pchisq(wald_statistic, df = 1)

# Print the Wald statistic and p-value
cat("Wald statistic:", wald_statistic, "\nP-value:", p_value, "\n")

# Decision rule at 5% significance level
if (p_value < 0.05) {
    cat("Reject the null hypothesis: there is evidence that beta != 4.\n")
} else {
    cat("Fail to reject the null hypothesis: there is not enough evidence to conclude that beta != 4.\n")
}
