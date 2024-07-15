# This file contains some diagnostic tools, which can
# be imported using `source(diagnostic.R)` and used in the main regression file

# Leverage Points Analysis
leverage_points <- function(mod, flag) {
    lev <- hatvalues(mod)
    p <- mod$rank
    n <- dim(model.matrix(mod))[1]

    watchout_points <- lev[which(lev > 2 * p / n)]
    watchout_idx <- seq_along(lev)[which(lev > 2 * p / n)]

    if (flag) {
        plot(mod$fitted.values, lev, main = "Leverages", pch = 16, col = "black") # nolint
        points(mod$fitted.values[watchout_idx], watchout_points, col = "red", pch = 16) # nolint
        abline(h = 2 * p / n, lty = 2, col = "red")
    }

    return(lev)
}


standardized_residuals <- function(mod, flag) {
    gs <- summary(mod)
    res_std <- mod$res / gs$sigma

    idx <- which(abs(res_std) > 2)
    watchout_rstd <- res_std[idx]

    if (flag) {
        plot(mod$fitted.values, res_std, main = "Standardized Residuals", pch = 16) # nolint
        points(mod$fitted.values[idx], watchout_rstd[idx], col = "green", pch = 16) # nolint
        abline(h = c(-2, 2), lty = 2, col = "orange")
    }

    return(res_std)
}


studentized_residuals <- function(mod, flag) {
    stud <- rstandard(mod)

    idx <- which(abs(stud) > 2)
    watchout_rstu <- stud[idx]

    if (flag) {
        plot(mod$fitted.values, stud, main = "Studentized Residuals", pch = 16) # nolint
        points(mod$fitted.values[idx], watchout_rstu[idx], col = "red", pch = 16) # nolint
        abline(h = c(-2, 2), lty = 2, col = "orange")
    }

    return(stud)
}


best_removal <- function(x, y, z, data) {
    mod <- lm(z ~ x + y, data = data)
    leverage <- leverage_points(mod, FALSE)
    res_stu <- studentized_residuals(mod, FALSE)
    res_std <- standardized_residuals(mod, FALSE)

    lev <- leverage[1]
    p <- leverage[2]
    n <- leverage[3]

    g_post_lev <- lm(z ~ x + y, data = data, subset(lev < 2 * p / n))
    g_post_stu <- lm(z ~ x + y, data = data, subset(abs(res_stu) < 2))
    g_post_std <- lm(z ~ x + y, data = data, subset(abs(res_std) < 2))

    AIC_lev <- AIC(g_post_lev)
    AIC_stu <- AIC(g_post_stu)
    AIC_std <- AIC(g_post_std)

    lowest_AIC <- min(AIC_lev, AIC_stu, AIC_std)

    if (lowest_AIC == AIC_lev) {
        print("Best Removal: Leverage Points")
    } else if (lowest_AIC == AIC_stu) {
        print("Best Removal: Studentized Residuals")
    } else {
        print("Best Removal: Standardized Residuals")
    }
}

diagnostica <- function(model){
    par(mfrow=c(1,2))
    # omoscedasticita'
    plot(model,which=1)
    # normalita' dei residui
    qqnorm(model$residuals)
    qqline(model$residuals,col='red')
    p_value <- shapiro.test(model$residuals)$p.value
    print(paste("Shapiro Test = ",p_value))
}
