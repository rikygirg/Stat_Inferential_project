# This file contains some diagnostic tools, which can
# be imported using `source(diagnostic.R)` and used in the main regression file

# Leverage Points Analysis
leverage_points <- function(mod, flag) {
    lev <- hatvalues(mod)
    p <- mod$rank
    n <- dim(stars)[1]

    watchout_points <- lev[which(lev > 2 * p / n)]
    watchout_idx <- seq_along(lev)[which(lev > 2 * p / n)]

    if (flag) {
        plot(mod$fitted.values, lev, main = "Leverages", pch = 16, col = "black") # nolint
        points(mod$fitted.values[watchout_idx], watchout_points, col = "red", pch = 16) # nolint
        abline(h = 2 * p / n, lty = 2, col = "red")
    }

    return(list(leverage = lev, p, n))
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
