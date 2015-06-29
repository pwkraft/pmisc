#' Function to convert ARIMA results in a latex table
#'
#' @param model Arima model
#' @param tex logical, should output be in latex?
#' @param fixed vector indicating fixed parameters with NAs, default is NULL
#' @param pr logical, save output in file?
#' @return model output
#' @note built: 2013-09-30
#' @export
#' @import xtable
#' @import TSA

arimatex <- function(model, tex = T, fixed = NULL, pr = F) {
    if (sum(grep("Arima", class(model))) == 0) 
        stop("This is not an ARIMA model!")
    
    ## taking care of fixed values
    if (!is.null(fixed)) 
        model$coef <- model$coef[is.na(fixed)]
    
    ## Coefficients
    coefs <- cbind(model$coef, sqrt(diag(model$var.coef)))
    colnames(coefs) <- c("Estimates", "Std. Error")
    
    ## Model Fit Statistics
    k <- length(model$coef)
    n <- length(model$resid)
    
    # Log-Likelihood
    loglik <- model$loglik
    
    # Sum of Squared Residuals + RMsE
    ssr <- sum((model$resid)^2)
    rmse <- sqrt(mean((model$resid)^2))
    
    # Ljung-Box Q-Statistic
    ljung <- Box.test(model$resid, lag = trunc(n/4), type = "Ljung-Box", fitdf = k)
    qval <- ljung$statistic
    pqval <- ljung$p.value
    
    # Durbin- Watson Statistic
    dw <- sum((model$resid - lag(model$resid))^2, na.rm = TRUE)/sum(model$resid^2, 
        na.rm = TRUE)
    
    # AIC and SBC: formula from enders book, not equal to R-result (AIC*, BIC*)
    aic <- n * log(ssr) + 2 * k
    sbc <- n * log(ssr) + k * log(n)
    
    fits <- rbind(loglik, ssr, rmse, qval, pqval, dw, aic, sbc)
    colnames(fits) <- c("Fit Statistic")
    rownames(fits) <- c("Log Likelihood", "SSR", "RMSE", paste("Ljung-Box Q(", trunc(n/4), 
        ",", k, ")", sep = ""), "P(Q)", "Durbin Watson", "AIC", "SBC")
    
    out <- rbind(coefs, cbind(fits, NA))
    
    if (tex == T) {
        if (pr == T) {
            print(xtable(out, caption = c("Parameter Estimates and Fit Statistics for ARIMA Model"), 
                align = "rcc", digits = 3, label = paste("tab:", deparse(substitute(model)), 
                  sep = "")), type = "latex", file = paste(deparse(substitute(model)), 
                ".tex", sep = ""), caption.placement = "top", hline.after = c(-1, 
                c(0, k), nrow(out)), booktabs = F)
        } else {
            print(xtable(out, caption = c("Parameter Estimates and Fit Statistics for ARIMA Model"), 
                align = "rcc", digits = 3, label = paste("tab:", deparse(substitute(model)), 
                  sep = "")), caption.placement = "top", hline.after = c(-1, c(0, 
                k), nrow(out)), booktabs = F)
        }
    } else {
        cat("\n")
        print(round(out, 3))
    }
} 
