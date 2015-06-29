#' Function to display some diagnostics for (f)ECM model list in a latex table
#' built: 2013-10-29; Patrick Kraft
#' @import xtable
#' @export
#' @param models (f)ECM model list
#' @param tex logical, should output be in latex
#' @param pr logical, should output be saved in new file
#' @param caption Caption of table
#' @param label Latex label
#' @return model output
#' 

diagtex <- function(models, tex = T, pr = F, caption = "Diagnostics and Fit Statistics", 
    label = NULL) {
    out <- NULL
    for (i in 1:length(models)) {
        ## Model Fit Statistics
        k <- length(models[[i]]$coef)
        n <- length(models[[i]]$resid)
        
        # Sum of Squared Residuals + RMsE
        ssr <- sum((models[[i]]$resid)^2)
        rmse <- sqrt(mean((models[[i]]$resid)^2))
        
        # Ljung-Box Q-Statistic
        ljung <- Box.test(models[[i]]$resid, lag = trunc(n/4), type = "Ljung-Box", 
            fitdf = k)
        qval <- ljung$statistic
        pqval <- ljung$p.value
        
        # Durbin- Watson Statistic
        tmp <- ts(models[[i]]$resid)
        dw <- sum((tmp - lag(tmp))^2, na.rm = TRUE)/sum(tmp^2, na.rm = TRUE)
        
        # AIC and SBC: formula from enders book, not equal to R-result (AIC*, BIC*)
        aic <- n * log(ssr) + 2 * k
        sbc <- n * log(ssr) + k * log(n)
        
        # Combine Values
        fits <- rbind(ssr, rmse, qval, pqval, dw, aic, sbc)
        out <- cbind(out, fits)
        
    }
    rownames(out) <- c("SSR", "RMSE", paste("Ljung-Box Q(", trunc(n/4), ",", k, ")", 
        sep = ""), "P(Q)", "Durbin Watson", "AIC", "SBC")
    # careful: it only displays n and k for last model!  I'll leave this for later,
    # it doesn't matter for the models since n and k are constant
    colnames(out) <- paste("(", seq(1, length(models)), ")", sep = "")
    
    if (tex == T) {
        if (pr == T) {
            print(xtable(out, caption = caption, align = paste0("r", paste0(rep("c", 
                length(models)), collapse = ""), collapse = ""), digits = 3, label = label), 
                type = "latex", file = paste(label, ".tex", sep = ""), caption.placement = "top", 
                hline.after = c(-1, -1, 0, nrow(out), nrow(out)), booktabs = F)
        } else {
            print(xtable(out, caption = caption, align = paste0("r", paste0(rep("c", 
                length(models)), collapse = ""), collapse = ""), digits = 3, label = label), 
                caption.placement = "top", hline.after = c(-1, -1, 0, nrow(out), 
                  nrow(out)), booktabs = F)
        }
    } else {
        cat("\n")
        print(round(out, 3))
    }
} 
