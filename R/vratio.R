#' Function runs the variance ratio unit root test outlined in Cochrane (1988),
#' Diebold (1989) and Lo and McKinley (1988) Code is translatet from the original
#' (at)vratio procedure for RATS by Christopher J. Zorn built: 2013-10-28; Patrick
#' Kraft
#' @param series time series to test for unit root
#' @param lags number of lags, default is 2
#' @return variance ratio test result
#' @export

vratio <- function(series, lags = 2) {
    nobs <- length(series)
    xdiff <- diff(series)
    sumser <- sum(xdiff)
    meandif <- sumser/(nobs - 1)
    xdiffsq <- xdiff^2
    numone <- sum(xdiffsq)
    varone <- numone/(nobs - 1)
    ratio <- NULL
    for (i in 1:length(lags)) {
        xdflag <- diff(series, lag = lags[i])
        xdflagsq <- (xdflag - (lags[i] * meandif))^2
        numlag <- sum(xdflagsq)
        varlag <- numlag/(nobs - lags[i])
        ratio[i] <- (lags[i] * varone)/varlag
    }
    out <- cbind(lags, ratio)
    colnames(out) <- c("Lags", "Variance Ratio")
    return(out)
} 
