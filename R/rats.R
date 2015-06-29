#' Function to transform an unordered rats dataset in the right format for R
#' built: 2013-09-30, Patrick Kraft
#' @param data dataset loaded in R
#' @param items number of variables in dataser
#' @param byrow logical, is data organized by row?
#' @return usable dataframe
#' @export
#' 

rats <- function(data, items, byrow = T) {
    if (byrow == T) {
        data <- stack(data.frame(t(data)))[, 1]
    } else {
        data <- stack(data)[, 1]
    }
    datapoints <- length(data)
    cols <- length(items)
    rows <- length(data)/cols
    newdata <- data.frame(matrix(data, nrow = rows, ncol = cols, byrow = T))
    colnames(newdata) <- items
    return(newdata)
} 
