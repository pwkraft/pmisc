#' Function that allows searching for variable names in a dataset.  it shows all
#' variable names that are fit to the (truncated) search entry and reports whether
#' the item list was complete and exact (i.e. not truncated) built: 2013-09-28,
#' Patrick Kraft
#' @param data data frame
#' @param term term to look for in variable names
#' @param summarize logical, include summary statistics for variables?
#' @return variable list
#' @export
#' 

lookfor <- function(data, term, summarize = F) {
    out <- NULL
    for (i in 1:length(term)) {
        out <- c(out, names(data[grep(term[i], names(data))]))
    }
    if (summarize == T) {
        out <- vector("list", length = length(out))
        for (i in 1:length(out)) {
            out[[i]] <- summary(data[grep(term[i], names(data))])
        }
    }
    complete = length(term) == length(out)
    exact = sum(term == out) == length(out)
    return(list(variables = out, complete = complete, exact = exact))
} 
