% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sim.R
\name{sim}
\alias{sim}
\title{Simulate expected values/first differences (replaces Zelig)
built: 2016-08-27, Patrick Kraft}
\usage{
sim(models, iv, robust = F, ci = c(0.025, 0.975), nsim = 1000)
}
\arguments{
\item{models:}{list of model results (lm, glm, or vglm/tobit)}

\item{iv:}{data frame containing the values for comparison (only 2 rows, selected variables)}

\item{robust:}{logical, should robust standard errors be used}

\item{nsim:}{number of simulations}
}
\value{
data.frame: contains expected values, confidence intervals, variable names
}
\description{
Simulate expected values/first differences (replaces Zelig)
built: 2016-08-27, Patrick Kraft
}

