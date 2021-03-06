% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CowbellAnalyzer.R
\name{print.cowbell}
\alias{print.cowbell}
\title{Summarizes the cowbell regression analysis}
\usage{
\method{print}{cowbell}(x, ...)
}
\arguments{
\item{x}{Object generated with function \code{\link{generateCowbell}}}

\item{...}{Only for compatibility purposes.}
}
\description{
Prints the used formula, the R squared and the F statistics in comparison with a constant function
(average of values).
}
\details{
Implementation of the \code{\link{print}} generic.
}
\examples{
# Run a simplified anaylsis with 10 iterations only (to save time.)
concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
data(allFun)
test<-generateCowbell(concept, allFun, 10)
test
}
