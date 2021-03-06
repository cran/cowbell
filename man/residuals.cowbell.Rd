% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CowbellAnalyzer.R
\name{residuals.cowbell}
\alias{residuals.cowbell}
\title{Implementation of the \code{\link{residuals}} generic.}
\usage{
\method{residuals}{cowbell}(object, ...)
}
\arguments{
\item{object}{The data obtained by function \code{\link{generateCowbell}}.}

\item{...}{Just for compatibility purposes.}
}
\value{
Vector with the residuals to the data.
}
\description{
Implementation of the \code{\link{residuals}} generic.
}
\examples{
# Run a simplified anaylsis with 10 iterations only (to save time.)
concept<-generateCowbellConcept(Fun ~ Fluency * Absorption, 1, 9, 1, 7, 1, 7)
data(allFun)
test<-generateCowbell(concept, allFun, 10)
residuals(test)
}
