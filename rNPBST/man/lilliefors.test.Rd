% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NP-GoodnessTests.R
\name{lilliefors.test}
\alias{lilliefors.test}
\title{Lilliefors test for goodness of fit}
\usage{
lilliefors.test(sequence, distribution = "NORMAL")
}
\arguments{
\item{sequence}{Secuence of data}

\item{distribution}{Distribution name to perform test}
}
\value{
A htest object with pvalues and statistics
}
\description{
This function performs the Lilliefors test
}

