% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/es2PAMR.R
\name{es2PAMR}
\alias{es2PAMR}
\title{es2PAMR}
\usage{
es2PAMR(es, covar1, covar2 = NULL, survival = FALSE)
}
\arguments{
\item{es}{expression set}

\item{covar1}{1st covariate}

\item{covar2}{2nd covariate}

\item{survival}{If survival = TRUE, then covar1 is the survival time and
covar2 is the censor data}
}
\description{
Sets up a list to be used for PAMR
}
\examples{

#PAMR <- es2PAMR(eset, covariate1, covariate2 = NULL, survival = FALSE)
#PAMR

}
\references{
"An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
February, 2007 \cr
}
\author{
Shahab Asgharzadeh
}
