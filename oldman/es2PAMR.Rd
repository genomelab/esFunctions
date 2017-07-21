\name{es2PAMR}
\alias{es2PAMR}
\title{es2PAMR}
\description{
Sets up a list to be used for PAMR
}
\usage{
es2PAMR(es, covar1, covar2 = NULL, survival = FALSE)
}

\arguments{
  \item{es}{expression set

}
  \item{covar1}{1st covariate

}
  \item{covar2}{2nd covariate

}
  \item{survival}{If survival = TRUE, then covar1 is the survival time and covar2 is the censor data

}
}

\references{
"An Introduction to Bioconductor's ExpressionSet Class" \cr
Seth Falcon, Martin Morgan, and Robert Gentleman \cr
6 October, 2006; revised 9 February, 2007 \cr

}
\author{
Dr. Shahab Asgharzadeh \cr
Janahan Gnanachandran \cr
Anthony Colombo \cr
Alexander Magsarili \cr

}

\examples{
#PAMR <- es2PAMR(eset, covariate1, covariate2 = NULL, survival = FALSE)
#PAMR
}
