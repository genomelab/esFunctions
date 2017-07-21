\name{glmfunction}
\alias{glmfunction}
\title{glmfunction}
\description{
Perform differential expression analysis for all possible combination of from a vector of data and computes the mean, fold change, p-value, chi-squared values, and generates a score for ranking.
}
\usage{
glmfunction(y, var1, contrastmodel = NULL)

}

\arguments{
  \item{y}{a vector of data}
  \item{var1}{the covariate}
  \item{contrastmodel}{NULL}
}

\author{
Dr. Shahab Asgharzadeh \cr
Janahan Gnanachandran \cr
Anthony Colombo \cr
Alexander Magsarili \cr
}


\examples{
#x <- c(2,3,4,7,9,4,10,6,4,2)
#y <- as.factor(c("a","b","c","a","b","c","b","a","c","c"))
#results <- glmfunction(x,y) 
#results 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
