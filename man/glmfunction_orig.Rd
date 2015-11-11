\name{glmfunction_orig}
\alias{glmfunction_orig}
\title{General Linear Model}
\description{
Glmfunction calls the in-house "glm" function to include p-value, chi-squared, means, but does not compare between groups. The calculated values are only done for a single vector of data. 
}
\usage{
glmfunction_orig(y, var1)
}

\arguments{
  \item{y}{
   the vector of data}
  \item{var1}{
  the covariate (n levels) for ANOVA analyses}
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
#results <- glmfunction_orig(x,y)
#results 
 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
