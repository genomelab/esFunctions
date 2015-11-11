\name{esAddPheno_OnevsOthers}
\alias{esAddPheno_OnevsOthers}
\title{esAddPheno_OnevsOthers}
\description{
Function to take a covariate and build several others that will generate multiple covariates with each covariate versus combine of others. The returned es will have x number of new columns (x being number of variables in covar). The labels will be combine_x1 (where x1 is name of the variable in covar)
}
\usage{
esAddPheno_OnevsOthers(es, covar)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set}
  \item{covar}{covariate}
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
#results <- esAddPheno_OnevsOthers(eset, covariate_of_interest)
#results
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
