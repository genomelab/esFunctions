\name{esPathwaySig}
\alias{esPathwaySig}
\title{esPathwaySig}
\description{
Runs the complete pathway analysis for a given expression set and the pathway of interest.
}
\usage{
esPathwaySig(es, covar, nsim = 10, anovaresults, msigdb)
}

\arguments{
  \item{es}{expression set

}
  \item{covar}{covariate

}
  \item{nsim}{number of simulate. Default to 10. Typically run for 10,000.

}
  \item{anovaresults}{results from anova

}
  \item{msigdb}{pathway from BROAD Institute

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
#esPathwaySig(eset, covar="covariate_of_interest", nsim = 10, anovaresults, msigdb)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
