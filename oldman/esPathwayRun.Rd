\name{esPathwayRun}
\alias{esPathwayRun}
\title{esPathwayRun}
\description{
For a given pathway type this will load the curated msidb and call the esPathwaySig function. This could be applied to any pathway files.
}
\usage{
esPathwayRun(es, covar, nsim = 10, anovaresults, pathwaytype = "c2.cp.kegg")
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
  \item{pathwaytype}{pathway type, gene set of interest from BROAD Institute

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
#esPathwayRun(eset, covar="covariate_of_interest", nsim = 10, anovaresults, pathwaytype = "c2.cp.kegg")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
