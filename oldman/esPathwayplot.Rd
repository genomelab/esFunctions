\name{esPathwayplot}
\alias{esPathwayplot}
\title{esPathwayplot}
\description{
This will plot the pathway results based on chi-squared value and the eucledian distance. Number of top pathways could be labeled with \code{labeltopsigpaths} option.
}
\usage{
esPathwayplot(esPathwaySig_results, covarcompare, fdrlevel = 0.05, labeltopsigpaths = c(1:5))
}

\arguments{
  \item{esPathwaySig_results}{pathway analysis results from \code{esPathwaySig}

}
  \item{covarcompare}{covariate comparison of interest

}
  \item{fdrlevel}{level of false discovery rate to be considered

}
  \item{labeltopsigpaths}{Number of top pathways could be labeled

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
#esPathwayplot(esPathwaySig_results, covarcompare, fdrlevel = 0.05, labeltopsigpaths = c(1:5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
