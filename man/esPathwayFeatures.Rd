\name{esPathwayFeatures}
\alias{esPathwayFeatures}
\title{esPathwayFeatures}
\description{
Finds Top scoring Pathways (using Score Calculated by multiplying pathway eucledian distance and SD of Chi2
}
\usage{
esPathwayFeatures(esPathwaySig_results, pathname, anno = NULL, column_lookup = "gene_symbol")
}

\arguments{
  \item{esPathwaySig_results}{pathway analysis results from \code{esPathwaySig}

}
  \item{pathname}{pathname of interest

}
  \item{anno}{is an annotation object (first column is the probe_id)

}
  \item{column_lookup}{column_lookup of interest

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
#esPathwayFeatures(esPathwaySig_results, pathname, anno = NULL, column_lookup = "gene_symbol")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
