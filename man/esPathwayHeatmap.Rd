\name{esPathwayHeatmap}
\alias{esPathwayHeatmap}
\title{esPathwayHeatmap}
\description{
Plot a heat map for genes belong to a specific pathway using the expression set and the pathway analysis results.
}
\usage{
esPathwayHeatmap(es, covar, covargroups, selpathway, pathwayanalysis, annotate = TRUE, col = colorRampPalette(c("navy", "white", "firebrick3"))(50), colorPalette = "")
}

\arguments{
  \item{es}{expression set}
  \item{covar}{covariate}
  \item{covargroups}{covariate groups of interest

}
  \item{selpathway}{select pathway of interest

}
  \item{pathwayanalysis}{pathway analysis results

}
  \item{annotate}{annotation. \code{TRUE} or \code{FALSE}

}
  \item{col}{color palette

}
  \item{colorPalette}{colorPallette has to be a list of vectors, each with number of colors. By default it creates a recurring list of colors obtained from global kmPallette, Pallette1, Pallette2 covar should be a list of characters i.e. c('risk', 'efscens).
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
#esPathwayHeatmap(eset, covar="covariate_of_interest", covargroups, selpathway, pathwayanalysis, annotate = TRUE, col = colorRampPalette(c("navy", "white", "firebrick3"))(50))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
