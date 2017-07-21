\name{esPlotpairs}
\alias{esPlotpairs}
\title{esPlotpairs}
\description{
Cuts out the underscore from esAnnotate function and plots it using ggpairs. Converts to ggplot format for plotting and can also select genes and covariates that are denoted in the argument.It also can reannotate an expression set and modify the feature names that can be useful for plotting.
}
\usage{
esPlotpairs(es, select_covar, features, annot, annotate = TRUE, drop_endids = TRUE)
}

\arguments{
  \item{es}{expression set object

}
  \item{select_covar}{list of covariate (varLabels of the expressionset)

}
  \item{features}{list of genes (featureNames of expressionset)

}
  \item{annot}{annotaiton file to use (for PSR or Transcripts)

}
  \item{annotate}{annotate (TRUE//FALSE) - Changes the featureNames to the annotation needed

}
  \item{drop_endids}{drop_endids  - In changing annotation, need to maintiain uniqueness, which is done by joining the featureNames with the annotation (using underscore), if TRUE for plotting purposes, the original featureNames (ids) are not shown.
%%     ~~Describe \code{drop_endids} here~~
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
#esPlotpairs(eset, select_covar="group_of_interest", features="genes_interest", annot="anno_data", annotate = TRUE, drop_endids = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
