\name{esPctiles}
\alias{esPctiles}
\title{esPctiles}
\description{
Returns an expressionset class where the values have been converted to percentiles 
i.e. Calculate empirical continous desnity function and create a expression set based on that.
}
\usage{
esPctiles(eset)
}

\arguments{
  \item{eset}{expression set

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


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
#ecdf_on_assay_data <- esPctiles(eset)
#ecdf_on_assay_data
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
