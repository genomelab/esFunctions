\name{esReduce2}
\alias{esReduce2}
\title{esReduce2}
\description{
ExpressionSet with featureNames
}
\usage{
esReduce2(es, annovec, ann2featMap, pdvname = "symbol", collapseFun = NULL)
}

\arguments{
  \item{es}{expression set

}
  \item{annovec}{annotation vector, a collection of annotations

}
  \item{ann2featMap}{is either an AtomicAnnDbBimap with keys in a and range in f, or a list with element names in a and element values in f

}
  \item{pdvname}{gene symbol

}
  \item{collapseFun}{collapse function, a function that applys to 

}
}


\author{
Dr. Shahab Asgharzadeh \cr
Janahan Gnanachandran \cr
Anthony Colombo \cr
Alexander Magsarili \cr


}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#esReduce2(eset, annovec, ann2featMap, pdvname = "symbol", collapseFun = NULL)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
