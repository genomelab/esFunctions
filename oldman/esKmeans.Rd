\name{esKmeans}
\alias{esKmeans}
\title{esKmeans}
\description{ esKmeans function finds the kmeans clusters of a expressionset and adds the cluster info to the covariate file}
\usage{
esKmeans(es, kmeans = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set}
  \item{kmeans}{number of clusters assumed and returns an expressionset object}
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
#results<- esKmeans(eset, kmeans = 2)
#results 
}
