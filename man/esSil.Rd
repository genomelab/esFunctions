% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esSil.R
\name{esSil}
\alias{esSil}
\title{esSil}
\usage{
esSil(es, clusters, covar_name = "")
}
\arguments{
\item{es}{expression set}

\item{clusters}{Number of clusters}

\item{covar_name}{covariate name}
}
\description{
Identify and draw sil_width for a clustering pattern. It requires package
'cluster'. It adds the NMF cluster and silhouette width in the es dataframe
(using covar_name + cluster or sil_width)
}
\note{
Requires package 'cluster'
}
\examples{

#esSil(eset, clusters, covar_name = "covariate_name_of_interest")

}
\references{
"An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
February, 2007 \cr
}
\author{
Shahab Asgharzadeh
}
\keyword{~kwd1}
\keyword{~kwd2}
