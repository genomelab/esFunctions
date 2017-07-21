\name{esSil}
\alias{esSil}
\title{esSil}
\description{
Identify and draw sil_width for a clustering pattern. It requires package 'cluster'. It adds the NMF cluster and silhouette width in the es dataframe (using covar_name + cluster or sil_width)
}
\usage{
esSil(es, clusters, covar_name = "")
}

\arguments{
  \item{es}{expression set

}
  \item{clusters}{Number of clusters 

}
  \item{covar_name}{covariate name

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
\note{
Requires package 'cluster'
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#esSil(eset, clusters, covar_name = "covariate_name_of_interest")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
