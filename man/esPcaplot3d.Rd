\name{esPcaplot3d}
\alias{esPcaplot3d}
\title{esPcaplot3d}
\description{
esPcaplot3d, works on macs or systems with X windows. PCA plot of selected features of an expressionset class in 3D.
}
\usage{
esPcaplot3d(es, features = "", covar = "", labelvar = "ignored", pcs = c(1, 2, 3), scale = TRUE, center = TRUE)
}

\arguments{
  \item{es}{

}
  \item{features}{features = set of featurenames of the expression set, if left blank all are used

}
  \item{covar}{covar = used to color the points in the PCA

}
  \item{labelvar}{labelvar = used to label the points in the PCA

}
  \item{pcs}{pcs = used to state which principle components to plot

}
  \item{scale}{

}
  \item{center}{

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
# Requires package RGL
#esPcaplot3d(eset, covar="covariate_of_interest")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
