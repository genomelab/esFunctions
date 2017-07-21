\name{esFindHighCvFeatures}
\alias{esFindHighCvFeatures}
\title{esFindHighCvFeatures}
\description{
Find high cv genes within the dataset from each biological subgroup from es data
}
\usage{
esFindHighCvFeatures(es, covar, extracovargroup = NULL, cvlimit = 0.1, exprlimit = "", pct = 0.1, minsample = 10)
}

\arguments{
  \item{es}{expression set

}
  \item{covar}{covariate

}
  \item{extracovargroup}{extra covariate group. Default to \code{NULL} . If not \code{NULL}, a test eset is created from extracting the covar in the extracovar group and take those matches of covariates that belong to the es that is inputed. This will be the test eset that will become filtered.  

}
  \item{cvlimit}{covariate limit

}
  \item{exprlimit}{expression limit

}
  \item{pct}{percent of samples. Used to calculate the percent of the data you want to filter.

}
  \item{minsample}{minimum sample. Absolute minimum samples that you want to filtered if the percent of the sample will be the minimum of product of \code{pct} and the length of your feature names or the minimum sample

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
#test <- esFindHighCvFeatures(eset, covar="features_of_interst", #extracovargroup = NULL, cvlimit = 0.1, exprlimit = "", pct = 0, minsample = 10)
#test

}
