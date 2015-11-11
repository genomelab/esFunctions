\name{esHcplot}
\alias{esHcplot}
\title{esHcplot}
\description{
Generic Heirarchical Clustering using data that are Windsorized. Need to set winsor levels in the function(levels set at 10 percent and 90 percent). 
}
\usage{
esHcplot(es, features, selcovar, base, winsor = TRUE, title = "", scale = FALSE)
}

\arguments{
  \item{es}{expression set}
  \item{features}{list of features of expression set

}
  \item{selcovar}{selcovar are groups that will be used to determine how to calculate the fold change}
  \item{base}{base is the mean level of features from this selcovar group will be used as base of comparison for all samples.}
  \item{winsor}{
If using Winsor (default), color levels need to be low and don't need to scale values (normalize with SD 1)
}
  \item{title}{title of the plot
}
  \item{scale}{If you do not winsorize then set scale to True to get regular heatmap. If scale is said to be true, then we unlog the values and let heatmap standardize the values and need to pick large number of color levels.}
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
Requires package 'ggplot2'.
}

\examples{
#esHcplot(eset, features = "features_of_interest", selcovar = "covariates_of_interest", base, winsor = TRUE, title = "Expression Set Heirarchical Clustering Plot ", scale = FALSE)
}
