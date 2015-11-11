\name{esHcplot_target}
\alias{esHcplot_target}
\title{Heirarchical Clustering}
\description{
Heirarchical Clustering using data that are Windsorized. Generates HC and map of HC and can have columns representing phenotypes on top row, specifically written for TARGET data.
}
\usage{
esHcplot_target(es, features, selcovar, winsor = TRUE, title = "", scale = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set}
  \item{features}{the rows of the expression set}
  \item{selcovar}{select covariate group of interest that you want label on the cluster}
  \item{winsor}{winsorize set to TRUE. It will fix the outliers.}
  \item{title}{Main title of the plot}
  \item{scale}{If scale is said to be true, then we unlog the values and let heatmap standardize the values and need to pick large number of color levels.}
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
#esHcplot_target(eset, features= c("feature1"), selcovar="group_of_interest", winsor = TRUE, title = "Hc Plot", scale = FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
