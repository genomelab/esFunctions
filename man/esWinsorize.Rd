\name{esWinsorize}
\alias{esWinsorize}
\title{esWinsorize}
\description{
Winsorization creates a upper and lower bounds for reducing the influence of outliers, such that if the outlier past either bound, it is given a constant value at the bound, as opposed its real value as an outlier.  
}
\usage{
esWinsorize(es, subtractcovar = FALSE, log = FALSE, covar, base = "", intervals = c(0.1, 0.9))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set
%%     ~~Describe \code{es} here~~
}
  \item{subtractcovar}{If the base is specificed and the subtractcovar set to TRUE, then it will find the base of the selected covariate and it will apply the normalization of that covariate and then it will take the entire assay data or expression values and will scale the expression data to the reference base on a log scale.
}
  \item{log}{log is set to TRUE, it will take the data and set it equal 2 to the power of the expression set
}
  \item{covar}{covar is covariate of interest}
  \item{base}{base is used as a reference for scaling}
  \item{intervals}{winsorizing process caps the outliers to the intervals selected. Where .1 is 10th percentile value and .9 is 90th percentile value. Acts as a ceiling. 
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
#results <- esWinsorize(eset, subtractcovar = FALSE, log = FALSE, covar="group_of_interest", intervals = c(0.1, 0.9))
#results
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
