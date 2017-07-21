\name{esKM2}
\alias{esKM2}
\title{esKM2}
\description{
Uses SurvPlot function that gives out info about number of patient censored at each time.}
\usage{
esKM2(es, kmtime, kmcens, covar = "", n.risk = TRUE, levels.only = TRUE, title = "", xtitle = "Time", ytitle = "Survival Probability", outputdir, filename, save = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set}
  \item{kmtime}{survival time (assumed to be in years, if not convert it to be in years)}
  \item{kmcens}{censor indictor (0 alive, 1 for dead)}
  \item{covar}{covariate}
  \item{n.risk}{is a logical command. If it is set to \code{TRUE}, then will add the number of risk from \code{risk.summary} and the numbers are placed in the bottom.

}
  \item{levels.only}{Set to \code{TRUE} to remove the variable names

}
  \item{title}{title of the analysis

}
  \item{xtitle}{title of the x-axis

}
  \item{ytitle}{title of the y-axis

}
  \item{outputdir}{outputdir

}
  \item{filename}{filename  (use if save is set to \code{TRUE}, default is \code{FALSE})}
  \item{save}{use if save is set to \code{TRUE}, default is \code{FALSE}

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
Requires package 'SurvPlot'
}


\examples{
#esKM2(eset, kmtime, kmcens, covar = "covariates_of_interest", n.risk = TRUE, levels.only = TRUE, title = "esKM 2", xtitle = "Time", ytitle = "Survival Probability", outputdir, filename, save = FALSE)
}
