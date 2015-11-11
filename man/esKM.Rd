\name{esKM}
\alias{esKM}
\title{esKM}
\description{Wrapper for qplot_survival function
  
}
\usage{
esKM(es, kmtime, kmcens, covar = "", title = "", xtitle = "Time", ytitle = "Survival", kmcolors = kmPalette, outputdir, filename, simplestat = FALSE, save = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set}
  \item{kmtime}{survival time (assumed to be in years, if not convert it to be in years)}
  \item{kmcens}{censor indictor (0 alive, 1 for dead)}
  \item{covar}{covariate}
  \item{title}{title of the analysis

}
  \item{xtitle}{title of the x-axis

}
  \item{ytitle}{title of the y-axis

}
  \item{kmcolors}{color palette

}
  \item{outputdir}{outputdir}
  \item{filename}{filename (use if save is set to \code{TRUE}, default is \code{FALSE}}
  \item{simplestat}{If \code{TRUE}, it will write the statistics to display the text on the plot

}
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
Requires package 'ggplot' and 'serve'. Requires function 'surv', 'theme_set', and 'ggplot'.

}


\examples{
#esKM(eset, kmtime, kmcens, covar = "covariate_of_interest", title = "KM Plot", xtitle = "Time", ytitle = "Survival", kmcolors = kmPalette, outputdir, filename, simplestat = FALSE, save = FALSE)
}
