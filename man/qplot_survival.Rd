\name{qplot_survival}
\alias{qplot_survival}
\title{qplot_survival}
\description{
Draw survival function, needs object survival package, second parameter if set to \code{TRUE} will display confidence intervals. Use different plotting commands dependig whether or not strata's are given.
}
\usage{
qplot_survival(f.frame, f.CI = "default", f.linesize = 1, f.shapesize = 2.5, f.shape = 3, kmcolors = kmPalette)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f.frame}{data frame

}
  \item{f.CI}{data frame of confidence interval. Either \code{default}, \code{TRUE}, or \code{FALSE}

}
  \item{f.linesize}{thickness setting which is set to 1 

}
  \item{f.shapesize}{used for the aesthetic setting argument for the "size" in ggplot 

}
  \item{f.shape}{used for the aesthetic setting argument for the "shape" in ggplot

}
  \item{kmcolors}{color palette

}
}

\author{
Dr. Shahab Asgharzadeh \cr
Janahan Gnanachandran \cr
Anthony Colombo \cr
Alexander Magsarili \cr

}
\note{
Requires package 'ggplot' and 'survival'.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
#qplot_survival(f.frame, f.CI = "default", f.linesize = 1, f.shapesize = 2.5, f.shape = 3, kmcolors = kmPalette)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the R documentation directory.

\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line