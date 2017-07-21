\name{esPcaplot}
\alias{esPcaplot}
\title{esPcaplot}
\description{
esPCA plot of selected features of an expressionset class

}
\usage{
esPcaplot(es, features = "", covar = "", labelvar = "", pcs = c(1, 2), shape = FALSE, kmcolors = colorPalette("kmPalette"), scale = TRUE, center = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set

}
  \item{features}{features = set of featurenames of the expression set, if left blank all are used

}
  \item{covar}{used to color the points in the PCA

}
  \item{labelvar}{used to label the points in the PCA

}
  \item{pcs}{used to state which principle components to plot

}
  \item{shape}{TRUE/FALSE. TRUE if you want to add shapes.

}
  \item{kmcolors}{Loads the color palette.  

}
  \item{scale}{Logical command that scales the data. TRUE for scale.

}
  \item{center}{Logical command that centers the data. TRUE for center.

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
For argument kmcolors, one can create their own color palette called kmPalette <- c("red", "blue", "darkgreen", "black", "orange, "yellow4", "brown", "purple", "grey", "turquoise"). The amount of colors must match the number of levels.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
# kmcolor, look at the note
#esPcaplot(eset, covar="group_of_interest", shape = FALSE, kmcolors = colorPalette("kmPalette"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
