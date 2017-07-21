\name{esBoxplot}
\alias{esBoxplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{esBoxplot

}
\description{
Box plot of selected features of an expressionset class, can be categorized by a covariate 
}
\usage{
#esBoxplot(es, features = "", covar = "", annotation = annot, colPallette = colorPalette("kmPalette2"), jitter = FALSE, annotate = FALSE, drop_ends = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{espression set

}
  \item{features}{feature names of the eset
}
  \item{covar}{covariate of eset}
  \item{annotation}{Annotation data set that is used to convert the transcript ID into the gene names}
  \item{colPallette}{the name of the color pallette used}
  \item{jitter}{logical command that will create a jitter plot in ggplot2}
  \item{annotate}{logical command to include the annotation set}
  \item{drop_ends}{logical command to drop the transcript ID from the gene names}
  }
\references{
"An Introduction to Bioconductor's ExpressionSet Class"
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
#esBoxplot(es, features = "", covar = "", annotation = annot, colPallette = colorPalette("kmPalette2"), jitter = FALSE, annotate = FALSE, drop_ends = FALSE)
}



