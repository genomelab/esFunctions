\name{esLoadTarget}
\alias{esLoadTarget}
\title{esLoadTarget}
\description{
Automatically load target microarray data and removes Y chromosome features. The function also loads an annotation matrix.
}
\usage{
esLoadTarget(type = NULL, load_target_subset = TRUE, excludeYprobes = TRUE)
}

\arguments{
  \item{type}{4 types: tsr (transcript), psr (probe script), avgaffy (average transcript base on affy notation), avgref (average transcript based on ref gene) 

}
  \item{load_target_subset}{Logical argument. \code{TRUE} or \code{FALSE}

}
  \item{excludeYprobes}{Logical argument.If \code{TRUE}, it will remove the cell lines. Default to \code{TRUE}. It will return to the expression set. It removes probeset that belongs to chromosome y

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
#esLoadTarget(type = NULL, load_target_subset = TRUE, excludeYprobes = TRUE)
}

