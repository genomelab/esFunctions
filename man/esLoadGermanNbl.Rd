\name{esLoadGermanNbl}
\alias{esLoadGermanNbl}
\title{esLoadGermanNbl}
\description{
Load Function and automatically load target data. Then, removes Y chromosome features.

}
\usage{
esLoadGermanNbl(type = NULL, load_target_subset = TRUE, excludeYprobes = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{type}{4 types: tsr (transcript), psr (probe script), avgaffy (average transcript base on affy notation), avgref (average transcript based on ref gene) 

}
  \item{load_target_subset}{takes a different annotation r-object dataset

}
  \item{excludeYprobes}{Logical argument.If \code{TRUE}, it will remove the cell lines. Default to \code{TRUE}. It will return to the expression set.It removes probeset that belongs to chromosome y. 

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
#esLoadGermanNbl(type = NULL, load_target_subset = TRUE, excludeYprobes = TRUE)
}
