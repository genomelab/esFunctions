\name{esDropPheno}
\alias{esDropPheno}
\title{esDropPheno}
\description{
Function to drop a column of the phenoData of expressionset
}
\usage{
esDropPheno(es, column_number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set

}
  \item{column_number}{specify a column number and removes it from the dataframe}
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
# 5 is column number you want to delete
#neweset <- esDropPheno(eset, 5)
#neweset

}
