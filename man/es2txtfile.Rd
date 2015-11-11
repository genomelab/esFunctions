\name{es2txtfile}
\alias{es2txtfile}
\title{es2txtfile}
\description{
Converts Expression set data to .txt format that can be used to upload to MySQL. Creates the Class label file (.cls) using the covar variable provided. The covar label must be in the phenotype data of es.
}
\usage{
es2txtfile(es, exprfilename, covarfilename)
}

\arguments{
  \item{es}{expression set

}
  \item{exprfilename}{expression set file name that you will write to a table, but only the expression assay data. Requires (\code{.txt})

}
  \item{covarfilename}{covariate file name that requires (\code{.cls})

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
#text_file <- es2txtfile(eset, "smallassay.txt", "smallpheno.cls")
#text_file
# Writes to current working directory 
#getwd()
# Files will be on that location 
}
