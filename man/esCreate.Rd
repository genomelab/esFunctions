% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esCreate.R
\name{esCreate}
\alias{esCreate}
\title{esCreate}
\usage{
esCreate(aptfile, covfile, match = FALSE, match_covcol = "rownames",
  annot = "optional", annotate = FALSE)
}
\arguments{
\item{aptfile}{Gene expression matrix as a dataframe}

\item{covfile}{Covariate file describes the sample information}

\item{match}{Match is set to false and check if the column names of gene
expression matrix match row names of the covariate file. If match is TRUE,
it will not check.}

\item{match_covcol}{By default, the function looks for the matches of the
column assay to the rows of the covariate dataframe. But match covariate
column, specify whether you want to match the column of the expression
matrix to a column of the covariate dataframe (instead of matching a row in
a covariate dataframe).}

\item{annot}{Annot is a annotation dataframe that has transcript_ids,
probeset_ids, reference gene symbols, etc...}

\item{annotate}{This argument is like a flag that if it is specified, it
will call esAnnotate function with the annot as the argument}
}
\description{
Create an ExpressionSet Class from gene expression matrix and covariate
file. \cr Column names of gene expression matrix must match the row names of
covariate file.
}
\examples{

#neweset<-esCreate(assaydata, covariateframe, match = FALSE)
#View(neweset)

}
\references{
"An Introduction to Bioconductor's ExpressionSet Class" Seth
Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
February, 2007 \cr
}
\author{
Shahab Asgharzadeh
}
