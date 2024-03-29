% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esCorrGene.R
\name{esCorrGene}
\alias{esCorrGene}
\title{esCorrGene}
\usage{
esCorrGene(es, features, numb_corr = 50, abscor = TRUE,
  corrtype = "spearman", annotate = FALSE, anno = annot)
}
\arguments{
\item{es}{expression set}

\item{features}{gene_symbols or other features of interest}

\item{numb_corr}{number of correlations to find}

\item{abscor}{Use absolute correlation}

\item{corrtype}{correlation type - 'spearman', 'pearson'}

\item{annotate}{annotate the data (TRUE//FALSE)}

\item{Anno}{annotation to use}
}
\description{
Finds nearest genes by variety of correlation types from a expressionset
class
}
\examples{

#results <- esCorrGene(eset, features="features_of_interest", numb_corr = 50, abscor = TRUE, corrtype = "spearman", annotate = TRUE, Anno = annot)
#results



}
\references{
"An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
February, 2007 \cr
}
\author{
Shahab Asgharzadeh
}
