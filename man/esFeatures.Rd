\name{esFeatures}
\alias{esFeatures}
\title{esFeatures}
\description{
Function that generates information on the features selected
}
\usage{
esFeatures(es, features = "", column_lookup = "gene_symbol", anno=annot)
}

\arguments{
  \item{es}{expression set}
  \item{features}{typically genes or anything found in the column of annotation needed
  }
  \item{column_lookup}{typically gene_symbol but can be anything in the annotation file. Returns list with multiple information. The first vector is the featureNames of the es matching features. The second vector is the features and featureNames pasted to gether with the underscore}
  \item{anno}{this is a the annotation data set}
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
# annot input is dataset containing experiemental data: probe set id, reference gene symbols, ...
#esFeatures(eset, features = "feature_of_interest", column_lookup = "gene_name", annot)
}
