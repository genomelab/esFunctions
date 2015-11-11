\name{esFindgenecor}
\alias{esFindgenecor}
\title{esFindgenecor}
\description{
Finds nearest genes by variety of correlation types from a expressionset class
}
\usage{
esFindgenecor(es, features, numb_corr = 50, abscor = TRUE, corrtype = "spearman", annotate = TRUE,  Anno=annot)
}

\arguments{
  \item{es}{expression set

}
  \item{features}{gene_symbols or other features of interest

}
  \item{numb_corr}{number of correlations to find
}
  \item{abscor}{Use absolute correlation

}
  \item{corrtype}{correlation type - 'spearman', 'pearson'

}
  \item{annotate}{annotate the data (TRUE//FALSE)

}
  \item{Anno}{annotation to use

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
#results <- esFindgenecor(eset, features="features_of_interest", numb_corr = 50, abscor = TRUE, corrtype = "spearman", annotate = TRUE, Anno = annot)
#results


}
