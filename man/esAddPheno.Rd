\name{esAddPheno}
\alias{esAddPheno}
\title{esAddPheno}
\description{
To add a column to the phenoData of expressionset}
\usage{
esAddPheno(es, cvector, colname = "", match = FALSE, xcolmatch = 0, ycolmatch = 0)
}
\arguments{
  \item{es}{expression set
}
  \item{cvector}{a list of vector
}
  \item{colname}{label of the variable to be put in the pData of expressionset, if non given,the name of the variable is used.
}
  \item{match}{
  if match = TRUE, then the dataframe is added, the rownames of the data.frame will be matched against phenodata of the es
}
  \item{xcolmatch}{find where in the covariate is selected, that will be extracted and joined together with the c value input.

}
  \item{ycolmatch}{length of a vector to be match with a c vector to a pdata of the expression set. 
    
  
  }
}
\details{
For xcolmatch, if xcolmatch equal to 0, that value will pass to the merge command that is used by the esAddPheno function.This value xcolmatch is passed into the by.x parameter used in merge. If colmatch not equal to 0, it selects the column names of the data frame of the phenotypic data or covariates and then passes the matched column names of the phenodata into the parameter by.x.

For ycolmatch, if ycolmatch is 0, that length the value will pass into the parameter by.y used in the merge command. If is not 0, it will take the column names of c vector that equal ycolmatch and that will be used in the by. parameter. 


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
