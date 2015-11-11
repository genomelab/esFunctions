\name{esDE}
\alias{esDE}
\title{esDE}
\description{
Uses esApply function on the expressionset class. Currently implemented for one factor model with 3 groups. Returns a table with x2, pvalues, mean & SE of each group, x2, fold change (fc) of each pairwise analyses. Also returns the score (fc * x2) and absolute of the score, it adds the rownames of the expression matrix of the expressionset to the returned table. Generates comparison between 2 groups only.
}
\usage{
esDE(es, covar, base = levels(covar)[1])
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set object}
  \item{covar}{is a variable to include in the ANOVA model y ~ covar}
  \item{base}{indicates which of the covar levels should be used as the base level for ANOVA - default is the first one. When calling the function, you don't need to include the base}
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
# The function only generate comparison between 2 groups 
#results <- esAnova(eset, covar = eset$group_of_interest)
#results 
}
