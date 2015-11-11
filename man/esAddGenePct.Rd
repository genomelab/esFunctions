\name{esAddGenePct}
\alias{esAddGenePct}
\title{esAddGenePct}
\description{
Function to generate median, tertile, and quartiles of all the features in the expression set and adds it to the pheno data of the expressionset limiting it to 20 genes.
  
}
\usage{
esAddGenePct(es, variable = FALSE, pctlevel = 50)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set

}
  \item{variable}{custom selection of percentiles. If variable is set to FALSE, which is default value, it will generate quartiles, medians, and tertiles
}
  \item{pctlevel}{It is used to find the selected percentile above or below a certain threshold. Default is 50.
  
  
  
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
#pct <- esAddGenePct(eset, variable=FALSE, pctlevel=50)
#varLabels(pct)

}
