\name{esHeatmap}
\alias{esHeatmap}
\title{esHeapmap}
\description{
Takes the features of an expression set and create an annotated heat map where the rows are the features of the expression set and the columns are the covariate. 
}
\usage{
esHeatmap(es, covar, rownames = NULL, hmapcol = NULL, colorPalette = "", distfun = "euclidean", hclustfun = "average", scale = "none", featurelimit = 5000, cexRow = 0.05 + 1/log10(dim(exprs(es))[1]),...)
}

\arguments{
  \item{es}{expression set}
  \item{covar}{ccovariate}
  \item{rownames}{Default setting is NULL. Give every feature names. If it is not set to NULL, the function will label the row names given into the function  and match the feature names.   

}
  \item{hmapcol}{Colors based on the heat map. If is specificed as NULL, there is a default color setting.

}
  \item{colorPalette}{
colorPallette has to be a list of vectors, each with number of colors. By default it creates a recurring list of colors obtained from global kmPallette, Pallette1, Pallette2 covar should be a list of characters i.e. c('risk', 'efscens).
}
  \item{distfun}{Default parameter to complete linkage clustering using euclidean distance measure and you can control this by specifying it by different functions to hclustFun or disfun, for example Manhattan distance.  

}
  \item{hclustfun}{function used to compute the hierarchial clustering when Rowv or Colv are not dendrograms

}
  \item{scale}{where the data points are centered

}
  \item{featurelimit}{limits you can have

}
  \item{cexRow}{Row axis labeling feature. Specify the tick or text label with a numeric value

}
\item{...}{
This is a placeholder
}

}


\references{
"An Introduction to Bioconductor's ExpressionSet Class" \cr
Seth Falcon, Martin Morgan, and Robert Gentleman \cr
6 October, 2006; revised 9 February, 2007 \cr

http://rss.acs.unt.edu/Rdoc/library/Heatplus/html/heatmap_plus.html

http://nmf.r-forge.r-project.org/aheatmap.html



}
\author{
Dr. Shahab Asgharzadeh \cr
Janahan Gnanachandran \cr
Anthony Colombo \cr
Alexander Magsarili \cr
}
\note{
Package NMF must be installed first
}


\examples{

#esHeatmap(eset, covar="covariate_of_interest")
}
