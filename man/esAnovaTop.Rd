\name{esAnovaTop}
\alias{esAnovaTop}
\title{esAnovaTop}
\description{
An arrange of inputs. It will take the best results from the argument that is given through sortby. 
}
\usage{
esAnovaTop(es, anovaresults, range, sortby = "abs_score", equal = FALSE)
}

\arguments{
  \item{es}{expression set

}
  \item{anovaresults}{dataframe from esanova

}
  \item{range}{Amount of the top results you want

}
  \item{sortby}{column name you want from esanova

}
  \item{equal}{Include values that are equal 

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
#results <- esAnovaTop(eset, anova_table, range = c(1:some_number), #sortby = "anova_column_interest", equal = FALSE)
}
