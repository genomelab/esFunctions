\name{esPCA}
\alias{esPCA}
\title{Principal Component Analysis}
\description{Principal Component Analysis uses eigenvectors and eigenvalues to create a linear subspace that span the data points along the direction of maximum variance.  Usually the first three principal components are considered only. 
}
\usage{
esPCA(es, features = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{es is the Expression Set that is coupled with the covariate matrix.  }
  \item{features}{The features are the gene names or transcript ID's , or the rows of the assay data matrix used in the Expression set.  For best use, features is usually left blank to evaluate all of them.}
}
\details{It column binds the rows of the assay data matrix of the expression set , into a new expression set.  It is usually helpful to then plot the PC1 and PC2 on a new plane for analysis.  

}
\value{There are no values returned in this function, only a new expression set with principal components binded to the columns
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
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
\note{In the below example, we leave out features so that all the rows are evaluated.  To customize selected features enter the feature names under the second argument 'features'.  Note: for the second example below, there are three feature names arbitrarily selected.  The output for neweset2 will show only three principal components that are related only to these customized features that were selected. 

}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# See the references for creating an expression set.  Assuming one is made, adding the principal components is straight forward.
#neweset<- esPCA(oldeset)
#View(neweset) 

# Second Example with Customized Features used in PCA
#neweset2<-esPCA(oldeset, c('featurename1', 'featurename2', 'featurename3'))
#View(neweset2)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 
}
