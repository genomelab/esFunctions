\name{esNMF}
\alias{esNMF}
\title{esNMF}
\description{
NMF for expressionset, returns a covariate vector. Non-Negative Matrix Factorization (NMF) factorize a matrix V into two non-negative matrices WH.

}
\usage{
esNMF(es, k = 2, run = 100, nmfmethod = "brunet")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{expression set with all positive values

}
  \item{k}{is the rank, which is a specification of a single numeric value. The the maximum linear independent columns of matrix A. You can determine how many factorization you want.

}
  \item{run}{number of runs to perform

}
  \item{nmfmethod}{Default method "brunet"

}
}

\references{
"An Introduction to Bioconductor's ExpressionSet Class" \cr
Seth Falcon, Martin Morgan, and Robert Gentleman \cr
6 October, 2006; revised 9 February, 2007 \cr

\url{cran.r-project.org/web/packages/NMF/NMF.pdf}
}
\author{
Dr. Shahab Asgharzadeh \cr
Janahan Gnanachandran \cr
Anthony Colombo \cr
Alexander Magsarili \cr

}
\note{
Load package NMF.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
#NMF <- esNMF(Annoset, k=2, run = 100, nmfmethod = "brunet")
#NMF 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
