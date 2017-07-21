\name{esPathwayTop}
\alias{esPathwayTop}
\title{esPathwayTop}
\description{
Finds Top scoring Pathways (using Score Calculated by multiplying pathway eucledian distance and SD of Chi2.
}
\usage{
esPathwayTop(esPathwaySig_results, covarcompare, headerprefix = "ecu_pw_chi_sd_", fdrlevel = 0.05)
}

\arguments{
  \item{esPathwaySig_results}{pathway analysis results from \code{esPathwaySig}

}
  \item{covarcompare}{covariate comparison of interest

}
  \item{headerprefix}{columns of interest to be chosen from pathway results

}
  \item{fdrlevel}{level of false discovery rate to be considered

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

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
#esPathwayTop(esPathwaySig_results, covarcompare, headerprefix = "ecu_pw_chi_sd_", fdrlevel = 0.05)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
