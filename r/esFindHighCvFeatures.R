#' esFindHighCvFeatures
#' 
#' Find high cv genes within the dataset from each biological subgroup from es
#' data
#' 
#' 
#' @param es expression set
#' @param covar covariate
#' @param extracovargroup extra covariate group. Default to \code{NULL} . If
#' not \code{NULL}, a test eset is created from extracting the covar in the
#' extracovar group and take those matches of covariates that belong to the es
#' that is inputed. This will be the test eset that will become filtered.
#' @param cvlimit covariate limit
#' @param exprlimit expression limit
#' @param pct percent of samples. Used to calculate the percent of the data you
#' want to filter.
#' @param minsample minimum sample. Absolute minimum samples that you want to
#' filtered if the percent of the sample will be the minimum of product of
#' \code{pct} and the length of your feature names or the minimum sample
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #test <- esFindHighCvFeatures(eset, covar="features_of_interst", #extracovargroup = NULL, cvlimit = 0.1, exprlimit = "", pct = 0, minsample = 10)
#' #test
#' 
#' 
esFindHighCvFeatures <-
function(es, covar, extracovargroup=NULL, cvlimit=.1, exprlimit = '', pct = .1, minsample=10 ) {
  ## Find high cv genes within the dataset from each biological subgroup from es data
  cvlist = list()
  if (exprlimit == '') exprlimit = mean(exprs(es))
  for(selcovar in levels(droplevels(factor(es[[covar]])))) {
    testes = es[,which(es[[covar]] == selcovar)]    
    cvlist[[selcovar]] = featureNames(esFilter(testes, 
                                               cvlimit = cvlimit, 
                                               exprlimit = exprlimit, 
                                               pctsample= min(pct*length(sampleNames(testes)), 
                                                              minsample)/length(sampleNames(testes))))
  }
  cvlist[[covar]] = featureNames(esFilter(es, 
                                          cvlimit = cvlimit, 
                                          exprlimit = exprlimit, 
                                          pctsample= min(pct*length(sampleNames(es)), 
                                                         minsample)/length(sampleNames(es))))
  if(!is.null(extracovargroup)){
    testes = es[,which(es[[covar]] %in% extracovargroup)] 
    cvlist[[paste(extracovargroup,collapse="_")]] = featureNames(esFilter(testes, 
                                                                          cvlimit = cvlimit, 
                                                                          exprlimit = exprlimit, 
                                                                          pctsample= min(pct*length(sampleNames(testes)), 
                                                                                         minsample)/length(sampleNames(testes))))
  }
  ## Combine all high cv genes among the biological subgroups
  cvfeatures = unique(unlist(cvlist))
}
