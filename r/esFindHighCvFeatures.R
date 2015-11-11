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
