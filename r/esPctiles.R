esPctiles <-
function(eset) {
  #########################
  ## esPctiles
  ## Returns an expressionset class where the values have been converted to percentiles 
  ## i.e. Calculate empirical continous desnity function and create a expression set based on that
  ###########################
  
  pctiles = data.frame(sapply(as.data.frame((exprs(eset))), function(x) ecdf(x)(x)))
  rownames(pctiles) = featureNames(eset)
  eset_pct = eset
  exprs(eset_pct) = as.matrix(pctiles)
  eset_pct
}
