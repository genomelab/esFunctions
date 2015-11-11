esWinsorize <-
function(es, subtractcovar = FALSE, log=FALSE, covar, base='', intervals=c(.1,.9)) {
  if (subtractcovar) {
    selcovar <- droplevels(factor(es[[covar]]))
    #Set the level to be analyzed to the users choice otherwise it is the first varaible in the levels
    selcovar <- relevel(selcovar, ref=base)
    
    es = es[, order(-as.numeric(selcovar))]
    es_base = exprs(es[, which(selcovar==base)]) 
    
    row.base <- apply(es_base, 1, mean)
    exprs(es) = 2^(exprs(es) - row.base)
  }
  
  if(log)  data = 2^exprs(es) else data=exprs(es)
  upper = quantile(data, intervals[2])
  lower = quantile(data, intervals[1])
  #winsbyrow = function(x) {
  #  pmax(pmin(x, quantile(x, .9)), quantile(x, .1))
  #}
  
  winsoverall = function(x) {
    pmax(pmin(x, upper), lower)
  }
  data = apply(data, 2, winsoverall)
  exprs(es) = data
  es
}
