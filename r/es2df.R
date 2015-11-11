es2df <-
function(es) {
  ##################################################################
  ## Function 4
  ##  es2df
  ##  es - expression set 
  ##    Converting Expressionset to a dataframe
  ##################################################################  
  exprs = t(as.data.frame(exprs(es)))
  pheno = pData(es)
  #dataf = merge(pheno,exprs,by.x = 0,by.y = 0)
  #rownames(dataf) = dataf[[1]]
  dataf = cbind(pheno, exprs)
  dataf$Row.names = rownames(dataf)
  dataf
}
