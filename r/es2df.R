#' es2df
#' 
#' Converting Expressionset to a dataframe
#' 
#' 
#' @param es expression set
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #results <- es2df(eset)
#' #results
#' 
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
