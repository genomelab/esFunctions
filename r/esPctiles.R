#' esPctiles
#' 
#' Returns an expressionset class where the values have been converted to
#' percentiles i.e. Calculate empirical continous desnity function and create a
#' expression set based on that.
#' 
#' 
#' @param eset expression set
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #ecdf_on_assay_data <- esPctiles(eset)
#' #ecdf_on_assay_data
#' 
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
