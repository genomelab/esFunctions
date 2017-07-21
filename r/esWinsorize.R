#' esWinsorize
#' 
#' Winsorization creates a upper and lower bounds for reducing the influence of
#' outliers, such that if the outlier past either bound, it is given a constant
#' value at the bound, as opposed its real value as an outlier.
#' 
#' 
#' @param es expression set %% ~~Describe \code{es} here~~
#' @param subtractcovar If the base is specificed and the subtractcovar set to
#' TRUE, then it will find the base of the selected covariate and it will apply
#' the normalization of that covariate and then it will take the entire assay
#' data or expression values and will scale the expression data to the
#' reference base on a log scale.
#' @param log log is set to TRUE, it will take the data and set it equal 2 to
#' the power of the expression set
#' @param covar covar is covariate of interest
#' @param base base is used as a reference for scaling
#' @param intervals winsorizing process caps the outliers to the intervals
#' selected. Where .1 is 10th percentile value and .9 is 90th percentile value.
#' Acts as a ceiling.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #results <- esWinsorize(eset, subtractcovar = FALSE, log = FALSE, covar="group_of_interest", intervals = c(0.1, 0.9))
#' #results
#' 
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
