#' es2PAMR
#' 
#' Sets up a list to be used for PAMR
#' 
#' @export
#' @param es expression set
#' @param covar1 1st covariate
#' @param covar2 2nd covariate
#' @param survival If survival = TRUE, then covar1 is the survival time and
#' covar2 is the censor data
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #PAMR <- es2PAMR(eset, covariate1, covariate2 = NULL, survival = FALSE)
#' #PAMR
#' 
es2PAMR <-
function(es, covar1, covar2=NULL, survival=FALSE) {
  #### Sets up a list to be used for PAMR
  #### If survival = TRUE, then covar1 is the survival time
  #### and covar2 is the censor data
  covar = covar1
  data = list(x=exprs(es), y=es[[covar]], genenames=featureNames(es), geneid = featureNames(es))
  
  if (survival) {
    #Setting Data for PAMR binary analysis for efs both using survival time analysis of PAM
    covartime = covar1
    covarcens = covar2
    data = list(x=exprs(es), survival.time=es[[covar1]], censoring.status=es[[covar2]], genenames=featureNames(es), geneid = featureNames(es))
  }
  data
}
