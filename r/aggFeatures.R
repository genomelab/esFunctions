#' aggFeatures
#' 
#' Series of functions to Average over gene symbols, obtained from package
#' pathRender
#' 
#' 
#' @param fvector this is a full vector that needs splitting
#' @param splitvar this is the covariate which needs splitting
#' @param aggfun an internal function in Biobase that is required dependency
#' @author Shahab Asgharzadeh
aggFeatures = function( fvector, splitvar, aggfun ) {
  ############################
  ## Series of functions to Average over gene symbols, obtained from package pathRender
  ###########################
  if (!is.vector(fvector)) stop("fvector must satisfy is.vector()")
  if (!(length(fvector) == length(splitvar))) stop("fvector and splitvar must have equal lengths")
  sapply(split(fvector, splitvar), aggfun)
}
