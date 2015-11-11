aggFeatures = function( fvector, splitvar, aggfun ) {
  ############################
  ## Series of functions to Average over gene symbols, obtained from package pathRender
  ###########################
  if (!is.vector(fvector)) stop("fvector must satisfy is.vector()")
  if (!(length(fvector) == length(splitvar))) stop("fvector and splitvar must have equal lengths")
  sapply(split(fvector, splitvar), aggfun)
}
