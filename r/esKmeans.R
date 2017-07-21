#' esKmeans
#' 
#' esKmeans function finds the kmeans clusters of a expressionset and adds the
#' cluster info to the covariate file
#' 
#' 
#' @param es expression set
#' @param kmeans number of clusters assumed and returns an expressionset object
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #results<- esKmeans(eset, kmeans = 2)
#' #results 
#' 
esKmeans <-
function(es, kmeans=2) { 
  ############################################
  ## Function 8
  ## Function to find the kmeans clusters of a expressionset 
  ## and adds the cluster info to the covariate file
  ##  es = expressionset 
  ##  kmeans = number of clusters assumed
  ##  returns an expressionset object
  #############################################
  cl=(kmeans(t(exprs(es)),kmeans))
  df = as.data.frame(pData(es))
  df$cluster=factor(cl$cluster)
  phenoData <- new("AnnotatedDataFrame", data=df)
  es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
}
