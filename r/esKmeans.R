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
