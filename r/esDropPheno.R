esDropPheno <-
function(es, column_number) {
  ############################################
  ## Function to drop a column of the phenoData of expressionset
  #############################################
  
  df = as.data.frame(pData(es))
  phenoData <- new("AnnotatedDataFrame", data=df[,-column_number])
  es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
}
