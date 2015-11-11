esPCA <-
function(es, features = '')   {
  ##########################
  ## Function 22
  ##   generates a dataframe of principle components and addes it to es
  ##   returns es
  ############################
  
  if(length(features)==1) {features = featureNames(es)}
  es = es[features,]
  target.PC = prcomp(t(exprs(es)), scale=TRUE, center=TRUE)
  target.scores = predict(target.PC)
  pheno = cbind(pData(es), sample = rownames(pData(es)))
  phenoPCA = merge(target.scores,pheno,by.x = 0,by.y = 0)
  rownames(phenoPCA) = phenoPCA[[1]]
  phenoPCA[1] = NULL
  phenoData <- new("AnnotatedDataFrame", data=phenoPCA)
  es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
}
