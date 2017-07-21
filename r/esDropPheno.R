#' esDropPheno
#' 
#' Function to drop a column of the phenoData of expressionset
#' 
#' 
#' @param es expression set
#' @param column_number specify a column number and removes it from the
#' dataframe
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' # 5 is column number you want to delete
#' #neweset <- esDropPheno(eset, 5)
#' #neweset
#' 
#' 
esDropPheno <-
function(es, column_number) {
  ############################################
  ## Function to drop a column of the phenoData of expressionset
  #############################################
  
  df = as.data.frame(pData(es))
  phenoData <- new("AnnotatedDataFrame", data=df[,-column_number])
  es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
}
