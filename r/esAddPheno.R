#' esAddPheno
#' 
#' To add a column to the phenoData of expressionset
#' 
#' For xcolmatch, if xcolmatch equal to 0, that value will pass to the merge
#' command that is used by the esAddPheno function.This value xcolmatch is
#' passed into the by.x parameter used in merge. If colmatch not equal to 0, it
#' selects the column names of the data frame of the phenotypic data or
#' covariates and then passes the matched column names of the phenodata into
#' the parameter by.x.
#' 
#' For ycolmatch, if ycolmatch is 0, that length the value will pass into the
#' parameter by.y used in the merge command. If is not 0, it will take the
#' column names of c vector that equal ycolmatch and that will be used in the
#' by. parameter.
#' 
#' @export
#' @param es expression set
#' @param cvector a list of vector
#' @param colname label of the variable to be put in the pData of
#' expressionset, if non given,the name of the variable is used.
#' @param match if match = TRUE, then the dataframe is added, the rownames of
#' the data.frame will be matched against phenodata of the es
#' @param xcolmatch find where in the covariate is selected, that will be
#' extracted and joined together with the c value input.
#' @param ycolmatch length of a vector to be match with a c vector to a pdata
#' of the expression set.
#' 
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
esAddPheno <-
function(es, cvector, colname = '', match=FALSE, xcolmatch=0, ycolmatch=0) {
  ############################################
  ## Function to add a column to the phenoData of expressionset
  ##  es - expression set
  ##  cvector - a list or vector
  ##  colname - label of the variable to be put in the pData of expressionset, if non given, 
  ##            the name of the variable is used.
  #############################################
  if (match) {
    ## if match = TRUE, then the dataframe is added,
    ## the rownames of the data.frame will be matched against 
    ## phenodata of the es
    ## Column name change is only permitted if you are adding a column with match=False, 
    ## With match = TRUE, the column names of the dataframe being added and matched will be used for column names
    df = as.data.frame(pData(es))
    df$orig_rownames = rownames(df)
    df$orig_order = order(rownames(df))
    if (xcolmatch==0 & ycolmatch==0) {
      if(length(grep('Row.names', x=df))==0) df$Row.names = NULL
      df = merge(df,cvector,by.x = 0,by.y = 0, all.x=TRUE)
      df = df[order(df$orig_order),]
      rownames(df) = df$orig_rownames
      df$orig_rownames = NULL
      df$orig_order = NULL
      
      
    } else if(xcolmatch!=0 | ycolmatch!=0) {
      #rownames(df) = df[[grep(colmatch, colnames(df))]]  
      if (xcolmatch==0) xmatch = "orig_rownames" else xmatch = colnames(df)[colnames(df)==xcolmatch]
      if (ycolmatch==0) {cvector$orig_rownames = rownames(cvector); ymatch="orig_rownames"} else ymatch = colnames(cvector)[colnames(cvector)==ycolmatch]
      
      df = merge(df, cvector, by.x= xmatch,by.y=ymatch, all.x=TRUE)
      df = df[order(df$orig_order),]
      rownames(df) = df$orig_rownames
      df$orig_rownames = NULL
      df$orig_order = NULL
      
    }
  }
  else
  {
    df = as.data.frame(pData(es))
    name <- deparse(substitute(cvector)) 
    if (colname=='') {df[[name]] = cvector}
    else {df[[colname]] = cvector }
    
    
  }
  if(length(grep('Row.names', x=df))==0) df$Row.names = NULL
  phenoData = new("AnnotatedDataFrame", data=df)
  es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
}
