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
