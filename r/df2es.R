df2es <-
function(es) 
  ### Converts a dataframe to an expressionset.  
  ### Assumptions, rows have the covariate inforamtion in non-numeric values, these rows are converted to covariate part
  {
  
  df2es = sapply(es, function(x) as.numeric(as.character(x)))   # rudimentary making anything not numeric into NA
  rows<-rownames(df2es) 
  df2es_dim = dim(df2es)
  df2es_size = length(df2es)
  df2es_data = df2es[complete.cases(df2es),]
  rownames(df2es_data) = rows[complete.cases(df2es)]   ## need the rownames so the as.matrix function can do its thing
  df2es_data = as.matrix(df2es_data)
  
  df2es_cov = as.data.frame(t(es[!complete.cases(df2es),]))
  
  esCreate(df2es_data, df2es_cov)
  
}
