esAddPheno_OnevsOthers <-
function(es, covar) {
  ############################################
  ## Function to take a covariate and build several others  
  ##  that will generate multiple covariates with each covariate versus combine of others
  ##  es - expression set
  ##  covar - covar to be used
  ##The returned es will have x number of new columns (x being number of variables in covar)
  ##The labels will be combine_x1 (where x1 is name of the variable in covar)
  #############################################
  covar = gsub("\\s","", covar)
  if (length(covar) != 1) { covar <- droplevels(factor(covar))}  else {covar = droplevels(factor(es[[covar]]))}
  
  for ( j in 1: length(levels(covar))){
    R <- levels(covar)[j]
    L <- levels(covar)[levels(covar)!=R]
    
    var2 <- as.character(covar)
    for ( i in 1:length(covar)){
      if (covar[i] %in% L)  {(var2[i] = paste(L,collapse="")) }
      else {(var2[i] = R)}
    }
    var2 <- factor(var2)
    column_header = paste0("Combined", levels(var2)[[1]])
    es <- esAddPheno(es,var2, column_header)
  }
  es
  
}
