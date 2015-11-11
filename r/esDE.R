esDE <-
function(es, covar, base=levels(covar)[1]) {
  ############################################
  ##  Function 2
  ##  DE_analysis,  function to call glm, 
  ##  es - expressionset object
  ##  covar - variable to include in the ANOVA model y ~ covar
  ##  base - which of the covar levels should be used as the base level for ANOVA - default is the first one
  ###########################################
  
  #Ensure that no extra factor levels are in the covar
  covar = gsub("\\s","", covar)
  if (typeof(covar) == 'integer') { covar <- droplevels(covar)}  else {covar = factor(covar)}
  #Set the level to be analyzed to the users choice otherwise it is the first varaible in the levels
  covar <- relevel(covar, ref=base)
  #Call to function glmfunction for each row of the data
  results = as.data.frame(t(esApply(es, 1, DEfunction, var1 = covar)))
  results = cbind("id" = rownames(exprs(es)), results)
}
