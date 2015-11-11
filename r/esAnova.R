esAnova <-
function(es, covar, base=levels(covar)[1]) {
  ############################################
  ##  Function 2
  ##  anova_analysis,  function to call glm, 
  ##  es - expressionset object
  ##  covar - variable to include in the ANOVA model y ~ covar
  ##  base - whic of the covar levels should be used as the base level for ANOVA - default is the first one
  ##
  ##  Uses esApply function on the expressionset class
  ##  Currently implemented for one factor model with 3 groups  (based on Lingyun's code)
  ##  Returns a table with x2, pvalues, mean & SE of each group, x2, fold change (fc) of each pairwise analyses
  ##  also returns the score (fc * x2) and absolute of the score, it adds the rownames of the expression matrix of the
  ##  expressionset to the returned table.
  ###########################################
  
  #Ensure that no extra factor levels are in the covar
  covar = gsub("\\s","", covar)
  if (typeof(covar) == 'integer') { covar <- droplevels(covar)}  else {covar = factor(covar)}
  #Set the level to be analyzed to the users choice otherwise it is the first varaible in the levels
  covar <- relevel(covar, ref=base)
  #Call to function glmfunction for each row of the data
  results = as.data.frame(t(esApply(es, 1, glmfunction, var1 = covar)))
  results = cbind("id" = rownames(exprs(es)), results)
}
