#' esAnova
#' 
#' Uses esApply function on the expressionset class. Currently implemented for
#' one factor model with 3 groups. Returns a table with x2, pvalues, mean & SE
#' of each group, x2, fold change (fc) of each pairwise analyses. Also returns
#' the score (fc * x2) and absolute of the score, it adds the rownames of the
#' expression matrix of the expressionset to the returned table.
#' 
#' 
#' @param es expression set object
#' @param covar is a variable to include in the ANOVA model y ~ covar
#' @param base indicates which of the covar levels should be used as the base
#' level for ANOVA - default is the first one. When calling the function, you
#' don't need to include the base
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' ##Assuming an Expression set is created from the references, the function call is straight foward. ##\cr
#' #results <- esAnova(eset, covar = eset$group_of_interest)
#' #results
#' 
#' 
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
