#' es2esCor
#' 
#' Calculates correlation between two es data (have to be the same size and
#' obviously same rownames).
#' 
#' 
#' @param es1 first expression set
#' @param es2 second expression set
#' @param features_es1 select features
#' @param corrtype Options for this argument: pearson, spearman, kendall.
#' Recommended spearman.
#' @param plot Logical command where if TRUE, includes a plot
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #results_cor_obj <- es2esCor(eset1, eset2, features_es1 = NULL, corrtype = "spearman", plot = TRUE)
#' #results_cor_obj
#' 
#' 
es2esCor <-
function(es1, es2, features_es1=NULL, corrtype="spearman", plot=TRUE) 
  ### Calculates correlation between two es data (have to be the same size and obviously same rownames)
{
  if (is.null(features_es1)) 
    features = featureNames(es1) else features = featureNames(es1[features_es1,])
  es1 = es1[features,]
  es2 = es2[features,]
  result = diag(cor(t(exprs(es1)), t(exprs(es2)),method=corrtype))
  if (plot) {
    print(
      ggplot(data=data.frame(result), aes(x=result)) + geom_density(alpha = 0.2)
    )
    print (
      ggplot(data=data.frame(result), aes(x=result)) + geom_histogram(binwidth = 0.01)
    )
  }
  result
  
  #Example of overlaying two correlation data and generating histogram
  #highcvcorrelation = rbind(u133huex_corr_data, u133huex_corr_highcv_data)
  #ggplot(highcvcorrelation, aes(U133_HuEx_Correlation, fill = cv)) + geom_density(alpha = 0.3)
  #ggplot(highcvcorrelation, aes(U133_HuEx_Correlation, fill = cv)) + geom_histogram(binwidth=0.025, color ="black", alpha=0.2)
}
