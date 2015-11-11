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
