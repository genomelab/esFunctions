#' esAddGenePct
#' 
#' Function to generate median, tertile, and quartiles of all the features in
#' the expression set and adds it to the pheno data of the expressionset
#' limiting it to 20 genes.
#' 
#' @export
#' @param es expression set
#' @param variable custom selection of percentiles. If variable is set to
#' FALSE, which is default value, it will generate quartiles, medians, and
#' tertiles
#' @param pctlevel It is used to find the selected percentile above or below a
#' certain threshold. Default is 50.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #pct <- esAddGenePct(eset, variable=FALSE, pctlevel=50)
#' #varLabels(pct)
#' 
#' 
esAddGenePct <-
function(es, variable=FALSE, pctlevel=50) {
  ############################################
  ## Function to generate median, tertile, and quartiles of all the features in the expressionset
  ## and adds it to the pheno data of the expressionset 
  ## limiting it to 20 genes for now
  ##  es - expressionset class
  #############################################
  
  if (length(featureNames(es)) > 500) 
  { sprintf("More than 500 Genes, try smaller gene number")
    return(es)
  }
  else 
  {  
    if(!variable) {   # Default value, just generate quartiles, medians, and tertiles
    df = as.data.frame(pData(es))
    ### compute ecdf of each gene
    pctiles = data.frame(sapply(as.data.frame(t(exprs(es))), function(x) ecdf(x)(x)))
    quarts = apply(pctiles, 2, function(x) ifelse(x<=0.25,'1st Quartile', ifelse(x>0.25 & x<=0.5, '2nd Quartile', 
                                                                    ifelse(x>0.5 & x<=0.75, '3rd Quartile', ifelse(x>0.75, '4th Quartile',x)))) )
    medians = apply(pctiles, 2, function(x) ifelse(x<=0.5,' <= Median', ifelse(x>0.5, ' >  Median',x)) )
    tertiles = apply(pctiles, 2, function(x) ifelse(x<=0.33, '1st Tertile', ifelse(x>0.33 & x<=0.66, '2nd Tertile', 
                                                                      ifelse(x>0.66, '3rd Tertile',x))) )
    colnames(quarts) = paste('quartiles_', colnames(pctiles), sep='')
    colnames(medians) = paste('median_', colnames(pctiles), sep='')
    colnames(tertiles) = paste('tertiles_', colnames(pctiles), sep='')
    df = cbind(df, medians, tertiles, quarts)
    phenoData <- new("AnnotatedDataFrame", data=df)
    es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
    }
    else {   # custom selection of percentiles
      df = as.data.frame(pData(es))
      ### compute ecdf of each gene
      pctiles = data.frame(sapply(as.data.frame(t(exprs(es))), function(x) ecdf(x)(x)))
      selectpctile = apply(pctiles, 2, function(x) ifelse(x<=pctlevel/100,1, ifelse(x>pctlevel/100, 2,x)) )

      colnames(selectpctile) = paste('pctiles_', colnames(pctiles), sep='')
      df = cbind(df, selectpctile )
      phenoData <- new("AnnotatedDataFrame", data=df)
      es <- new("ExpressionSet", exprs=exprs(es), phenoData=phenoData)
      
      
    }
      
  }
}
