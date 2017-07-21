#' esReduce2
#' 
#' ExpressionSet with featureNames
#' 
#' 
#' @param es expression set
#' @param annovec annotation vector, a collection of annotations
#' @param ann2featMap is either an AtomicAnnDbBimap with keys in a and range in
#' f, or a list with element names in a and element values in f
#' @param pdvname gene symbol
#' @param collapseFun collapse function, a function that applys to
#' @author Shahab Asgharzadeh
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esReduce2(eset, annovec, ann2featMap, pdvname = "symbol", collapseFun = NULL)
#' 
esReduce2 <-
function( es, annovec, ann2featMap, pdvname="symbol", collapseFun=NULL ) {
  #
  # es is an ExpressionSet with featureNames f, say, and
  # annovec is a collection of annotations a.  ann2featMap
  # is either an AtomicAnnDbBimap with keys in a and range in f, or
  # a list with element names in a and element values in f
  #
  # a new ExpressionSet is computed with features restricted to those
  # mapped from annovec
  #
  
  #Modified by Shahab to use the annot format:  Begin:
  #ann2featMap = ann2featMap[ann2featMap[[2]] !="",]
  #ann2featMap = na.omit(ann2featMap)
  #gene_list = as.character(ann2featMap[[1]])
  #names(gene_list) = as.character(ann2featMap[[2]])
  #gene_list = gene_list[which(gene_list %in% featureNames(es))]
  #ann2featMap = as.list(gene_list)
  ## End
  
  n1 = na.omit(featNamesFromAnno( annovec, ann2featMap ))
  es2 = es[ n1, ]
  newdf = data.frame(names(n1))
  names(newdf) = pdvname
  rownames(newdf) = n1
  featureData(es2) = new("AnnotatedDataFrame", newdf)
  if (!is.null(collapseFun)) {
    splv = as.character(newdf[,1])
    s1 = aggFeatures( exprs(es2)[,1], splv, collapseFun )
    ans = apply( exprs(es2), 2, aggFeatures, splv, collapseFun )
    rownames(ans) = names(s1)
    newdf = data.frame(names(s1))
    names(newdf) = pdvname
    rownames(newdf) = names(s1)
    es2 = new("ExpressionSet", exprs=ans, phenoData=phenoData(es))
    featureData(es2) = new("AnnotatedDataFrame", newdf)
  }
  es2
}
