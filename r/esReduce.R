#' esReduce
#' 
#' Create a common gene expressionset
#' 
#' @export
#' @param es expression set
#' @param annot is an annotation object (first column is the probe_id)
#' @author hahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #results <- esReduce(eset, anno_obj)
#' #results
#' 
#' 
esReduce <-
function(es, annot) {
  ## Create a common gene expressionset, converts my way of annotation use
  ## to work with function esReduce2 (which was downloaded from another person, originally called Reduce)
  tgenes = as.data.frame(cbind(tsym = esGetSymbol(annotatedlist=featureNames(esAnnotate(es,annot=annot))), tgenes_f = featureNames(es)))
  tgenes = tgenes[tgenes$tsym!="---",]
  
  ## Follwoing needed for function Reduce
  gene_list = as.list(as.character(unlist(tgenes$tgenes_f)))
  names(gene_list) = as.character(tgenes$tsym)
  eset = es[as.character(unlist(tgenes$tgenes_f)),]
  mode(exprs(eset)) <- "numeric"
  
  ## Create Expression sets where each of the above is
  ## reduced by the gene symbol (average expression per gene symbols)
  eset = esReduce2(es=eset, 
                   annovec=as.character(unique(tgenes$tsym)), 
                   ann2featMap=gene_list, pdvname='sym', mean)
  eset
}
