#' esPathwayFeatures
#' 
#' Finds Top scoring Pathways (using Score Calculated by multiplying pathway
#' eucledian distance and SD of Chi2
#' 
#' 
#' @param esPathwaySig_results pathway analysis results from
#' \code{esPathwaySig}
#' @param pathname pathname of interest
#' @param anno is an annotation object (first column is the probe_id)
#' @param column_lookup column_lookup of interest
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
#' #esPathwayFeatures(esPathwaySig_results, pathname, anno = NULL, column_lookup = "gene_symbol")
#' 
esPathwayFeatures <-
function(esPathwaySig_results, pathname, anno=NULL, column_lookup='gene_symbol') {
  #### Finds Top scoring Pathways (using Score Calculated by multiplying pathway eucledian distance and SD of Chi2
  annot<-NULL #setting annot to NULL to pass R CMD check 
  
  pathway_data = esPathwaySig_results
  genes = pathway_data$genes[which(pathway_data$pathway==pathname)]
  if(!is.null(annot)) {
    anno_idx = unlist(lapply(genes, function(x) which(x == anno[[column_lookup]])))  
    genes = anno[anno_idx,][[1]]
  }
  genes
}
