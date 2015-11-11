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
